#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <avl.h>
#include <stdint.h>

#define LINELEN		200
#define MAXLEN		16

// bloom filter params
#define BLOOM_WIDTH	30ULL
#define BLOOM_TYPE	unsigned long long
#define BLOOM_SIZE	((BLOOM_TYPE) 1<<BLOOM_WIDTH)
#define BLOOM_STORAGE	((BLOOM_SIZE / 8)+1)
#define BLOOM_MASK	(BLOOM_SIZE-1)

/* partie cityhash */
static const uint64_t k0 = 0xc3a5c85c97cb3127ULL;
static const uint64_t k1 = 0xb492b66fbe98f273ULL;
static const uint64_t k2 = 0x9ae16a3b2f90404fULL;
static const uint64_t k3 = 0xc949d7c7509e6557ULL;

#define UNALIGNED_LOAD64(p) (*(const uint64_t*)(p))
#define UNALIGNED_LOAD32(p) (*(const uint32_t*)(p))

inline uint64_t HashLen16(const uint64_t x1, const uint64_t x2) {
  // Murmur-inspired hashing.
  const uint64_t kMul = 0x9ddfea08eb382d69ULL;
  uint64_t a = (x1 ^ x2) * kMul;
  a ^= (a >> 47);
  uint64_t b = (x2 ^ a) * kMul;
  b ^= (b >> 47);
  b *= kMul;
  return b;
}

static uint64_t RotateByAtLeast1(uint64_t val, int shift) {
  return (val >> shift) | (val << (64 - shift));
}

static uint64_t ShiftMix(uint64_t val) {
  return val ^ (val >> 47);
}

static uint64_t HashLen0to16(const char *s, size_t len) {
	if (len > 8) {
		uint64_t a = UNALIGNED_LOAD64(s);
		uint64_t b = UNALIGNED_LOAD64(s + len - 8);
		return HashLen16(a, RotateByAtLeast1(b + len, len)) ^ b;
	}
	if (len >= 4) {
		uint64_t a = UNALIGNED_LOAD32(s);
		return HashLen16(len + (a << 3), UNALIGNED_LOAD32(s + len - 4));
	}
	if (len > 0) {
		uint8_t a = s[0];
		uint8_t b = s[len >> 1];
		uint8_t c = s[len - 1];
		uint32_t y = ((uint32_t)a) + ( ((uint32_t)b) << 8);
		uint32_t z = len + ( ((uint32_t) c) << 2);
		return ShiftMix(y * k2 ^ z * k3) * k2;
	}
	return k2;
}

static unsigned long cityhash(const char *s)
{
	uint64_t out;

	out = HashLen0to16(s,strlen(s));
	return out & BLOOM_MASK;
}

inline void SETBIT(BLOOM_TYPE * ptr, unsigned long hash)
{
	unsigned long index;
	unsigned long dec;

	hash &= BLOOM_MASK;
	index = hash / (sizeof(BLOOM_TYPE)*8);
	dec = hash & (sizeof(BLOOM_TYPE)*8-1);
	ptr[index] |= 1 << dec;
}


inline unsigned int GETBIT(BLOOM_TYPE * ptr, unsigned long hash)
{
	unsigned long index;
	unsigned long dec;

	hash &= BLOOM_MASK;
	index = hash / (sizeof(BLOOM_TYPE)*8);
	dec = hash & (sizeof(BLOOM_TYPE)*8-1);
	return ptr[index]&(1<<dec);
}

// 4G MAXMEM
#define MAXMEM		4294967296

//djb
/*
#define HASH_INIT	5381
#define HASH_STEP(hash,nchar) hash = ( (hash<<5)+hash+(nchar) )
*/
//djb2
/*
#define HASH_INIT	5381
#define HASH_STEP(hash,nchar) hash = ( (hash*33)^(nchar) )
*/
//fowler no
/*
#define HASH_INIT 14695981039346656037ULL
#define HASH_STEP(hash,nchar) hash = (hash*1099511628211ULL)^((unsigned char) (nchar))
*/
//sdbm
#define HASH_INIT 0
#define HASH_STEP(hash,nchar) hash = nchar + (hash << 6) + (hash << 16) - hash;

unsigned int nblines;
unsigned int nbpass;
unsigned int nbfp;
unsigned int nbmatch;

void usage(void)
{
	printf("usage: rulesfinder dictionnary password_file ruleinfo\n");
	exit(1);
}

unsigned long hash(char * str)
{
	unsigned long hash = HASH_INIT;
	unsigned int i;

	for(i = 0; str[i] && (i<MAXLEN); i++)
	{
		HASH_STEP(hash,str[i]);
	}
	return hash;
}

void * xmalloc(unsigned int size)
{
	void * cur;

	cur = malloc(size);
	if(cur)
		return cur;
	perror("malloc");
	fprintf(stderr, "@ nblines=%d\n", nblines);
	exit(1);
}

int hash_compare(long * a, long *b)
{
	return *a-*b;
}

int main(int argc, char ** argv)
{
	FILE * dictionnary;
	FILE * pwds;
	char line[LINELEN];
	unsigned int i,j;
	avl_tree_t * sroot;
	struct rlimit rlim;
	BLOOM_TYPE * bloom;
	BLOOM_TYPE * bloom_cityhash;

	unsigned long curhashes[MAXLEN];
	unsigned int curstart[MAXLEN];
	unsigned int maxlen;
	unsigned int maxstart;
	unsigned char ln2[LINELEN];
	unsigned int len;

	rlim.rlim_cur = MAXMEM;
	rlim.rlim_max = MAXMEM;
	if(setrlimit(RLIMIT_AS, &rlim))
	{
		perror("setrlimit");
		return 3;
	}

	if(argc != 4)
		usage();
	nblines = 0;

	/* arbre contenant le dictionnaire pour éviter les FP */
	sroot = avl_alloc_tree((avl_compare_t)strcmp, (avl_freeitem_t)free);

	/* bloom table */
	bloom = xmalloc(BLOOM_STORAGE);
	memset(bloom, 0, BLOOM_STORAGE);
	bloom_cityhash = xmalloc(BLOOM_STORAGE);
	memset(bloom_cityhash, 0, BLOOM_STORAGE);
	
	fprintf(stderr, "%lld bytes have been allocated for bloom filters\n", BLOOM_STORAGE*2);

	if(strcmp(argv[1],"-")==0)
		dictionnary = stdin;
	else
		dictionnary = fopen(argv[1], "r");
	if(dictionnary == NULL)
	{
		perror(argv[1]);
		return 2;
	}
	pwds = fopen(argv[2], "r");
	if(pwds == NULL)
	{
		perror(argv[2]);
		return 3;
	}

	while(fgets(line, LINELEN-1, dictionnary))
	{
		nblines++;
		if(nblines % 1000000 == 0)
			fprintf(stderr, "%dM dictionnary lines integrated\n", nblines/1000000);
		len = strlen(line);
		if(len>MAXLEN)
			continue;
		line[len-1]=0; // trim !
		SETBIT(bloom, hash(line));
		SETBIT(bloom_cityhash, cityhash(line));
		avl_insert(sroot, strdup(line));
	}
	fprintf(stderr, "%d lines\n", nblines);
	fclose(dictionnary);

	nbpass = 0;
	nbfp = 0;
	nbmatch = 0;
	while(fgets((char*) line, LINELEN-1, pwds))
	{
		nbpass++;
		if(nbpass % 500000 == 0)
			fprintf(stderr, "%.1fM passwords analyzed\n", ((float)nbpass)/1000000.0);
		maxlen = 0;
		maxstart = 0;
		memset(curstart, 0, sizeof(curstart));
		for(i=0;i<MAXLEN;i++)
			curhashes[i]=HASH_INIT;
		for(i=0;line[i] && line[i]!='\n' && (i<MAXLEN*2);i++)
		{
			curhashes[i%MAXLEN]=HASH_INIT;
			curstart[i%MAXLEN] = i;
			for(j=0;j<MAXLEN;j++)
			{
				HASH_STEP(curhashes[j],line[i]);
			}
			for(j=0;j<MAXLEN;j++)
			{
				/* si on ne bat pas le maxlen courant ça ne sert à rien */
				if( i-curstart[j] <= maxlen )
					continue;
				/* check bloom filter */
				if(GETBIT(bloom, curhashes[j]))
				{
					memset(ln2, 0, sizeof(ln2));
					memcpy(ln2, line+curstart[j], i-curstart[j]+1);
					if(GETBIT(bloom_cityhash, cityhash((char*)ln2)))
					{
						if(avl_search(sroot, ln2)) /* evitons les FP */
						{
							maxlen = i-curstart[j];
							maxstart = curstart[j];
						}
						else
						{
							nbfp++;
						}
					}
				}
			}
		}
		if(maxlen==0)
			continue;
		line[strlen((char*)line)-1] = 0;
		printf("%s\t%s\t", argv[3], line);
		if(maxstart>0)
		{
			memset(ln2, 0, sizeof(ln2));
			strncpy((char*)ln2, (char*)line, maxstart);
			printf("%s\t", ln2);
		}
		else
			printf("\t");
		if(maxstart+maxlen+1<strlen((char*)line))
			printf("%s\t", line + maxstart + maxlen + 1);
		else
			printf("\t");
		line[maxstart+maxlen+1] = 0;
		printf("%s\t%d\n", line+maxstart, nbpass);
		nbmatch++;
	}

	fprintf(stderr, "%d passwords analyzed, %d matches, %d false positives\n", nbpass, nbmatch, nbfp);

	return 0;
}
