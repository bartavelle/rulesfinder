#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <stdint.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#define HASHBITS 28
#define NBINS (1<<HASHBITS)
#define BITMASK (NBINS-1)

#include "cityhash.c"

struct s_wordlink
{
	uint32_t hash;
	uint32_t length;
	char * linestart;
	struct s_wordlink * next;
};

unsigned int getbin(struct s_wordlink * wl)
{
	return (wl->hash & BITMASK);
}

void addhash(struct s_wordlink ** map, struct s_wordlink * wl)
{
	unsigned long bin = getbin(wl);
	wl->next = map[bin];
	map[bin] = wl;
}

int readwords(char * wordmap, unsigned long long size, struct s_wordlink ** hashmap)
{
	unsigned long long we = 0;
	unsigned long long ws = 0;
	struct s_wordlink * wl;
	uint32_t hlength;

	while(we < size)
	{
		ws = we;
		while( wordmap[we] != '\n' && wordmap[we] != '\r' )
		{
			we++;
			// this means we reached the end of the file without finding a carriage return ..
			if(we>=size)
				return 0;
		}
		wl = malloc(sizeof(struct s_wordlink));
		if(wl == NULL)
		{
			perror("malloc");
			return -1;
		}
		wl->next = NULL;
		wl->linestart = wordmap + ws;
		wl->length = we - ws;
		if(wl->length > 16)
			hlength = 16;
		else
			hlength = wl->length;
		wl->hash = HashLen0to16(wl->linestart, hlength);

		addhash(hashmap, wl);

		while( wordmap[we] == '\n' || wordmap[we] == '\r' )
		{
			we++;
			if(we>=size)
				return 0;
		}
	}
	return 0;
}

int check(char * word, struct s_wordlink ** hashmap)
{
	char * ptr = word;
	while( (*ptr != '\n') && (*ptr != '\r') && (*ptr != 0) )
		ptr++;
	*ptr = 0;
	unsigned int wl = ptr - word;
	unsigned int hl = wl;
	if(hl>16)
		hl = 16;
	uint32_t hash = HashLen0to16(word, hl);
	struct s_wordlink * lk = hashmap[hash & BITMASK];

	while(lk != NULL)
	{
		if( (lk->hash == hash) && (lk->length == wl) )
		{
			if(memcmp(lk->linestart, word, wl) == 0)
				return 1;
		}
		lk = lk->next;
	}

	return 0;
}

int main(int argc, char ** argv)
{
	int r;

	if(argc != 2)
	{
		printf("Usage: %s dictionnary < wordlist\n", argv[0]);
		return 1;
	}

	struct s_wordlink ** hashmap = calloc(sizeof(struct s_wordlink *), NBINS);
	if(hashmap == 0)
	{
		perror("malloc");
		return 5;
	}

	int dicofd = open(argv[1], O_RDONLY);
	if(dicofd < 0)
	{
		perror(argv[1]);
		return 2;
	}

	struct stat dstats;
	if(fstat(dicofd, &dstats) != 0)
	{
		perror("fstats");
		return 3;
	}

	char * wordmap = mmap(NULL, dstats.st_size, PROT_READ, MAP_PRIVATE, dicofd, 0);
	if(wordmap == NULL)
	{
		perror("mmap");
		return 4;
	}

	if(r = readwords(wordmap, dstats.st_size, hashmap))
	{
		fprintf(stderr, "readwords failed with code %d", r);
		return r;
	}

	char line[256];
	while(fgets(line, 255, stdin))
	{
		if(!check(line, hashmap))
			printf("%s\n", line);
	}

	free(hashmap);
	munmap(wordmap, dstats.st_size);
	close(dicofd);
	return 0;
}
