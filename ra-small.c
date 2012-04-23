#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <avl.h>
#include <malloc.h>
#include <pthread.h>
#include <unistd.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <time.h>
#include "mtwist.h"

#define LINELEN         200
#define MAXMEM          14500000000L
#define MUTRATE         20
#define INMUTRATE       25
#define NBRULES         64
#define BASEPOPSIZE     32

unsigned int matchlimit;
unsigned int nbthreads;
unsigned int nbfiles;
unsigned int nbytes;
unsigned int POPSIZE;
avl_tree_t * dictroot;
unsigned long long totalcoverage;

void SETBIT(unsigned char * table, unsigned int index)
{
    table[index / 8] |= 1 << (index & 7);
}

struct s_rule
{
	char * rule;
	avl_tree_t * coverage;
};

struct s_brule
{
    char * rule;
    char * bitcover;
    unsigned int count;
} * brules;

struct s_individual
{
    struct s_brule * b[NBRULES];
    unsigned int fitness;
};

struct s_individual * pop;
struct s_individual best;

struct s_rule_link
{
	char * rule;
	avl_tree_t * coverage;
	struct s_rule_link * next;
};

struct s_rulejob
{
	char * filename;
	struct s_rule_link * root;
	struct s_rule_link * tail;
    unsigned int maxpwd;
	int done_by;
} * rulejob;

pthread_mutex_t rj_mutex = PTHREAD_MUTEX_INITIALIZER;

unsigned int weigth[256];

struct s_rulefile
{
	char * name;
	unsigned int nblines;
	unsigned int nbrules;
	avl_tree_t * ruleroot;
};

struct s_dictentry
{
	char * word;
	unsigned long hash;
	unsigned int index;
};

void initweigth()
{
    unsigned int i;
    unsigned int n;

    for(i=0;i<256;i++)
    {
        n = i;
        weigth[i] = 0;
        while(n)
        {
            weigth[i] += n&1;
            n >>= 1;
        }
    }
}

unsigned int count(unsigned char * x)
{
    unsigned int i;
    unsigned int out;

    out = 0;
    for(i=0;i<nbytes;i++)
        out += weigth[(unsigned int) x[i]];
    return out;
}

void usage(void)
{
	printf("usage: rulenalyzer cutout nbthreads [rule files ...]\n");
	exit(0);
}

void rule_free(struct s_rule * rule)
{
	if(rule->coverage)
		avl_free_tree(rule->coverage);
	if(rule->rule)
		free(rule->rule);
	free(rule);
}

int rule_compare(struct s_rule * a, struct s_rule * b)
{
	return strcmp(a->rule,b->rule);
}

void * xmalloc(unsigned int size)
{
	void * out;
	out = malloc(size);
	if(out)
		return out;
	perror("malloc");
	exit(5);
}

int int_cmp(unsigned int * a, unsigned int * b)
{
	if(*a>*b)
		return 1;
	if(*b>*a)
		return -1;
	return 0;
}

int ptr_cmp(unsigned int * a, unsigned int * b)
{
	if(a>b)
		return 1;
	if(b>a)
		return -1;
	return 0;
}

struct s_rule_link * newlink(char * rule)
{
	struct s_rule_link * out;

	out = xmalloc(sizeof(struct s_rule_link));
	out->rule = strdup(rule);
	if(out->rule == NULL)
	{
		perror("strdup");
		exit(50);
	}
	out->coverage = avl_alloc_tree((avl_compare_t)ptr_cmp, NULL);
	return out;
}

void setlimits(void)
{
    struct rlimit rlim;

    rlim.rlim_cur = MAXMEM;
    rlim.rlim_max = MAXMEM;
    if(setrlimit(RLIMIT_AS, &rlim))
    {
        perror("setrlimit");
        exit(3);
    }
}

int dict_compare(struct s_dictentry * a, struct s_dictentry * b)
{
	if(a->hash>b->hash)
		return 1;
	if(a->hash<b->hash)
		return -1;
	return strcmp(a->word, b->word);
}

void dict_free(struct s_dictentry * a)
{
	free(a->word);
	free(a);
}

void * load_rules(void * vtid)
{
    unsigned int * tid = vtid;
	struct s_rule_link * root;
	int rulefd;
	char rulestr[LINELEN];
	unsigned int curval;
    void * insertval;
	unsigned int nbpwds;
	//unsigned int * pcurval;
	unsigned int i;
	struct s_rule_link * link;

	unsigned int jobid;

	while(1)
	{
		root = NULL;
		/* select jobid */
		pthread_mutex_lock( &rj_mutex );
		for(jobid=0;jobid<nbfiles;jobid++)
			if( rulejob[jobid].done_by == -1 )
				break;
		if(jobid>=nbfiles)
		{
			pthread_mutex_unlock( &rj_mutex );
			break;
		}
		rulejob[jobid].done_by = *tid;
		pthread_mutex_unlock( &rj_mutex );

        rulejob[jobid].maxpwd = 0;
		rulefd = open(rulejob[jobid].filename, O_RDONLY);
		if(rulefd <0)
		{
			perror(rulejob[jobid].filename);
			continue;
		}

		while(1)
		{
			if(read(rulefd, &curval, sizeof(unsigned int)) != sizeof(unsigned int))
				break;
			if(curval>=(LINELEN-1))
			{
				fprintf(stderr, "rule length too large : %d>=%d\n", curval, LINELEN-1);
				break;
			}
			if(read(rulefd, rulestr, curval) != curval)
			{
				fprintf(stderr, "could not read rule string of len %d", curval);
				break;
			}
			rulestr[curval]=0;
			if(read(rulefd, &nbpwds, sizeof(unsigned int)) != sizeof(unsigned int))
			{
				fprintf(stderr, "could not read nbpwd\n");
				break;
			}
			link = newlink(rulestr);
			for(i=0;i<nbpwds;i++)
			{
				if( read(rulefd, &curval, sizeof(unsigned int)) != sizeof(unsigned int) )
				{
					fprintf(stderr, "could not read password %d/%d\n", i, nbpwds);
					break;
				}
                if(rulejob[jobid].maxpwd < curval)
                    rulejob[jobid].maxpwd = curval;
                insertval = (void *) (unsigned long) curval;
				avl_insert(link->coverage, insertval);
			}
			link->next = rulejob[jobid].root;
			if(rulejob[jobid].tail == NULL)
                rulejob[jobid].tail = link;
			rulejob[jobid].root = link;
		}

		close(rulefd);


	}
	fprintf(stderr, "[%d] EXITS\n", *tid);
	return NULL;
}

unsigned int calcfitness(struct s_individual * ind)
{
    unsigned int i,j,out;
    unsigned char * tmp;
    struct s_brule * currule;

    out = 0;
    tmp = xmalloc(nbytes);
    memset(tmp, 0, nbytes);
    for(i=0;i<NBRULES;i++)
    {
        currule = ind->b[i];
        for(j=0;j<nbytes;j++)
            tmp[j] |= currule->bitcover[j];
    }
    out = count(tmp);
    free(tmp);
    return out;
}

struct s_brule * pickrule()
{
    unsigned long long curval;
    unsigned long long target;
    unsigned int i;

    curval = 0;
    target = mt_llrand() % totalcoverage;
    for(i=0;i<nbfiles;i++)
    {
        curval += brules[i].count;
        if(curval >= target)
            return &brules[i];
    }
    fprintf(stderr, "wtf %s %lld/%lld ?\n", __func__, curval, target);
    return &brules[mt_llrand() % nbfiles];
}

void indiv_init(struct s_individual * ind)
{
    unsigned i;
    struct s_brule * currule;
    unsigned int try;

    for(i=0;i<NBRULES;i++)
    {
        try = 0;
tryagain:
        try++;
        if(try > 10000)
            fprintf(stderr, "%s stall, i=%d\n", __func__, i);
        currule = pickrule();
        if(ind->b[i] == currule)
            goto tryagain;
        ind->b[i] = currule;
    }
    ind->fitness = calcfitness(ind);
}

void showbest()
{
    unsigned int i;
    time_t t;
    struct tm *tmp;
    char stime[128];

    t = time(NULL);
    tmp = localtime(&t);
    strftime(stime, sizeof(stime)-1, "%c", tmp);
    fprintf(stderr, "%s %8d\t", stime, best.fitness);
    for(i=0;i<NBRULES;i++)
        fprintf(stderr, "'%s' ", best.b[i]->rule);
    fprintf(stderr, "\n");
}

unsigned int updatetotal()
{
    unsigned int i;
    unsigned int total;
    unsigned int sb;

    total = 0;
    sb = 0;
    for(i=0;i<POPSIZE;i++)
    {
        total += pop[i].fitness;
        if(pop[i].fitness > best.fitness)
        {
            memcpy(&best, &pop[i], sizeof(struct s_individual));
            sb = 1;
        }
    }
    if(sb)
        showbest();
    return total;
}

struct s_individual * pickparent(unsigned long long total)
{
    unsigned int curval;
    unsigned int target;
    unsigned int i;

    curval = 0;
    target = mt_llrand() % (total + best.fitness);

    for(i=0;i<POPSIZE;i++)
    {
        curval += pop[i].fitness;
        if(curval >= target)
            break;
    }
    if(i >= POPSIZE)
        return &best;
    return &pop[i];
}

void descend(struct s_brule ** dst, struct s_brule ** src)
{
    unsigned int i,j,try;
    struct s_brule * currule;

    try = 0;
    for(i=0;i<NBRULES;i++)
    {
        try++;
        if(try>10000)
            fprintf(stderr, "%s stall, i=%d\n", __func__, i);
anothergene:
        if( (mt_lrand() & 0xff) < INMUTRATE )
            currule = pickrule();
        else
            currule = src[ (mt_lrand() % (NBRULES*2)) ];
        for(j=0;j<i;j++) if(dst[j]==currule)
            goto anothergene;
        dst[i] = currule;
    }
}

struct s_reproduction_args
{
    unsigned long long total;
    unsigned int index;
};

void * reproduction(void * x)
{
    unsigned int i;
    struct s_individual npop[BASEPOPSIZE];
    struct s_brule * parentrules[NBRULES*2];
    struct s_individual * curparent;
    struct s_reproduction_args * args = x;
    unsigned long long total = args->total;
    unsigned int index = args->index;

    for(i=0;i<BASEPOPSIZE;i++)
    {
        if( (mt_lrand()&0xff) < MUTRATE )
        {
            indiv_init(&npop[i]);
            continue;
        }
        curparent = pickparent(total);
        memcpy(&parentrules[0], curparent->b, sizeof(struct s_brule *)*NBRULES);
        curparent = pickparent(total);
        memcpy(&parentrules[NBRULES], curparent->b, sizeof(struct s_brule *)*NBRULES);

        descend(npop[i].b, parentrules);
        npop[i].fitness = calcfitness(&npop[i]);
    }
    memcpy(&pop[index*BASEPOPSIZE], npop, sizeof(npop));
    return NULL;
}

int main(int argc, char ** argv)
{
	unsigned int i,j;
	pthread_t * threads;
	unsigned int * ids;
    unsigned int ccount;
	unsigned int maxval;
	struct s_rule_link * root;
	struct s_rule_link * curlink;
    struct s_reproduction_args * rargs;
    unsigned int value;
    unsigned int gen;
    unsigned int total;
    avl_node_t * curnode;

    mt_seed();
	setlimits();
	if(argc<4)
		usage();
	matchlimit = atoi(argv[1]);
	if(matchlimit == 0)
		usage();
	nbthreads = atoi(argv[2]);
	if(nbthreads == 0)
		nbthreads = 1;
	
    POPSIZE = nbthreads * BASEPOPSIZE;
    pop = xmalloc(POPSIZE*sizeof(struct s_individual));
	nbfiles = argc-3;
	threads = xmalloc(sizeof(pthread_t)*nbthreads);
	memset(threads, 0, sizeof(pthread_t)*nbthreads);
	ids = xmalloc(sizeof(unsigned int)*nbthreads);
    rargs = xmalloc(sizeof(struct s_reproduction_args)*nbthreads);
	rulejob = xmalloc(sizeof(struct s_rulejob)*nbfiles);
	for(i=0;i<nbfiles;i++)
	{
		rulejob[i].root = NULL;
		rulejob[i].tail = NULL;
		rulejob[i].done_by = -1;
		rulejob[i].filename = argv[i+3];
	}

	for(i=0;i<nbthreads;i++)
	{
		ids[i] = i;
		if(pthread_create(&threads[i], NULL, load_rules, &ids[i]))
		{
			fprintf(stderr, "error, could not create thread for file %s\n", argv[i+4]);
			perror("pthread_create");
			return 1;
		}
	}

	for(i=0;i<nbthreads;i++)
		pthread_join( threads[i], NULL );

	root = NULL;
    maxval = 0;
	for(i=0;i<nbfiles;i++)
	{
		if(rulejob[i].tail == NULL)
			continue;
		rulejob[i].tail->next = root;
        if(rulejob[i].maxpwd > maxval)
            maxval = rulejob[i].maxpwd;
		root = rulejob[i].root;
	}
	free(rulejob);

    if( (maxval & 7) == 0 )
        nbytes = maxval / 8;
    else
        nbytes = 1 + (maxval / 8);
	fprintf(stderr, "check if the %d passwords (%d*%d bytes) can be converted to bitfields\n", maxval, nbytes, nbfiles);

    brules = xmalloc(sizeof(struct s_brule) * nbfiles);
    
    initweigth();
    curlink = root;
    i = 0;
    totalcoverage = 0;
    while(curlink)
    {
        brules[i].bitcover = xmalloc( nbytes );
        brules[i].rule = curlink->rule;
        memset(brules[i].bitcover, 0, nbytes);

        j = 0;
        while( (curnode = avl_at(curlink->coverage, j)) )
        {
            value = (unsigned int) (unsigned long long) curnode->item;
            if(value > maxval)
            {
                fprintf(stderr, "WTF value=%d > %d\n", value, maxval);
                exit(3);
            }
            SETBIT((unsigned char *)brules[i].bitcover, value);
            j++;
        }
        ccount = count((unsigned char *) brules[i].bitcover);
        if( ccount != avl_count(curlink->coverage))
        {
            fprintf(stderr, "%d != %d [%d]\n", ccount, avl_count(curlink->coverage), j);
            exit(4);
        }
        fprintf(stderr, "ccount %d = %d\n", i, ccount);
        brules[i].count = ccount;
        totalcoverage += ccount;
        i++;
        avl_free_tree(curlink->coverage);
        curlink->coverage = NULL;
        curlink = curlink->next;
    }
    nbfiles = i;
	fprintf(stderr, "start crunching (%d passwords, %d rules) totalcoverage=%lld\n", maxval, nbfiles, totalcoverage);

    best.fitness = 0;
    for(i=0;i<POPSIZE;i++)
        indiv_init(&pop[i]);

    gen = 0;
    while(1)
    {
        total = updatetotal();
        gen++;
        for(i=0;i<nbthreads;i++)
        {
            rargs[i].total = total;
            rargs[i].index = i;
            if(pthread_create(&threads[i], NULL, reproduction, &rargs[i]))
            {
                fprintf(stderr, "error, could not create thread for computation %d\n", i);
                perror("pthread_create");
                return 1;
            }
        }
        for(i=0;i<nbthreads;i++)
            pthread_join( threads[i], NULL );
    }

	return 0;
}
