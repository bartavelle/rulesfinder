#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <malloc.h>
#include <unistd.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/types.h>
#include <dirent.h>

#define LINELEN         200
#define MAXMEM          14500000000L

unsigned int matchlimit;
unsigned int nbthreads;
unsigned int nbfiles;

struct s_rule
{
    char * rule;
};

struct s_rule_link
{
    char * rule;
    struct s_rule_link * next;
};

struct s_rulejob
{
    char * filename;
    struct s_rule_link * root;
    struct s_rule_link * tail;
    int done_by;
} * rulejob;

struct s_rulefile
{
    char * name;
    unsigned int nblines;
    unsigned int nbrules;
};

void usage(void)
{
    printf("usage: list_useless_rules cutout nbthreads clean_directory\n");
    exit(0);
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

unsigned long long load_rules(char * filename)
{
    int rulefd;
    char rulestr[LINELEN];
    unsigned int curval;
    unsigned int nbpwds;
    unsigned long testedpwds;
    unsigned long long total;
    char name[4096];

    strcpy(name, "clean/");
    strcat(name, filename);
    rulefd = open(name, O_RDONLY);
    if(rulefd <0)
    {
        perror(filename);
        return;
    }

    total = 0;
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
        if(read(rulefd, &testedpwds, sizeof(unsigned long)) != sizeof(unsigned long))
        {
            fprintf(stderr, "could not nb of tested passwords\n");
            break;
        }
        if(read(rulefd, &nbpwds, sizeof(unsigned int)) != sizeof(unsigned int))
        {
            fprintf(stderr, "could not read nbpwd\n");
            break;
        }
        if(lseek(rulefd, nbpwds*sizeof(unsigned int), SEEK_CUR) < 0)
        {
            perror(filename);
            break;
        }
        total += nbpwds;
    }

    close(rulefd);

    return total;
}

int main(int argc, char ** argv)
{
    DIR * cleandir;
    struct dirent * curfile;
    unsigned long long curval;

    setlimits();
    if(argc!=3)
        usage();
    matchlimit = atoi(argv[1]);
    if(matchlimit == 0)
        usage();

    cleandir = opendir(argv[2]);
    if(cleandir == NULL)
    {
        perror(argv[2]);
        return 3;
    }

    while(curfile = readdir(cleandir))
    {
        if(curfile->d_name[0] == '.')
            continue;
        curval = load_rules(curfile->d_name);
        if(curval < matchlimit)
            printf("%s %lld\n", curfile->d_name, curval);
    }

    return 0;
}
