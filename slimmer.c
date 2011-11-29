#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <avl.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

#define LINELEN         200
#define MAXMEM          4000000000L
	
unsigned int nblines;
unsigned int nbcorrect;
unsigned int nbrules;

struct s_intlink
{
	unsigned int val;
	struct s_intlink * next;
};

struct s_rule
{
	char * rule;
	unsigned int count;
	struct s_intlink * root;
};

void rule_free(struct s_rule * rule)
{
	struct s_intlink * curlink;
	struct s_intlink * nextlink;

	curlink = rule->root;
	while(curlink)
	{
		nextlink = curlink->next;
		free(curlink);
		curlink = nextlink;
	}
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

struct s_rule * newrule(char * rule)
{
	struct s_rule * out;

	out = xmalloc(sizeof(struct s_rule));
	out->rule = strdup(rule);
	out->count = 0;
	if(out->rule == NULL)
	{
		perror("strdup");
		exit(50);
	}
	out->root = NULL;
	nbrules++;
	return out;
}

void add_item(struct s_rule * tmprule, unsigned int nodeid)
{
	struct s_intlink * curlink;
	
	curlink = xmalloc(sizeof(struct s_intlink));
	curlink->val = nodeid;
	curlink->next = tmprule->root;
	tmprule->root = curlink;
	tmprule->count++;
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


void usage(void)
{
	printf("usage: slimmer threshold file\n");
	exit(0);
}

void s_write(int fd, void * ptr, unsigned int len)
{
	if(write(fd,ptr,len)==len)
		return;
	perror("write");
	exit(55);
}

int main(int argc, char ** argv)
{
	unsigned int threshold;
	char line[LINELEN];
	char rulestr[LINELEN];
	FILE * input;
	unsigned int linepart[6];
	unsigned int len,i,j;
	avl_tree_t * ruleroot;
	unsigned int curid;
	struct s_rule staticrule;
	struct s_rule * tmprule;
	avl_node_t * snode;
	int outfd;
	mode_t mode = S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH;
	struct s_intlink * curlink;

	setlimits();
	nblines = 0;
	nbcorrect = 0;
	nbrules = 0;
	if(argc != 4)
		usage();

	threshold = atoi(argv[1]);
	if(threshold == 0)
		usage();

	input = fopen(argv[2], "r");
	if(input == NULL)
	{
		perror(argv[2]);
		return 1;
	}

	outfd = open(argv[3], O_WRONLY | O_CREAT | O_TRUNC, mode);
	if(outfd<0)
	{
		perror(argv[3]);
		return 2;
	}

	staticrule.root = NULL;
	staticrule.rule = rulestr;

	ruleroot = avl_alloc_tree((avl_compare_t)rule_compare, (avl_freeitem_t)rule_free);

	while(fgets(line, LINELEN-1, input))
	{
		nblines++;
		if( (nblines % 1000000) == 0 )
			fprintf(stderr, "%.1fM rules integrated, %.1fM lines read\n", ((float)nbrules)/1000000.0, ((float)nblines)/1000000.0);
		if(line[0]==0)
			continue;
		len=strlen(line);
		j=1;
		linepart[0] = 0;
		for(i=0;i<len;i++)
		{
			if(line[i]!='\t')
				continue;
			linepart[j]=i+1;
			line[i]=0;
			j++;
			if(j==6)
				break;
		}
		if(j!=6)
			continue;
		nbcorrect++;
		/*
		 * ici les parties sont
		 * 0 : rule
		 * 1 : password
		 * 2 : prefix
		 * 3 : suffix
		 * 4 : mot du dico
		 * 5 : numéro de ligne du fichier de passwords
		 */
		strcpy(rulestr, line+linepart[0]);
		if(strlen(line+linepart[2]))
		{
			strcat(rulestr, " A0\"");
			strcat(rulestr, line+linepart[2]);
			strcat(rulestr, "\"");
		}
		if(strlen(line+linepart[3]))
		{
			strcat(rulestr, " Al\"");
			strcat(rulestr, line+linepart[3]);
			strcat(rulestr, "\"");
		}
		snode = avl_search(ruleroot, &staticrule);
		if(snode == NULL)
		{
			tmprule =  newrule(rulestr);
			avl_insert(ruleroot, tmprule);
		}
		else
			tmprule = snode->item;
		curid = atoi(line+linepart[5]);
		add_item(tmprule, curid);
	}
	fprintf(stderr, "parsing finished, %d lines, %d correct lines, %d rules\n", nblines, nbcorrect, nbrules);
	fclose(input);

	snode = ruleroot->head;
	nbrules = 0;
	while(snode)
	{
		tmprule = snode->item;
		if(tmprule->count>=threshold)
		{
			nbrules++;
			len = strlen(tmprule->rule);
			s_write(outfd, &len, sizeof(unsigned int));
			s_write(outfd, tmprule->rule, len);
			s_write(outfd, &tmprule->count, sizeof(unsigned int));
			curlink = tmprule->root;
			while(curlink)
			{
				s_write(outfd, &curlink->val, sizeof(unsigned int));
				curlink = curlink->next;
			}
		}
		snode = snode->next;
	}
	close(outfd);
	fprintf(stderr, "%d rules after filtering\n", nbrules);
	return 0;
}
