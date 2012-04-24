
cleans := $(patsubst rules/%,clean/%,$(wildcard rules/*))

pass = ~/tools/john/run/password.lst
dico = /usr/share/dict/american-english
john = ~/tools/john/run/john
limit = 10
threads = 6

.SECONDARY: 

.PHONY: all clean
result: $(cleans) ra
	./ra $(limit) $(threads) $(cleans) > result

result-small: $(cleans) ra-small
	./ra-small $(limit) $(threads) $(cleans) > result-small

badrules: $(cleans) list_useless_rules
	./list_useless_rules $(limit) clean | tee badrules

clean:
	rm -f output/* conf/* clean/* result ra rf slimmer

mtwist.o: mtwist.c mtwist.h
	gcc -Wall -g2 -O2 -c -o mtwist.o mtwist.c

ra: ra.c
	gcc -Wall -g2 -lavl -lpthread -O2 -o ra ra.c

list_useless_rules: list_useless_rules.c
	gcc -Wall -g2 -lavl -lpthread -O2 -o list_useless_rules list_useless_rules.c

ra-small: ra-small.c mtwist.o
	gcc -Wall -g2 -lavl -lpthread -O2 -o ra-small ra-small.c mtwist.o

rf: rf.c
	gcc -Wall -g2 -lavl -O2 -o rf rf.c

slimmer: slimmer.c
	gcc -Wall -g2 -lavl -O2 -o slimmer slimmer.c 

clean/%.rule: output/%.out slimmer
	zcat $< | ./slimmer $(limit) - $@

output/%.out: conf/%.conf rf
	$(john) -w:$(dico) -sess:$* -rules:xxx --config:$< -stdout | ./rf - $(pass) "`cat rules/$*.rule`" $(minmatch) | gzip -9 > $@

conf/%.conf: rules/%.rule john.conf.skel
	cat john.conf.skel $< > $@ 

