
cleans := $(patsubst rules/%,clean/%,$(wildcard rules/*))

pass = ~/tools/john/run/password.lst
dico = /usr/share/dict/american-english
john = ~/tools/john/run/john
limitfile = limit
threads = 6
minmatch = 4

.SECONDARY: 

.PHONY: all clean
result: $(cleans) ra $(limitfile)
	./ra `cat $(limitfile)` $(threads) $(cleans) > result

result-small: $(cleans) ra-small $(limitfile)
	./ra-small `cat $(limitfile)` $(threads) $(cleans) > result-small

badrules: $(cleans) list_useless_rules $(limitfile)
	./list_useless_rules `cat $(limitfile)` clean | tee badrules

clean:
	rm -f output/* conf/* clean/* result ra rf slimmer

mtwist.o: mtwist.c mtwist.h
	gcc -Wall -g2 -O2 -c -o mtwist.o mtwist.c

ra: ra.c
	gcc -Wall -g2 -O2 -o ra ra.c -lavl -pthread

list_useless_rules: list_useless_rules.c
	gcc -Wall -g2 -O2 -o list_useless_rules list_useless_rules.c -lavl -lpthread 

ra-small: ra-small.c mtwist.o
	gcc -Wall -g2 -O2 -o ra-small ra-small.c mtwist.o -lavl -lpthread 

rf: rf.c
	gcc -Wall -g2 -O2 -o rf rf.c -lavl

slimmer: slimmer.c
	gcc -Wall -g2 -O2 -o slimmer slimmer.c -lavl

clean/%.rule: output/%.out slimmer $(limitfile)
	zcat $< | ./slimmer `cat $(limitfile)` - $@

output/%.out: conf/%.conf rf $(dico) $(pass)
	$(john) -w:$(dico) -sess:$* -rules:xxx --config:$< -stdout | ./rf - $(pass) "`cat rules/$*.rule`" $(minmatch) | gzip -1 > $@

conf/%.conf: rules/%.rule john.conf.skel
	cat john.conf.skel $< > $@ 

