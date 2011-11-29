
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

clean:
	rm -f output/* conf/* clean/* result ra rf slimmer

ra: ra.c
	gcc -Wall -g2 -lavl -lpthread -O2 -o ra ra.c

rf: rf.c
	gcc -Wall -g2 -lavl -O2 -o rf rf.c

slimmer: slimmer.c
	gcc -Wall -g2 -lavl -O2 -o slimmer slimmer.c 

clean/%.rule: output/%.out slimmer
	./slimmer $(limit) $< $@

output/%.out: conf/%.conf rf
	$(john) -w:$(dico) -sess:$* -rules:xxx --config:$< -stdout | ./rf - $(pass) "`cat rules/$*.rule`" > $@

conf/%.conf: rules/%.rule john.conf.skel
	cat john.conf.skel $< > $@ 

