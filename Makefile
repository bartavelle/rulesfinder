
cleans := $(patsubst rules/%,clean/%,$(wildcard rules/*))

pass = ~/tools/john/run/password.lst
dico = /usr/share/dict/american-english
john = ~/tools/john/run/john
limitfile = limit
threads = 6
minmatch = 4

# parameters used for tuning your rule usages
# the first one is used to set which format should be used
format  = des
# the second one to decide the number of salts to be cracked
nbsalts = 50

.SECONDARY: 

.PHONY: all clean
result: $(cleans) ra $(limitfile) processWordlist processtime
	./ra `cat $(limitfile)` $(threads) $(cleans) `cat processtime` `cat processWordlist` > result

processWordlist: $(dico) $(john)
	/usr/bin/time --format="%e" $(john) -sess:benchProcessWordlist -w:$(dico) -stdout 2> /tmp/processWordlist > /dev/null && grep -v '^words' /tmp/processWordlist > processWordlist

processtime: $(john) computeprocesstime.pl
	$(john) -test:10 -format:$(format) | perl computeprocesstime.pl $(nbsalts) > processtime

removeknown: removeknown.c cityhash.c
	gcc -Wall -g2 -O2 -o removeknown removeknown.c

cleanpass: $(pass) $(dico) removeknown
	./removeknown $(dico) < $(pass) > cleanpass

badrules: $(cleans) list_useless_rules $(limitfile)
	./list_useless_rules `cat $(limitfile)` clean | tee badrules

clean:
	rm -f output/* conf/* clean/* result ra rf slimmer cleanpass removeknown processWordlist processtime

ra: ra.c
	gcc -Wall -g2 -O2 -o ra ra.c -lavl -pthread

list_useless_rules: list_useless_rules.c
	gcc -Wall -g2 -O2 -o list_useless_rules list_useless_rules.c -lavl -lpthread

rf: rf.c cityhash.c
	gcc -Wall -g2 -O2 -o rf rf.c -lavl

slimmer: slimmer.c
	gcc -Wall -g2 -O2 -o slimmer slimmer.c -lavl

clean/%.rule: output/%.out slimmer $(limitfile)
	zcat $< | ./slimmer `cat $(limitfile)` - $@

output/%.out: conf/%.conf rf $(dico) cleanpass
	$(john) -w:$(dico) -sess:$* -rules:xxx --config:$< -stdout | ./rf - cleanpass "`cat rules/$*.rule`" $(minmatch) | gzip -1 > $@

conf/%.conf: rules/%.rule john.conf.skel
	cat john.conf.skel $< > $@

