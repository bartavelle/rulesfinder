Rules finder
============

This program computes a good set of mutation rules for John the Ripper for cracking a givent password list with a given dictionary, using predefined rules as a template. It uses a greedy algorithm to compute the best append/prepend rules that could be used in conjunction with the predefined rules.

More details [here](http://tehlose.wordpress.com/category/mangling-rules-generation/).

Basic setup
-----------

You must edit the following parameters in the Makefile :

+   pass : a password file you want to train against, such as the rockyou list (it will require quite a bit of memory with such a large list). A word per line.
+   dico : a dictionnary that you will use to crack passwords (a wordlist you want to use when cracking). A word per line.
+   john : path to the John the Ripper binary
+   limit : the cutout limit for rules. This means that only the rules that crack that many passwords will be kept (the bigger the pass and dico files are, the bigger this value must be).
+   threads : number of threads used during the final computation stage

It is highly recommended you adjust the MAXMEM parameter in ra.c and slimmer.c to match the maximum memory you wish to use (defaults are for a 16GB machine, with 4 parallel threads : 14.5GiB for ra and 4GiB for slimmer). If this doesn't make any sense to you, just use a computer with 16GB of memory.

Base rules
----------
A sample is provided for your enjoyment in the rules subdirectory. Each file must contain a single John the Ripper mutation rule. In order to properly exploit this tool you will have to add better base rules.

