import sys


prev = ""
with open ('~/test2.txt', 'r') as f:
    for line in f:
	if line[0:16] == prev[0:16]:
		print line
		print prev
		sys.exit (0)
	else
		prev = line
