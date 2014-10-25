import sys


prev = ""
with open ('~/e2.txt', 'r') as f:
    for line in f:
	if line[0:15] == prev[0:15]:
		print line
		print prev
		sys.exit (0)
	else
		prev = line
