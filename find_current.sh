cat e1.txt d1.txt > test.txt
sort --parallel=2 -s -k 1,1 -T tmp test.txt > test2.txt
python find_match.py

