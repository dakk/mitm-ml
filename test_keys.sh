# plain, key1, key2 

echo $1 > t.txt
../SPN/encrypt $2 t.txt > t2.txt
../SPN/encrypt $3 t2.txt
