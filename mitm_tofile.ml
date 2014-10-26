(* *)
open Unix;;

let pow x y =
	let rec pow' x' y' = match y' with
		| 1 -> x'
		| i -> pow' (x'*x) (y'-1)
	in pow' x y
;;

let bits = 32;;
let hexchar = bits / 4;;
let keynum = pow 2 bits;;
let dataX = "17CA0E8D36BA3E5A";;
let dataY = "9FEBE85F56228FC5";;

(*  
	./mitm_tofile e > ~/e1.txt
	./mitm_tofile d > ~/d1.txt


	cat ~/e1 ~/d1 > ~/ED.txt
 
	sort --parallel=4 -T ~/tmp -s -k 1,1 ~/ED.txt > ~/e2.txt 

	python find_match.py > res.txt 
*)

let int_to_hexs i = 
	let s = Printf.sprintf "%04x" i in
	let l = String.length s in
	if l < hexchar then (String.make (hexchar-l) '0') ^ s
	else s
;;


let middleMake d f start n =
	let rec middle k =
		if k <= n then
			let khex = (int_to_hexs k) in
			Printf.printf "%s %s\n%!" (f khex d) khex;
			middle (k+1)
		else ()
	in middle start
;;


(* Multi process attack *)
let middleMakeMulti d f nprocs =
	let step = keynum/nprocs in
	let rec deploy nxst i =
		if i = 0 then ()
		else
			match fork () with
				| 0 -> middleMake d f nxst (nxst+step)
				| pid -> deploy (nxst+step) (i-1)
	in deploy 0 nprocs
;;



let op n =
	if Sys.argv.(1) = "e" then
		middleMakeMulti dataX (Encrypt.encrypt) n
	else
		middleMakeMulti dataY (Decrypt.decrypt) n
;;

let () = op 4;;
