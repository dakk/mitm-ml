(* 
	Pc:
		20bit -> 1M keys -> 15 minuti

	Server:
		20bit -> 1Mkeys -> 5 + nlogn
		28bit -> 268M keys -> 
			Ph1 = 268*5minuti circa -> 22 ore
			Ph2 = circa 15 ore
		Circa 268*7*2 = 4GB memoria non posso parallelizzare e mi serve swap

	Nel server fa 50KHash in 15 secondi -> 
		50 : 15 = x : 1
		x = 50/15= 3.3KH/s

	Il processo kswapd usa troppa ram. Nel server eseguiro' quella da 24.
		Avvio 9:44


	Chiavi da 20:
		1. 7B652692CFB963A7 - A373589BC4F1A5D3
		2. 06A28C53C3B1F141 - 28E2B778C5A2EEA1
		3. 20B080D0A6ED905E - 60F861BF24E89BC0

		k1 = 56cc0 k2 = f71ea

	Chiavi da 24:
		k1 = k2 =
	
	Chiavi da 28:
		k1 = k2 =
	
	Chiavi da 32:
		k1 = k2 =


	Scalando:
	Per quello a 32 bit lo posso finire in n giorni, trasformando il problema in modo
	meno memory heavy; effettuo il check del DEC su una parte del dominio della chiave
	ed itero per blocchi di chiavi.

	Posso definire una variabile contenente il numero di chiavi massimo processabile
	in un round, dividere il dominio, e fare l'attacco sul quel blocco.
	
	Devo fare un qualcosa che scriva su file la chiave risultante.
*)
open Unix;;

let pow x y =
	let rec pow' x' y' = match y' with
		| 1 -> x'
		| i -> pow' (x'*x) (y'-1)
	in pow' x y
;;

let bits = 28;;
let hexchar = bits / 4;;
let keynum = pow 2 bits;;
let dataX = "6AAF402B1F93400D";;
let dataY = "A4CFB906C800C3D9 ";;

(*  
	./mitm_tofile e > ~/e1.txt
	./mitm_tofile d > ~/d1.txt

	sort --parallel=4 -T ~/tmp -s -n -k 1,1 ~/e1.txt > ~/e2.txt 
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
			Printf.printf "%s %s\n" (f khex d) khex;
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

let () = op 2;;
