(* 
	Chiavi da 20:
		1. 7B652692CFB963A7 - A373589BC4F1A5D3
		2. 06A28C53C3B1F141 - 28E2B778C5A2EEA1
		3. 20B080D0A6ED905E - 60F861BF24E89BC0

		k1 = 56cc0 k2 = f71ea

	Chiavi da 24:
		k1 = 0cabde && k2 = b7bb1e


		Numero seriale: 41
		Coppia 1: EE3890C66DE8DDF5 - C14AC9A60A7A9C94
		Coppia 2: 5A87277BA923D51B - FD88419503B71039
		Coppia 3: 454D604CD25BF405 - 5A004D2507510929
		Tipologia: esercizio A
		Timestamp: 2014-10-25 11:36:30
		Tempo: 16 minuti
		k1 = 17e093 && k2 = 16932d

	
	Chiavi da 28:
		k1 = k2 =
	
	Chiavi da 32:
		k1 = k2 =
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
let procs = 1;;
let dataX = "45EDEFB0FAED3310";;
let dataY = "1CD4808D766E3A5D";;
let start = pow 2 (bits-1);;
let n = pow 2 18;;


let middleX = Hashtbl.create keynum;;


let int_to_hexs i = 
	let s = Printf.sprintf "%04x" i in
	let l = String.length s in
	if l < hexchar then (String.make (hexchar-l) '0') ^ s
	else s
;;




let middleMake d mx =
	let n' = start + n in
	let rec middle k =
		if k <= n' then
			let khex = (int_to_hexs k) in
			if k mod 50000 = 0 then Printf.printf "%d ... (%d%%) %! " k ((k-start)*100/n) else ();
			Hashtbl.add mx (Encrypt.encrypt khex d) khex;
			middle (k+1)
		else ()
	in middle start
;;



let middleAttackSingle mx y =
	for k2 = 0 to keynum do
		let k2 = keynum-k2 in
		let k2hex = (int_to_hexs k2) in
		let ed = Decrypt.decrypt k2hex y in
			try
				let k1 = Hashtbl.find mx ed in Printf.printf "k1 = %s && k2 = %s\n%!" k1 k2hex;
				exit 0
			with Not_found -> ();
	done
;;



let middleJob mx y istart iend i =
	Printf.printf "%d - Start from %d to %d\n%!" i istart iend;
	for k2 = istart to iend do
		let k2hex = (int_to_hexs k2) in
		let ed = Decrypt.decrypt k2hex y in
			try
				let k1 = Hashtbl.find mx ed in Printf.printf "k1 = %s && k2 = %s\n%!" k1 k2hex;
				exit 0
			with Not_found -> ();
	done;
;;



(* Multi process attack *)
let middleAttack mx y nprocs =
	let step = keynum/nprocs in
	let rec deploy nxst i =
		if i = 0 then ()
		else
			match fork () with
				| 0 -> middleJob mx y nxst (nxst+step) i
				| pid -> deploy (nxst+step) (i-1)
	in deploy 0 nprocs
;;



let nprocs = 
	if (Array.length Sys.argv) > 1 then
		(Scanf.sscanf (Sys.argv.(1)) "%d" (fun n -> n))
	else
		0
;;


let () =
	Printf.printf "Creating middleX...\n%!";
	middleMake dataX middleX;
	Printf.printf "\nSearching meets...\n%!"; 
	if nprocs = 0 then 
		middleAttackSingle middleX dataY
	else
		middleAttack middleX dataY nprocs
;;
