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
	
*)
(*open Unix;;*)
let pow x y =
	let rec pow' x' y' = match y' with
		| 1 -> x'
		| i -> pow' (x'*x) (y'-1)
	in pow' x y
;;


module Middle = Map.Make(String);;
let bits = 20;;
let hexchar = bits / 4;;
let keynum = pow 2 bits;;
let procs = 1;;

let int_to_hexs i = 
	let s = Printf.sprintf "%04x" i in
	let l = String.length s in
	if l < hexchar then (String.make (hexchar-l) '0') ^ s
	else s
;;


(* k : x = keynum : 100            x = 100 * k / keynum *)

let middleMake d =
	let rec middle k m =
		if k <= keynum then
			let khex = (int_to_hexs k) in
			if k mod 50000 = 0 then Printf.printf "%d ... (%d%%) %! " k (k*100/keynum) else ();
			middle (k+1) (Middle.add (Encrypt.encrypt khex d) khex m)
		else m
	in middle 0 Middle.empty
;;



let middleAttackSingle mx y =
	for k2 = 0 to keynum do
		let k2hex = (int_to_hexs k2) in
		let ed = Decrypt.decrypt k2hex y in
			try
				let k1 = Middle.find ed mx in Printf.printf "k1 = %s && k2 = %s\n%!" k1 k2hex
			with Not_found -> ();
	done
;;


let middleJob mx y istart iend i =
	Printf.printf "%d - Start from %d to %d\n%!" i istart iend;
	for k2 = istart to iend do
		let k2hex = (int_to_hexs k2) in
		let ed = Decrypt.decrypt k2hex y in
			try
				let k1 = Middle.find ed mx in Printf.printf "%d - Found! k1 = %s && k2 = %s\n%!" i k1 k2hex
			with Not_found -> ();
	done;
	Printf.printf "%i - End\n%!" i;
;;



(* Multi process attack *)
(*let middleAttack mx y =
	let step = keynum/procs in
	let rec deploy nxst i =
		if i = 0 then ()
		else
			match fork () with
				| 0 -> middleJob mx y nxst (nxst+step) i
				| pid -> deploy (nxst+step) (i-1)
	in deploy 0 procs
;;
*)


let () =
	let dataX = "7B652692CFB963A7" in
	let dataY = "A373589BC4F1A5D3" in
	let middleX = Printf.printf "Creating middleX...\n%!"; middleMake dataX in
	Printf.printf "\nSearching meets...\n%!"; middleAttackSingle middleX dataY
;;
