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

	
*)
open Unix;;

module Middle = Map.Make(String);;
let bits = 20;;
let hexchar = bits / 4;;
let keynum = 1048576;;
let procs = 1;;

let int_to_hexs i = 
	let s = Printf.sprintf "%04x" i in
	let l = String.length s in
	if l < hexchar then (String.make (hexchar-l) '0') ^ s
	else s
;;



let middleMake f d =
	let rec middle k m =
		if k <= keynum then
			let khex = (int_to_hexs k) in
			if k mod 50000 = 0 then Printf.printf "%d ...%! " k else ();
			middle (k+1) (Middle.add (f khex d) khex m)
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
	let middleX = Printf.printf "Creating middleX...\n%!"; middleMake Encrypt.encrypt dataX in
	Printf.printf "\nSearching meets...\n%!"; middleAttackSingle middleX dataY
;;