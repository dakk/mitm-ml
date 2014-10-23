(*** SPN-ET - Substitution-Permutation network Encryption Tool - Copyright (C) 2014 Sebastian Podda - University of Cagliari ***)

(* PARAMETRI DELLA SPN *)
let sbox_size = 4;;
let sbox_number = 16;;
let round_number = 2;;

(** SUBSTITUTION' e SUBSTITUTION **)
(*  Effettua una sostituzione fissata su stringhe 4 bit (da modificare se si varia la sbox_size) *)
let substitution' str = 
	match str with
		"0000" -> "0101"
	| "0001" -> "1100"
	| "0010" -> "0110"
	| "0011" -> "0000"
	| "0100" -> "1000"
	| "0101" -> "1111"
	| "0110" -> "0111"
	| "0111" -> "0100"
	| "1000" -> "0001"
	| "1001" -> "1010"
	| "1010" -> "1101"
	| "1011" -> "0010"
	| "1100" -> "1110"
	| "1101" -> "0011"
	| "1110" -> "1001"
	| "1111" -> "1011"
	| _ -> "Error with substitution cipher!"
;;

let rec substitution str =
	match str with
		"" -> ""
	| _ -> let len = String.length str - sbox_size in 
		substitution' (String.sub str 0 sbox_size) ^ substitution (String.sub str sbox_size len)
;;

(** PERMUTATION' e PERMUTATION **)
(*	Restituisce una permutazione (prefissata) di una stringa binaria lunga 64 caratteri *)
let rec permutation' str l  = 
	match l with
		[] -> ""
	| a::l' -> Char.escaped (String.get str a) ^ permutation' str l'
;;

let permutation str =
	permutation' str [61;51;5;40;23;37;46;63;38;55;1;58;34;7;53;59;27;20;3;30;32;28;48;33;14;45;35;16;6;44;21;29;43;60;41;39;54;36;15;4;49;42;2;50;26;11;57;56;22;17;18;13;47;12;62;52;8;31;19;25;10;9;0;24]
;;

(** BSTR_XOR **)
(*  Effettua lo xor tra due stringhe binarie della stessa dimensione *)
let rec bstr_xor s1 s2 =
	let len1 = String.length s1 in
	let len2 = String.length s2 in
	if (len1 == 0) then "" else
	let c1 = String.get s1 0 in
	let c2 = String.get s2 0 in
	let sub1 = String.sub s1 1 (len1 -1) in
	let sub2 = String.sub s2 1 (len2 -1) in
	string_of_int ((Char.code c1 + Char.code c2) mod 2) ^ bstr_xor sub1 sub2
;;




(** GET_KEY **)
(*  Genera una chiave univoca a partire dalla chiave base e dal seed passato (tra 1 e 256) *)
let get_key key seed = 
	let key_seed =
  	bstr_xor ((String.sub key 19 6) ^ "01")  (Utils.char_to_binary (Char.chr ((255 - seed)*246 mod 256))) ^
  	bstr_xor (String.sub key 12 8) (Utils.char_to_binary (Char.chr ((seed + 25) mod 256))) ^
  	bstr_xor ("00" ^ (String.sub key 14 5) ^ "1") (Utils.char_to_binary (Char.chr ((255 - seed)*37 mod 256))) ^
  	bstr_xor (String.sub key 7 8) (Utils.char_to_binary (Char.chr ((255 - seed + 22)*455 mod 256))) ^
  	bstr_xor (String.sub key 20 8) (Utils.char_to_binary (Char.chr (((seed + 154)*3) mod 256))) ^
  	bstr_xor ((String.sub key 5 4) ^ (String.sub key 25 4)) (Utils.char_to_binary (Char.chr ((seed + 97) mod 256))) ^
  	bstr_xor ((String.sub key 26 4) ^ (String.sub key 21 4)) (Utils.char_to_binary (Char.chr ((255 - seed + 19)*344 mod 256))) ^
  	bstr_xor (String.sub key 3 8) (Utils.char_to_binary (Char.chr ((seed*19 / 7) mod 256)))
		in
		bstr_xor ((bstr_xor key (Utils.reverse key)) ^ key) key_seed
		(* NB: di fatto, per il seed che va da 1 a 256, restituisce una keychain;
					 inoltre, piccole variazioni nel seed causano elevate variazioni nella chiave generata *)
;;

(** SPN_ALG' e SPN_ALG **)
(*	Esegue un algoritmo di cifratura SPN, espresso ricorsivamente *)
let rec spn_alg' w r k = 
	match r with
		0 -> w
	| _ -> spn_alg' (permutation (substitution (bstr_xor w (get_key k (round_number - r))))) (r-1) k (* Eseguito Nr - 1 volte *)
;;

let spn_alg w k = 
	bstr_xor (substitution (bstr_xor (spn_alg' w (round_number-1) k) (get_key k round_number))) (get_key k (round_number+1))
;;


(** CIPHER **)
(*  Applica l'algoritmo della SPN all'input *)
let cipher str key = spn_alg str (Utils.hex_to_binary key);;


(** PAD - DISABLED **)
(*  Effettua un padding 100...00 alla stringa in input; la lunghezza del padding e' tale per cui
    la lunghezza della stringa risultante mod 64 = 0 *)
(*let rec pad str = 
	if ((String.length str) mod 64 <> 0) then
		str ^ "1" ^ Utils.zero_padding (63 - ((String.length str) mod 64))
	else
		str ^ "1" ^ Utils.zero_padding 63;;*)

let pad str = str;; (* Dummy function *)


(** CBC' e CBC **)
(*  Applica la cifratura con mode of operation CBC all'input (prima effettua il padding) *)
let rec cbc' str key vec =
	match str with
		"" -> ""
	| _ -> 
		let out = cipher (bstr_xor vec (String.sub str 0 64)) key in
		out ^ cbc' (String.sub str 64 (String.length str - 64)) key out
;;

let cbc str key = 
	cbc' (pad (Utils.hex_to_binary str)) key (Utils.zero_padding 64)
;;
		


let encrypt key data = Utils.binary_to_hex (cbc (Utils.check_input (data)) (Utils.check_hex (key)));;
