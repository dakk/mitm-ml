(*** SPN-DT - Substitution-Permutation network Decryption Tool - Copyright (C) 2014 Sebastian Podda - University of Cagliari ***)

(* PARAMETRI DELLA SPN *)
let sbox_size = 4;;
let sbox_number = 16;;
let round_number = 2;;

(** SUBSTITUTION' e SUBSTITUTION **)
(*  Effettua una sostituzione (inversa) fissata su stringhe 4 bit (da modificare se si varia la sbox_size) *)
let substitution' str = 
	match str with
		"0101" -> "0000"
	| "1100" -> "0001"
	| "0110" -> "0010" 
	| "0000" -> "0011"
	| "1000" -> "0100" 
	| "1111" -> "0101" 
	| "0111" -> "0110"
	| "0100" -> "0111"
  | "0001" -> "1000" 
	| "1010" -> "1001" 
	| "1101" -> "1010" 
	| "0010" -> "1011"
	| "1110" -> "1100"
	| "0011" -> "1101" 
	| "1001" -> "1110"
	| "1011" -> "1111"
	| _ -> "Error with substitution cipher!"
;;

let rec substitution str =
	match str with
		"" -> ""
	| _ -> let len = String.length str - 4 in 
		substitution' (String.sub str 0 4) ^ substitution (String.sub str 4 len)
;;

(** PERMUTATION' e PERMUTATION **)
(*	Restituisce una permutazione inversa (prefissata) di una stringa binaria lunga 64 caratteri *)
let rec permutation' str l  = 
	match l with
		[] -> ""
	| a::l' -> Char.escaped (String.get str a) ^ permutation' str l'
;;

let permutation str =
	permutation' str [62; 10; 42; 18; 39; 2; 28; 13; 56; 61; 60; 45; 53; 51; 24; 38; 27; 49; 50;58; 17; 30; 48; 4; 63; 59; 44; 16; 21; 31; 19; 57; 20; 23; 12; 26; 37; 5; 8;35; 3; 34; 41; 32; 29; 25; 6; 52; 22; 40; 43; 1; 55; 14; 36; 9; 47; 46; 11;15; 33; 0; 54; 7]
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
(*	Esegue un algoritmo di decifratura SPN, espresso ricorsivamente *)
let rec spn_alg' w r k = 
	match r with
		0 -> w
	| _ -> spn_alg' (bstr_xor (substitution (permutation  w)) (get_key k r)) (r-1) k (* Eseguito Nr - 1 volte *)
;;

let spn_alg w k = spn_alg' (bstr_xor (substitution (bstr_xor w (get_key k (round_number+1)))) (get_key k round_number)) (round_number-1) k;;



(** DECIPHER **)
(*  Decifra l'input utilizzando l'algoritmo SPN (al contrario) *)
let decipher str key = spn_alg str (Utils.hex_to_binary key);;

(** UNPAD' e UNPAD - DISABLED **)
(*  Rimuove il padding 100...000 finale alla stringa. NB: se la stringa non e' decifrata con la chiave giusta,
    la rimozione del padding elimina i bit finali (che non sono di padding) alterando la lunghezza della
		stringa stessa. Se pertanto tale lunghezza modulo 8 e' diversa da 0, il padding viene ripristinato *)
let rec unpad' str = 
	let len = String.length str - 1 in
	if (String.get str len  == '1') then
		(String.sub str 0 len)
	else
		unpad' (String.sub str 0 len)
;;

let unpad str = 
	let up = unpad' str in
	let remainder = (String.length up) mod 8 in
	if (remainder == 0) then up
	else up ^ "1" ^ Utils.zero_padding (7 - remainder)
;;	

let unpad str = str;; (* Dummy function *)

(** CBC' e CBC **)
(*  Decifra l'input a blocchi con mode of operation CBC *)
let rec cbc' str key vec =
	match str with
		"" -> ""
	| _ -> 
		let block = String.sub str 0 64  in
		bstr_xor vec (decipher block key) ^ cbc' (String.sub str 64 (String.length str - 64)) key block
;;

let cbc str key =
	Utils.binary_to_hex (unpad (cbc' (Utils.hex_to_binary str) key (Utils.zero_padding 64)))
;;



let decrypt key data = cbc (Utils.check_input (data)) (Utils.check_hex (key));;

