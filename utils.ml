(*** SPN-ET - Substitution-Permutation network Encryption Tool - Copyright (C) 2014 Sebastian Podda - University of Cagliari ***)


(** ZERO_PADDING **)
(*	Costruisce una stringa composta da n zeri *)
let rec zero_padding n = String.make n '0';;


(** HEX_TO_BINARY **)
(* 	Converte una stringa esadecimale in una stringa binaria *)
let rec hex_to_binary k =
	let len = String.length k in
	let s' = if (len>1) then hex_to_binary (String.sub k 1 (String.length k - 1)) else "" in
	match k.[0] with
	  '0' -> "0000" ^ s'
	| '1' -> "0001" ^ s'
	| '2' -> "0010" ^ s'
	| '3' -> "0011" ^ s'
	| '4' -> "0100" ^ s'
	| '5' -> "0101" ^ s'
	| '6' -> "0110" ^ s'
	| '7' -> "0111" ^ s'
	| '8' -> "1000" ^ s'
	| '9' -> "1001" ^ s'
	| 'A' -> "1010" ^ s'
	| 'B' -> "1011" ^ s'
	| 'C' -> "1100" ^ s'
	| 'D' -> "1101" ^ s'
	| 'E' -> "1110" ^ s'
	| 'F' -> "1111" ^ s'
	| 'a' -> "1010" ^ s'
	| 'b' -> "1011" ^ s'
	| 'c' -> "1100" ^ s'
	| 'd' -> "1101" ^ s'
	| 'e' -> "1110" ^ s'
	| 'f' -> "1111" ^ s'
	| _ -> failwith "An invalid hexadecimal string was passed to hex_to_binary!"
;;

(** BINARY_TO_HEX **)
(* 	Converte una stringa binaria in una stringa esadecimale *)
let rec binary_to_hex k =
	let len = String.length k in
	let s' = if (len>4) then binary_to_hex (String.sub k 4 (String.length k - 4)) else "" in
	match String.sub k 0 4 with
	  "0000" -> "0" ^ s'
	| "0001" -> "1" ^ s'
	| "0010" -> "2" ^ s'
	| "0011" -> "3" ^ s'
	| "0100" -> "4" ^ s'
	| "0101" -> "5" ^ s'
	| "0110" -> "6" ^ s'
	| "0111" -> "7" ^ s'
	| "1000" -> "8" ^ s'
	| "1001" -> "9" ^ s'
	| "1010" -> "A" ^ s'
	| "1011" -> "B" ^ s'
	| "1100" -> "C" ^ s'
	| "1101" -> "D" ^ s'
	| "1110" -> "E" ^ s'
	| "1111" -> "F" ^ s'
	| _ -> failwith "An invalid binary string was passed to binary_to_hex !"
;;




(** CHAR_TO_BINARY **)
(*  Converte un carattere in una stringa binaria di 8 bit *)
let char_to_binary c =
  let rec strip_bits i s =
    match i with
      0 -> s
    | _ -> strip_bits (i lsr 1) ((string_of_int (i land 0x01)) ^ s) in
  let res = strip_bits (Char.code c) "" in
	let len = String.length res in
	if (len < 8) then
		zero_padding (8-len) ^ res
	else res
;;


(** BINARY_TO_CHAR **)
(*  Converte una stringa binaria di 8 bit in un carattere ASCII esteso *)
let binary_to_char s = Char.chr (int_of_string ("0b" ^ s));;

(** BINARY_TO_STRING **)
(*  Data una stringa, esegue iterativamente binary_to_char a ogni carattere e restituisce il risultato *)
let rec binary_to_string' str =
	match str with
		"" -> ""
	| _ -> let len = String.length str - 8 in 
		Char.escaped (binary_to_char (String.sub str 0 8)) ^ binary_to_string' (String.sub str 8 len)
;;

let rec binary_to_string str = (binary_to_string' str);;


(** STRING_TO_BINARY **)
(*  Come sopra, ma in direzione opposta *)
let rec string_to_binary str =
	match str with
		"" -> ""
	| str -> let c = String.get str 0 in
		char_to_binary c ^ (string_to_binary (String.sub str 1 (String.length str - 1)))
;;

(** REVERSE **)
(*  Restituisce la stringa passata, con i caratteri in ordine inverso *)
let rec reverse str =
	match str with
		"" -> ""
	| _ -> Char.escaped (String.get str (String.length str - 1)) ^ reverse (String.sub str 0 (String.length str - 1))
;;


(* Il programma nasce per gestire chiavi di 32 bit ed estenderle a 64 bit per adattarla al blocco.
   L'informazione per raddoppiare le chiavi è comunque contenuta nei 32 bit scelti dall'utente.
	 La funzione qua sotto consente di usare anche chiavi da 20, 24 o 28 bit (per il challenge), 
	 semplicemente estendendole con zeri per arrivare a 32. Si noti che questo non comporta nessuna 
	 variazione significativa nella sicurezza della chiave (né in positivo, né in negativo) *)
let extend_key str len =
	if (len == 5) then "000" ^ str else
	if (len == 6) then "00" ^ str else
	if (len == 7) then "0" ^ str else
	str
;;


let check_input str = String.uppercase (str);;
let check_hex str = let len = String.length str in String.uppercase (extend_key str len);;
