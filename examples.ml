(*Exemple de création de programme *)

let empty_program = Commands([],(int_to_nat 5 true),[Register((int_to_nat 23 false))]) ;;
let program_without_register = Commands([],(int_to_nat 1 true),[]) ;;


(* Programme qui additionne un registre R1 de valeur r1=12 avec un registre R2 de valeur r2=6*)
let addition_program = Commands(
	[Jump((int_to_nat 2 true),(int_to_nat 3 true),(int_to_nat 5 false));
	Successor(int_to_nat 3 true);
	Successor((int_to_nat 1 true));
	Jump((int_to_nat 1 true),(int_to_nat 1 true),(int_to_nat 1 false))],
	
	(int_to_nat 1 true),
	
	[Register((int_to_nat 12 false));Register((int_to_nat 6 false));Register(Null)]);; 
	
(*Programme qui soustrait r2 à r1 (fail si r2 > r1)*)
let subtraction_program = Commands(
	[Jump((int_to_nat 1 true),(int_to_nat 2 true),(int_to_nat 5 false));
	Successor((int_to_nat 3 true));
	Successor((int_to_nat 2 true));
	Jump((int_to_nat 1 true),(int_to_nat 1 true),(int_to_nat 1 false));
	Copy((int_to_nat 3 true),(int_to_nat 1 true))],
	
	(int_to_nat 1 true),
	
	[Register(int_to_nat 25 false);Register(int_to_nat 15 false);Register(Null)]);;
	 
(*Programme qui effectue une division par 2 et renvoie la valeur si c'est possible sinon boucle à l'infini*)
let divisional_program = Commands(
	[Jump((int_to_nat 1 true),(int_to_nat 2 true),(int_to_nat 6 true));
	Successor((int_to_nat 3 true));
	Successor((int_to_nat 2 true));
	Successor((int_to_nat 2 true));
	Jump((int_to_nat 1 true),(int_to_nat 1 true),(int_to_nat 1 true));
	Copy((int_to_nat 3 true),(int_to_nat 1 true))],
	
	(int_to_nat 1 true),
	
	[Register((int_to_nat 246 false));Register((int_to_nat 2 false));Register((int_to_nat 1 false))]);; 
	
(*Programme qui multiplie r1=201 et r2=4*)
let multiplication_program = Commands(
	 [Jump((int_to_nat 1 true), (int_to_nat 3 true), (int_to_nat 12 true));
	 Jump((int_to_nat 2 false), (int_to_nat 3 true), (int_to_nat 11 true));
	 Copy((int_to_nat 1 true), (int_to_nat 3 true));
	 Successor((int_to_nat 5 true));
	 Jump((int_to_nat 2 true), (int_to_nat 5 true), (int_to_nat 11 true));
	 Zero((int_to_nat 4 true));
	 Successor((int_to_nat 3 true));
	 Successor((int_to_nat 4 true));
	 Jump((int_to_nat 1 true), (int_to_nat 4 true), (int_to_nat 4 true));
	 Jump((int_to_nat 1 true), (int_to_nat 1 true), (int_to_nat 7 true));
	 Copy((int_to_nat 3 true), (int_to_nat 1 true));
	 Zero((int_to_nat 2 true));
	 Zero((int_to_nat 3 true));
	 Zero((int_to_nat 4 true));
	 Zero((int_to_nat 5 true))]

	,(int_to_nat 1 true),
	
	[Register((int_to_nat 201 false));Register((int_to_nat 4 false));Register(Null);Register(Null);Register(Null)]);;
	
	
(*exemples de déclarations de registres*)
	
(*registre de valeur de l'entier naturel 2*)
let register1 = Register(Succ(Succ(Null))) ;;  

(*registre de valeur de l'entier naturel 36*)
let register2 = Register(int_to_nat 36 false) ;;

(*exemples de déclarations de registres*)

(*entier naturel = 6*)
let six = Succ(Succ(Succ(Succ(Succ(Succ(Null))))));;
(*vérification en le convertissant en entier*)
nat_to_int six false ;;


(*exemples d'appels de fonctions*)
int_to_nat 0 true ;; (*échoue car on essaye de convertir 0 comme entier naturel plus grand que 0*)

int_to_nat 0 false ;; (*fonctionne et créé un entier naturel égal à 0 *)

int_to_nat 15 false ;; (* convertit 15 en nombre naturel *)

nat_to_int (Succ(Succ(Succ(Succ(Null))))) false ;; (* convertit le nombre naturel 4 en nombre entier relatif 4 *)
