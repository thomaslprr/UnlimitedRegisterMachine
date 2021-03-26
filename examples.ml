(*Exemple de création de programme *)

let programmevide = Commands([],(int_to_nat 5 true),[Register((int_to_nat 23 false))]) ;;
let programmesansregistre = Commands([],(int_to_nat 1 true),[]) ;;


(* Programme qui additionne un registre R1 de valeur r1=12 avec un registre R2 de valeur r2=6*)
let programme1 = Commands(
	[Jump((int_to_nat 2 true),(int_to_nat 3 true),(int_to_nat 5 false));
	Successor(int_to_nat 3 true);
	Successor((int_to_nat 1 true));
	Jump((int_to_nat 1 true),(int_to_nat 1 true),(int_to_nat 1 false))],
	
	(int_to_nat 1 true),
	
	[Register((int_to_nat 12 false));Register((int_to_nat 6 false));Register(Null)]);; 
	
(*Programme qui soustrait r2 à r1 (fail si r2 > r1)*)
let programme2 = Commands(
	[Jump((int_to_nat 1 true),(int_to_nat 2 true),(int_to_nat 5 false));
	Successor((int_to_nat 3 true));
	Successor((int_to_nat 2 true));
	Jump((int_to_nat 1 true),(int_to_nat 1 true),(int_to_nat 1 false));
	Copy((int_to_nat 3 true),(int_to_nat 1 true))],
	
	(int_to_nat 1 true),
	
	[Register(int_to_nat 25 false);Register(int_to_nat 15 false);Register(Null)]);;
	 
(*Programme qui effectue une division par 2 et renvoie la valeur si c'est possible sinon boucle à l'infini*)
let programme3 = Commands(
	[Jump((int_to_nat 1 true),(int_to_nat 2 true),(int_to_nat 6 true));
	Successor((int_to_nat 3 true));
	Successor((int_to_nat 2 true));
	Successor((int_to_nat 2 true));
	Jump((int_to_nat 1 true),(int_to_nat 1 true),(int_to_nat 1 true));
	Copy((int_to_nat 3 true),(int_to_nat 1 true))],
	
	(int_to_nat 1 true),
	
	[Register((int_to_nat 246 false));Register((int_to_nat 2 false));Register((int_to_nat 1 false))]);; 
	
(*Programme qui multiplie r1=201 et r2=4*)
let programme4 = Commands(
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
