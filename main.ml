(*I)
README : Projet Dames Chinoises - INF201
				Groupe :	FALIU Louis
									FORTIER Grégoire
									ESPI Thibaut
									BECCAT-DALLOT Alexis



	Question 1 :

1) Les cases qui satisfont i < -dim sont celles qui sont dans le triangle (branche de l'étoile) sur l'axe i du côté négatif
Dans l'exemple dim = 3 les cases qui satisfont i< -3 sont : {(-4,1,3),(-4,1,2),(-4,1,1),(-5,2,3),(-5,2,2),(-6,3,3)}

2) Les cases qui satisfont i > dim sont celles qui sont dans le triangle (branche de l'étoile) sur l'axe i du côté positif

3) Les cases qui satisfont j < -dim sont celles qui sont dans le triangle (branche de l'étoile) sur l'axe j du côté négatif

4) le point (𝑖, 𝑗, 𝑘) = (2𝑑𝑖𝑚, −𝑑𝑖𝑚, −𝑑𝑖𝑚) est le point à l'extremité de la branche positive sur l'axe des i
Dans l'exemple dim = 3 le point est (6,-3,-3)

5) le point (𝑖, 𝑗, 𝑘) = (−𝑑𝑖𝑚 − 1, 1, 𝑑𝑖𝑚) est le point en haut à gauche de la branche négative sur l'axe des i
Dans l'exemple dim = 3 le point est (-4,1,3)

6) les points appartenant à {𝑖 ≥ −𝑑𝑖𝑚 ∧ 𝑗 ≥ −𝑑𝑖𝑚 ∧ 𝑘 ≥ −𝑑𝑖𝑚} appartiennent au corps de l'étoile ainsi que la branche en haut, la branche en bas à gauche et la branche en bas à droite
*)


type case = int*int*int;;
type vecteur = int*int*int;;
type dimension = int;;
type couleur= Vert | Jaune | Rouge | Noir | Bleu | Marron
| Code of string (*une chaine restreinte a 3 caracteres*)
| Libre;;
type case_coloree = case * couleur ;;
type configuration = case_coloree list * couleur list * dimension;;

(*
	Question 2:

-dim<=k<=dim et -dim<=j<=dim et -2dim<=i<=2dim
*)

let est_dans_losange ((i, j, k) : case)  (dim:dimension): bool =
	(-dim<=k && k<=dim) && (-dim<=j && j<=dim) && (2*(-dim)<=i && i<=2*dim)
;;


(*
	Question 3 :
*)

let est_dans_etoile ((i, j, k) : case) (dim:dimension) : bool =
	est_dans_losange ((i, j, k) : case)  (dim:dimension) || est_dans_losange ((j, k, i) : case)  (dim:dimension) || est_dans_losange ((k, i, j) : case)  (dim:dimension)
;;



(*
	Question 4 :
*)

let rec tourner_case (m:int) ((i,j,k) : case): case =
match m with
| 1 -> (-j,-k,-i)
| m -> tourner_case (m-1) (-j,-k,-i)
;;


(*
	Question 5 :
*)

let translate ((i,j,k): case) ((v1,v2,v3): vecteur) : case =
(i+v1,j+v2,k+v3);;


(*
	Question 6 :
*)
let diff_case ((i,j,k): case) ((v1,v2,v3): vecteur) : case =
(i-v1,j-v2,k-v3);;


(*
	Question 7 :
*)

let sont_cases_voisines ((i,j,k):case) ((x,y,z):case):bool=
	let valeur=diff_case(i,j,k)(x,y,z) in 
	let a,b,c=valeur in 
	((a<=1) && (b<=1))||((a<=1) && (c<=1))||((b<=1) && (c<=1))
;;


(*
	Question 8 :
*)

let calcul_pivot (i,j,k:case) (x,y,z:case):case option=
match x,y,z with 
|i,_,_ -> Some((i+x)/2,(j+y)/2,(k+z)/2)
|_,j,_ -> Some((i+x)/2,(j+y)/2,(k+z)/2)
|_,_,k -> Some((i+x)/2,(j+y)/2,(k+z)/2)
|_,_,_ -> None
;;


(*
	Question 9 :
*)

let vrc_et_dist (i,j,k:case) (x,y,z:case): vecteur*int =
	let a,b,c =diff_case (i,j,k) (x,y,z) in
	if (a<>0) then 
		((a/a,b/a,c/a),abs(a))
	else if (b<>0) then 
		((a/b,b/b,c/b),abs(b))
	else if (c<>0) then 
		((a/c,b/c,c/c),abs(c))
  else
    failwith "vecteur nul" 
;;



			(*Deuxième partie :*)

(*
Question 10 :
*)

let rec tourner_liste (l: 'a list): 'a list =
	match l with
	|[] -> []
	|x::[] -> [x]
	|t::q::h -> q::(tourner_liste (t::h))
;;

let rec der_liste (l:'a list):'a=
	match l with
	|[]->[]
	|x::[]-> x
	|h::q -> der_liste q
;;

(*
Question 11
*)

let rec remplir_segment (m:int)((i,j,k):case): case list =
	match m with
	|0 -> []
	|x -> (i,j,k)::remplir_segment (x-1) ((i,j+1,k-1))
;;

(*
Question 12 :	 
*)

let rec remplir_triangle_bas (m:int)((i,j,k):case): case list =
	match m with 
	|0 -> []
	|x -> (remplir_segment m (i,j,k)) @ remplir_triangle_bas (m-1) (i-1,j+1,k)
;;

(*
Question 13 :	 
*)

let rec remplir_triangle_haut (m:int)((i,j,k):case): case list = 
	match m with
	|0 -> []
	|x -> (remplir_segment m (i,j,k)) @ remplir_triangle_haut (m-1) (i+1,j,k-1)
;;



(*Q26*)
let score (config:configuration):int = 
	let case_liste, couleur_liste,dim = config in 
	let t=List.hd(couleur_liste) in
	List.fold_left (fun acc e -> let case, couleur = e in let i, j ,k = case in if (couleur = t) then i+acc else 0+acc) 0 case_liste
;;

(* test :
	 
score ([((1,0,0),Vert);((0,0,0),Rouge);((5,0,0),Vert);((0,0,0),Bleu)],[Vert;Rouge],2);;

*)

let score_gagnant (dim:dimension):int=
	-1 *(score (remplir_init [Vert] dim))
;;


(*Q27*)

let gagne (config:configuration):bool=
	let case,couleur, dim = config in 
	(score config) = (score_gagnant dim)
;;

(*Q28*)
let rec est_partie (config:configuration) (cp: coup list):couleur =
	let case_liste, couleur_liste, dim = config in 
	if cp =[] then 
		Libre
	else 
		config = appliquer_coup config List.hd(cp)
		if (gagne config) then
			List.hd(cp)
		else
			est_partie (tourner_config config) List.tl(cp)
;;



(*Q29*)
let rec que_mes_pions (case_liste:case list) (couleur:couleur):case list=
	(*Fonction qui renvoie la liste de tous les pions appartenant à un joueur*)
	match case_liste with
	|[]->[]
	|h::t->let case, couleur_case = h if (couleur_case  = couleur) then 
																				h::que_que_mes_pions t couleur 
																		else 
																			que_que_mes_pions t couleur 
;;

let tester_un_mouv (case:case) ((i,j,k):int*int*int):case list=
	

let coup_possibles (config:configuration) (case:case_coloree):(case_coloree,coup) list =
	let case_liste, couleur_liste, dim = config in 
	mes_cases = que_que_mes_pions case_liste couleur 

