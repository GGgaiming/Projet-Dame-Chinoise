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

let rec inserer_fin (l) (x)=
	match l with
	|[]->[x]
	|t::q-> t::(inserer_fin t x)
;;


let tourner_liste (a: 'a list): 'a list =
	match a with 
	|[] -> []
	|t::q -> inserer_fin q t
;;
