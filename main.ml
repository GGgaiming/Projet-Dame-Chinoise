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
let diff_case ((i,j,k): case) ((v1,v2,v3): vecteur) : case = (i-v1,j-v2,k-v3);;


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

let calcul_pivot ((i,j,k):case) ((a,b,c):case):case option=
if (a+i) mod 2=0 && (b+j) mod 2=0 && (c+k) mod 2=0 then
Some ((a+i)/2,(b+j)/2,(c+k)/2)
else None;;

(*
	Question 9 :
*)

let vec_et_dist (i,j,k:case) (x,y,z:case): vecteur*int =
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

let rec der_liste (l: case list): case=
	match l with
	|[x]-> x
	|h::q -> (der_liste q)
;;

(*
Question 11
*)


let rec remplir_segment (m:int) ((i,j,k):case):case list=
match m with
|1->[(i,j,k)]
|x->(remplir_segment (x-1) (i,j,k))@[i,(j+(x-1)),(k-(x-1))]
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

(*
Question 14 :	 
*)

let rec colorie (c:couleur) (lc:case list): case_coloree list=
match lc with
|[]->[]
|pr::fin->let (i,j,k)=pr in ((i,j,k),c)::(colorie c fin)
;;

(*
Question 15 :	 
*)

let tourner_config (conf:configuration):configuration=
let lcase,lcoul,dim=conf in
    let rec aux (lcase:case_coloree list)=
        match lcase with
        |[]->[]
        |(case,coul)::fin->(tourner_case (6/(List.length lcoul)) case,coul)::(aux fin)
    in
(aux lcase),tourner_liste lcoul,dim;;

(*
    Question 16 :	 
    *)


let remplir_init (l:couleur list)(d:dimension):configuration=
let nbj=(List.length l) in
let rec aux (conf:configuration)=
	let lc,lcoul,dim=conf in
	match lc,lcoul with
	|lc,[]->let lc3,lcoul,dim=(tourner_config (lc,[Bleu],dim)) in lc3,l,dim
	|lc,pr::fin->let lc3,lcoul,dim=tourner_config (lc,[Bleu],dim) in 
					let lc2=lc@colorie pr (remplir_triangle_bas d (-d-1,d-2,d)) in
					aux (List.map (fun (a,b)->((tourner_case (6/(List.length l)) a),b))lc2,fin,dim)
	in
aux ([],l,d);;

(*
    Question 17 :	 
    *)

let rec associe (a:'a) (l:('a*'b) list) (defaut:'b):'b =
match l with
|[]->defaut
|(e,c)::fin->if e=a then c else associe a fin defaut
;;

let quelle_couleur (c:case) (conf:configuration):couleur=
let lcase,lcoul,dim=conf in
associe c lcase Libre;;

(*
    Question 18 :	 
    *)

let  supprime_dans_config (conf:configuration) (c:case):configuration=
let lcase,lcoul,dim=conf in
let rec aux (lcase:case_coloree list):case_coloree list=
    match lcase with
    |[]->[]
    |(c,_)::fin->fin
    |pr::fin->(aux fin)
in
(aux lcase),lcoul,dim;;

(*
    Question 19 :	 
    *)

type coup = Du of case * case | Sm of case list;;

let est_coup_valide (conf:configuration) (cp:coup):bool=
match cp with
|Du(c1,c2) ->
    let lcase,lcoul,dim=conf in
    (sont_cases_voisines c1 c2)&&((associe c1 lcase Libre)=(List.hd lcoul))
    &&(associe c2 lcase Libre =Libre)&&(est_dans_losange c2 dim)
|_-> failwith "Saut multiples non implementes"
;;


(*
    Question 20 :	 
    *)

let appliquer_coup (conf:configuration) (cp:coup):configuration=
let lcase,lcoul,dim=conf in
match cp with
|Du(c1,c2)->let lcase2=(c2,quelle_couleur c1 conf)::lcase in
            (supprime_dans_config (lcase2,lcoul,dim) c1)
;;

(*
    Question 21 :	 
    *)


let mettre_a_jour_configuration (conf:configuration) (cp:coup):configuration=
match cp with
|Du(c1,c2) -> if not (est_coup_valide conf (Du(c1,c2))) then 
    failwith "Ce coup n’est pas valide, le joueur doit rejouer"
    else appliquer_coup conf (Du(c1,c2))
   
|_-> failwith "Saut multiples non implementes"
;;


(*
    Question 22 :	 
    *)

let est_libre_seg (c1:case) (c2:case) (conf:configuration):bool=
let (v,d)=(vec_et_dist c1 c2) in
let rec aux (v,d:vecteur*int) (c1:case) (c2:case)=
    let (i,j,k)=v in
    match d with
    |0->true
    |1->true
    |n->(let (a,b,c)=c1 in (quelle_couleur ((a+(n-1)*i),(b+(n-1)*j),(c+(n-1)*k)) conf)=Libre)&&(aux (v,(n-2)) c1 c2) 
    in
aux (v,d) c1 c2;;
"""
let conf2=([((0,0,0),Bleu);((1,1,1),Libre);((2,2,2),Bleu)],[Bleu],3)
let conf3=([((0,0,0),Libre);((1,1,1),Bleu);((2,2,2),Bleu)],[Bleu],3)
"""

(*
    Question 23 :	 
    *)

let est_saut (c1:case) (c2:case) (conf:configuration):bool=
let lcase,lcoul,dim=conf in
let pr::fin=lcoul in
let p=calcul_pivot c1 c2 in
match p with
|None->false
|Some(a,b,c)->(est_libre_seg c1 (a,b,c) conf)&&(est_libre_seg (a,b,c) c2 conf)
;;

(*
    Question 24 :	 
    *)

let rec est_saut_multiple (lcase:case list)(conf:configuration):bool=
match lcase with
|[]->true
|[(i,j,k)]->true
|c1::c2::fin->(est_saut c1 c2 conf)&&(est_saut_multiple ([c2]@fin) conf)
;;


(*
    Question 25 :	 
    *)



let est_coup_valide (conf:configuration) (cp:coup):bool=
match cp with
|Du(c1,c2) ->
    let lcase,lcoul,dim=conf in
    (sont_cases_voisines c1 c2)&&((associe c1 lcase Libre)=(List.hd lcoul))
    &&(associe c2 lcase Libre =Libre)&&(est_dans_losange c2 dim)
|Sm([])->true
|Sm(pr::fin)-> est_saut_multiple (pr::fin) conf
;;



let appliquer_coup (conf:configuration) (cp:coup):configuration=
let lcase,lcoul,dim=conf in
match cp with
|Du(c1,c2)->let lcase4=[(c1,Libre)]@[(c2,quelle_couleur c1 conf)] in
			let lcase2,lcoul,dim=(supprime_dans_config conf c1) in
			let lcase3,lcoul,dim=(supprime_dans_config (lcase2,lcoul,dim) c2) in
			lcase4@lcase3,lcoul,dim
|Sm(l)->let lcase2=[(List.hd l,Libre)]@[(der_liste l,(quelle_couleur (List.hd l) conf))] in
		let lcase3,lcoul,dim=(supprime_dans_config (lcase,lcoul,dim) (List.hd l)) in
		let lcase4,lcoul,dim=(supprime_dans_config (lcase3,lcoul,dim) (der_liste l)) in
		lcase2@lcase4,lcoul,dim
;;

let mettre_a_jour_configuration (conf:configuration) (cp:coup): configuration=
	if est_coup_valide conf cp then appliquer_coup conf cp
	else failwith "Ce coup n'est pas valide, le joueur doit rejouer"




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
(*Q29*)


let rec faire_un_triangle (dim:dimension) ((i,j,k):case) : case list =
	match dim with
	|1->[(i,j,k)]
	|_->(remplir_segment dim (i,j,k))@faire_un_triangle (dim-1) (i-1,j+1,k)
;;

let  toutes_les_cases (config:configuration):case list =
	let liste_case, liste_couleur, dim = config in
	let liste_sortie = (faire_un_triangle (2*dim) (-1,dim-1,dim))@(remplir_segment (2*dim+1) (0,-1*dim,dim)) in
	let rec aux (lcase:case list)=
		match lcase with
		|[]->[]
		|case::fin->(tourner_case (6/(List.length liste_couleur)) case)::(aux fin)
	in
	(aux liste_sortie)@(faire_un_triangle (2*dim) (-1,dim-1,dim))
;;


toutes_les_cases ([],[Vert;Rouge],1);;

let rec liste_des_coups (config:configuration) (case:case):(case*coup)list=
	(*parcours la liste de case et test si un coup partant de la case en paramètres peut arriver a la case de la liste*)
	let liste_case, liste_couleur, dim = config in
	match liste_case with 
	|[] -> []
	|(cs,couleur)::t -> if est_coup_valide (config) (Du(cs,case)) then (case,Du(cs,case))::liste_des_coups (t,liste_couleur,dim) case else liste_des_coups (t,liste_couleur,dim) case
;;

let rec test_des_coups (config:configuration):(case * coup) list=
	let liste_cases,liste_couleur,dim = config in 
	match liste_cases with 
	|[] -> []
	|(case,coul)::t -> (liste_des_coups (t,liste_couleur,dim) case )@(test_des_coups (t,liste_couleur,dim))
;;

let coup_possibles (config:configuration) (case:case_coloree):(case*coup) list =
	let case_liste, couleur_liste, dim = config in 
	let les_cases = toutes_les_cases config  in 
	let cs, couleur = case in
	liste_des_coups ((colorie Libre les_cases)@case_liste,couleur_liste,dim) cs
;;


(*Q30*)
let rec que_mes_pions (liste_case:case_coloree list) (coul:couleur):case_coloree list=
	(*Fonction qui en prenant en paramètre une liste de case colorer et une couleur rend la liste des case appartenant à ctte couleur*)
	match liste_case with
	|[]->[]
	|(case,coul2)::t -> if coul2 = coul then 
												(case,coul2)::que_mes_pions t coul
											else
												que_mes_pions t coul
;;




let strategie_gloutonne (config:configuration):coup=
	let liste_case, liste_couleur,dim = config in
	let mes_cases = que_mes_pions (liste_case)