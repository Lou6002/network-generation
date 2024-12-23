open Graph

let absF (f:float) = if f > 0.0 then f else (f *. -1.0);;


let est_accessible x y nb_ligne nb_colonne =
  let ligne_x,colonne_x = x/nb_colonne,x mod nb_colonne in
  let ligne_y,colonne_y = y/nb_colonne,y mod nb_colonne in
  0<=x&&x<nb_ligne*nb_colonne&&0<=y&&y<nb_ligne*nb_colonne&&abs(ligne_x-ligne_y)<=1&&abs(colonne_x-colonne_y)<=1;;


let diagonale x terrain i j nb_colonne (*nb_ligne*) = sqrt (2.)+.(absF (terrain.(i).(j)-.terrain.(x/nb_colonne).(x mod nb_colonne)));;

let lateral x terrain i j nb_colonne (*nb_ligne*) = 1.+.(absF (terrain.(i).(j)-.terrain.(x/nb_colonne).(x mod nb_colonne)));;

let cout_autour_lst (terrain:float array array) =
  let nb_ligne = Array.length terrain in 
  let nb_colonne = Array.length terrain.(0) in
  let lst_adjacence = Array.make (nb_ligne*nb_colonne) [] in
  for i=0 to (nb_ligne-1) do 
    for j=0 to (nb_colonne-1) do 
      let numero = i*nb_colonne+j in
      if est_accessible (numero-nb_colonne-1) (i*nb_colonne+j) nb_ligne nb_colonne then lst_adjacence.(i*nb_colonne+j)<-(numero-nb_colonne-1,diagonale (numero-nb_colonne-1) terrain i j nb_colonne (*nb_ligne*))::lst_adjacence.(i*nb_colonne+j);
      if est_accessible (numero-nb_colonne) (i*nb_colonne+j) nb_ligne nb_colonne then lst_adjacence.(i*nb_colonne+j)<-(numero-nb_colonne,lateral (numero-nb_colonne) terrain i j nb_colonne (*nb_ligne*))::lst_adjacence.(i*nb_colonne+j);
      if est_accessible (numero-nb_colonne+1) (i*nb_colonne+j) nb_ligne nb_colonne then lst_adjacence.(i*nb_colonne+j)<-(numero-nb_colonne+1,diagonale (numero-nb_colonne+1) terrain i j nb_colonne (*nb_ligne*))::lst_adjacence.(i*nb_colonne+j);
      if est_accessible (numero-1) (i*nb_colonne+j) nb_ligne nb_colonne then lst_adjacence.(i*nb_colonne+j)<-(numero-1,lateral (numero-1) terrain i j nb_colonne (*nb_ligne*))::lst_adjacence.(i*nb_colonne+j);
      if est_accessible (numero+1) (i*nb_colonne+j) nb_ligne nb_colonne then lst_adjacence.(i*nb_colonne+j)<-(numero+1,lateral (numero+1) terrain i j nb_colonne (*nb_ligne*))::lst_adjacence.(i*nb_colonne+j);
      if est_accessible (numero+nb_colonne-1) (i*nb_colonne+j) nb_ligne nb_colonne then lst_adjacence.(i*nb_colonne+j)<-(numero+nb_colonne-1,diagonale (numero+nb_colonne-1) terrain i j nb_colonne (*nb_ligne*))::lst_adjacence.(i*nb_colonne+j);
      if est_accessible (numero+nb_colonne) (i*nb_colonne+j) nb_ligne nb_colonne then lst_adjacence.(i*nb_colonne+j)<-(numero+nb_colonne,lateral (numero+nb_colonne) terrain i j nb_colonne (*nb_ligne*))::lst_adjacence.(i*nb_colonne+j);
      if est_accessible (numero+nb_colonne+1) (i*nb_colonne+j) nb_ligne nb_colonne then lst_adjacence.(i*nb_colonne+j)<-(numero+nb_colonne+1,diagonale (numero+nb_colonne+1) terrain i j nb_colonne (*nb_ligne*))::lst_adjacence.(i*nb_colonne+j);
    done;
  done;
  lst_adjacence;; 


let terraformer_liste (terrain:float array array) lst_chemin =
  let dimension_tableau = Array.length terrain in
  let new_terrain = Array.make_matrix (dimension_tableau) (dimension_tableau) (0.,Sol) in 
  for i = 0 to dimension_tableau-1 do
    for j = 0 to dimension_tableau-1 do 
      let le_type_de_terrain = ref Sol in
      if terrain.(i).(j)<0. then le_type_de_terrain:=Eau;
      new_terrain.(i).(j)<-(terrain.(i).(j),!le_type_de_terrain)
    done;
  done;
  (*nouveau tableau Intersection*)
  let hauteur_point_probable_intersection = Array.make (dimension_tableau*dimension_tableau) neg_infinity in
  (*fin du nouveau tableau Intersection*)
  
  let modifier_new_terrain chemin =

    let s1 = match chemin with 
      |[]->assert false 
      |t::_ (*q*) ->t in
      
    let case_dans_terrain x =
      terrain.(x/dimension_tableau).(x mod dimension_tableau) in
      
    (*gestion hauteur début du chemin*)

    let hauteur_actuelle_route = ref (0.) in

    if hauteur_point_probable_intersection.(s1) <> neg_infinity then
      hauteur_actuelle_route:=hauteur_point_probable_intersection.(s1)
    else begin
      hauteur_actuelle_route := (case_dans_terrain s1); hauteur_point_probable_intersection.(s1)<-(case_dans_terrain s1) end;

    (*fin gestion hauteur début chemin*)

    if !hauteur_actuelle_route < case_dans_terrain s1 then 
      new_terrain.(s1/dimension_tableau).(s1 mod dimension_tableau)<-(case_dans_terrain s1,Tunnel (!hauteur_actuelle_route))
    else begin
      if !hauteur_actuelle_route > case_dans_terrain s1 then
        new_terrain.(s1/dimension_tableau).(s1 mod dimension_tableau)<-(case_dans_terrain s1,Pont (!hauteur_actuelle_route))
      else
        new_terrain.(s1/dimension_tableau).(s1 mod dimension_tableau)<-(case_dans_terrain s1,Route (!hauteur_actuelle_route)) end;
          
    (*Point qui sont intersection*)
    
    let rec parcours c = match c with
      |[]->assert false 
      |[y]-> (
        if !hauteur_actuelle_route-.case_dans_terrain y > 1. then begin
          hauteur_actuelle_route := !hauteur_actuelle_route -. 1.;
          new_terrain.(y/dimension_tableau).(y mod dimension_tableau) <- (case_dans_terrain y,Pont (!hauteur_actuelle_route))
        end
        else if !hauteur_actuelle_route-.case_dans_terrain y < (-1.) then begin
          hauteur_actuelle_route := !hauteur_actuelle_route-. 1.;
          new_terrain.(y/dimension_tableau).(y mod dimension_tableau) <- (case_dans_terrain y,Tunnel (!hauteur_actuelle_route))
        end
        else begin
          hauteur_actuelle_route := case_dans_terrain y;
          new_terrain.(y/dimension_tableau).(y mod dimension_tableau) <- (case_dans_terrain y,Route (!hauteur_actuelle_route))
        end;
        hauteur_point_probable_intersection.(y) <- (!hauteur_actuelle_route)
        )
      | (*x::*) y::q when case_dans_terrain y < 0.
        -> (
        new_terrain.(y/dimension_tableau).(y mod dimension_tableau) <- (case_dans_terrain y,Pont (!hauteur_actuelle_route));
        parcours (y::q)
        )
      | (*x::*) y::q when !hauteur_actuelle_route < case_dans_terrain y
        -> (
        if absF (!hauteur_actuelle_route -. case_dans_terrain y) > 1. then begin
          hauteur_actuelle_route := !hauteur_actuelle_route +. 1.;
          new_terrain.(y/dimension_tableau).(y mod dimension_tableau) <- (case_dans_terrain y,Tunnel (!hauteur_actuelle_route))
        end
        else begin
          hauteur_actuelle_route := case_dans_terrain y;
          new_terrain.(y/dimension_tableau).(y mod dimension_tableau) <- (case_dans_terrain y,Route (!hauteur_actuelle_route)) end;
        parcours (y::q)
        )
      | (*x::*) y::q when !hauteur_actuelle_route > case_dans_terrain y
        -> (
        if absF (!hauteur_actuelle_route -. case_dans_terrain y) > 1. then begin
          hauteur_actuelle_route:=!hauteur_actuelle_route -. 1.;
          new_terrain.(y/dimension_tableau).(y mod dimension_tableau) <- (case_dans_terrain y,Pont (!hauteur_actuelle_route))
        end
        else begin 
        hauteur_actuelle_route:=case_dans_terrain y;
        new_terrain.(y/dimension_tableau).(y mod dimension_tableau) <- (case_dans_terrain y,Route (!hauteur_actuelle_route))
        end;
        parcours (y::q)
        )
      | (*x::*) y::q
        -> (
          new_terrain.(y/dimension_tableau).(y mod dimension_tableau) <- (case_dans_terrain y,Route (!hauteur_actuelle_route));
          parcours (y::q)
        )
    in parcours chemin
  in List.iter (modifier_new_terrain) lst_chemin;
  new_terrain;;

(*let _,resultat2 =Algo_a_star.a_star (Algo_a_star.list_from_mat_adja (cout_autour planete_terre)) 870 29 ;;

let terraformation2 = terraformer_liste planete_terre [resultat;resultat2];;*)
  

(*LES FONCTIONS POUR PRINT AISEMENT LES NOUVEAUX TYPES*)

let type_terrain_en_str t = match t with
  |Sol ->"Sol"
  |Eau->"Eau"
  |Ville->"Ville"
  |Intersection->"Intersection"
  |Route x ->"Route "^string_of_float x
  |Pont x ->"Pont "^string_of_float x
  |Tunnel x ->"Tunnel "^string_of_float x;;

let conversion_tuples matrice n =
  let s =ref "[| \n" in
  for i=0 to (Array.length matrice)-1 do 
    s:=!s^"[| ";
    for j=0 to (Array.length matrice)-1 do 
      if (j mod n = 0) then
        s:=!s^"\n ("^string_of_float (fst (matrice.(i).(j)))^","^type_terrain_en_str (snd (matrice.(i).(j)))^") ; "
      else
        s:=!s^"( "^string_of_float (fst (matrice.(i).(j)))^","^type_terrain_en_str (snd (matrice.(i).(j)))^") ; "
    done;
    s:= !s^"|]\n";
  done;
  s:=!s^"|]";
  !s;;

let impression_lycee_tuples matrice n file =
  let oc = open_out ("/home/etudiant/Documents/Julien/"^file) in 
  Printf.fprintf oc "%s\n" (conversion_tuples matrice n);
  close_out oc;;

let impression_perso_tuples matrice n file =
  let oc = open_out ("/home/juju4956/Document/TIPE/"^file) in 
  Printf.fprintf oc "%s\n" (conversion_tuples matrice n);
  close_out oc;;

let impression_lucie_tuples matrice n file =
  let oc = open_out ("/home/lucie-roussel/Documents/TIPE v3/"^file) in 
  Printf.fprintf oc "%s\n" (conversion_tuples matrice n);
  close_out oc;;


(*FIN*)

(*impression_perso_tuples terraformation2 30 "resultats_finaux.txt";;*)

let rec intersection_en_ville le_terrain ville = match ville with
  | [] -> le_terrain
  | (x, y)::q -> let h, _ = le_terrain.(x).(y) in
    le_terrain.(x).(y) <- (h, Ville);
    intersection_en_ville le_terrain q

(*Fonction finales*)

let creation_reseau_routier (map:float array array) (liste_coordonnees_villes:(int*int) list) =

  let compteur_test = ref 0 in

  let dimension_tableau = Array.length map in
  let tab_cases = Array.make (List.length liste_coordonnees_villes) (-1) in 

  let compteur = ref 0 in
  let rec aux_tab_cases l= match l with 
    |[]->()
    |(a,b)::q->(tab_cases.(!compteur)<-a*dimension_tableau+b;incr compteur;aux_tab_cases q)
  in aux_tab_cases liste_coordonnees_villes;

  let creer_liste_des_chemins tab =
    let liste_des_chemins_a_creer = ref [] in
    let n = Array.length tab in
    for i = 0 to n-2 do 
      for j= i+1 to n-1 do 
        liste_des_chemins_a_creer:=(a_star (cout_autour_lst map) (tab.(i)) (tab.(j)))::(!liste_des_chemins_a_creer)
      done;
    done;
    !liste_des_chemins_a_creer in

  let liste_des_chemins_sans_intersection = creer_liste_des_chemins tab_cases in
  
  let choisir_nouveaux_points_interets lst_des_chemins =
    let nouvelle_liste_points = ref liste_coordonnees_villes in
    let tableau_des_sommets_rencontre = Array.make (int_of_float ((float_of_int dimension_tableau)**2.)-1) false in
    let rec aux_1 lst_des_chemins = match lst_des_chemins with
      |[]->()
      |t::q-> let rec aux_2 new_list =
                match new_list with 
                |[]->()
                |[(*a*)_]->()
                |t::q->if tableau_des_sommets_rencontre.(t) then nouvelle_liste_points:=(t/dimension_tableau,t mod dimension_tableau)::!nouvelle_liste_points else tableau_des_sommets_rencontre.(t)<-true;aux_2 q in 
          aux_2 t;
          aux_1 q in
    aux_1 lst_des_chemins;
    !nouvelle_liste_points in

  let rec liste_seconds_elements l res = (* res = [] initialement*)
	  match l with
		| [] -> List.rev res
		| (_, chemin) :: q -> liste_seconds_elements q (chemin :: res)
      in

  let coordonnees_points_opti = choisir_nouveaux_points_interets (liste_seconds_elements liste_des_chemins_sans_intersection []) in

  compteur:=0;
  compteur_test:=!compteur_test+1;
    print_int (!compteur_test);
    print_newline();
  let new_tab_cases = Array.make (List.length coordonnees_points_opti) (-1) in
  let rec aux_new_tab_cases l= match l with 
    |[]->()
    |(a,b)::q->(new_tab_cases.(!compteur)<-a*dimension_tableau+b;incr compteur;aux_new_tab_cases q)
  in aux_new_tab_cases coordonnees_points_opti;
  compteur_test:=!compteur_test+1;
    print_int (!compteur_test);
    print_newline();

  let liste_des_chemins_a_creer = creer_liste_des_chemins new_tab_cases in
  
  let premier l = match l with 
	|[]->assert false 
	| t::_ (*q*) -> t
    in
	
  let rec dernier l = match l with 
	|[]->assert false 
	|[a]->a
	| (*t*) _::q->dernier q
    in
  
  let rec conversion_en_aretes lst = match lst with 
	|[]->[]
	|(poids,t)::q->(poids, (premier t,dernier t), t)::(conversion_en_aretes q) in  (*Modifier A* pour le poids: fait! *)
	
  compteur_test:=!compteur_test+1;
  print_int (!compteur_test);
  print_newline();

  let liste_des_aretes = conversion_en_aretes liste_des_chemins_a_creer in (*A mettre dans la fonction de Lucie : fait! *)
		
  compteur_test:=!compteur_test+1;
  print_int (!compteur_test);
  print_newline();

  let rec recuperer_chemins_post_kruskal lst = match lst with 
	|[]->[]
	|(_,_,chemin)::q -> begin print_int 43;
  print_newline();
    chemin::(recuperer_chemins_post_kruskal q) end in

    print_int 64;
    print_newline();

  compteur_test:=!compteur_test+1;
  print_int (!compteur_test);
  print_newline();
  print_int 102;
  print_newline();

  let temp_kruskal = kruskal (dimension_tableau*dimension_tableau) liste_des_aretes in

  print_int 22;
  print_newline();

  compteur_test:=!compteur_test+1;
  print_int (!compteur_test);
  print_newline();

  let chemins_post_kruskal = recuperer_chemins_post_kruskal temp_kruskal in

  compteur_test:=!compteur_test+1;
  print_int (!compteur_test);
  print_newline();

  compteur_test:=!compteur_test+1;
  print_int (!compteur_test);
  print_newline();

  intersection_en_ville (terraformer_liste map chemins_post_kruskal) liste_coordonnees_villes;;


(*TESTS*)

let test_vide = Array.make_matrix 10 10 0.;;

(*impression_perso_tuples (creation_reseau_routier exemple_du_diapo [(0,0);(4,3);(0,4)]) 5 "resultats_finaux.txt";;*)
