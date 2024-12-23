open Unionfind

type t = float array array

type terrain =
  | Sol
  | Ville
  | Intersection
  | Route of float
  | Tunnel of float
  | Pont of float
  | Eau

let graphe_intermediaire l n =
  let liste_adj = Array.make n [] in
  let rec aux liste =
    match liste with
    | [] -> ()
    | (p, (sd, sp)) :: t ->
        liste_adj.(sd) <- (p, sp) :: liste_adj.(sd);
        liste_adj.(sp) <- (p, sd) :: liste_adj.(sp);
        aux t
  in
  aux l;
  liste_adj

let kruskal n aretes =
  let classes = creation n in
  let compare_aretes (p1, _, _) (p2, _, _) = compare p1 p2 in
  let tri_aretes = List.sort compare_aretes aretes in
  let rec aux res l =
    match l with
    | [] -> res
    | (p, (sd, sp), chemin) :: t -> begin
      print_int 69;
      print_newline();
      let _ = trouver classes sp in
        if trouver classes sd <> trouver classes sp then (
          print_int 23; print_newline();
          unir classes sd sp;
          aux ((p, (sd, sp), chemin) :: res) t)
        else(print_int 24; print_newline(); aux res t)
        
      end
  in
  
  aux [] tri_aretes

type ('a, 'b) filedeprio = {
  mutable file : ('b * 'a) list;
  mutable taille : int;
}

let create () = { file = []; taille = 0 }
let is_empty f = f.taille = 0

let push v p f =
  let rec aux v p = function
    (* crée la nouvelle liste *)
    | [] -> [ (p, v) ]
    | (p', v') :: q when p < p' -> (p, v) :: (p', v') :: q
    | t :: q -> t :: aux v p q
  in
  f.file <- aux v p f.file;
  f.taille <- f.taille + 1

let pop f =
  assert (f.taille > 0);
  let _, v = List.hd f.file in
  f.file <- List.tl f.file;
  f.taille <- f.taille - 1;
  v

  
let absF (f : float) = if f > 0.0 then f else f *. -1.0

let list_from_mat_adja m =
  let taille = Array.length m in
  let liste_adja = Array.make taille [] in
  for i = 0 to taille - 1 do
    for j = 0 to taille - 1 do
      if m.(i).(j) <> infinity then
        liste_adja.(i) <- (j, m.(i).(j)) :: liste_adja.(i)
    done
  done;
  liste_adja

let a_star g s1 s2 =
  let dimension_tableau = int_of_float (sqrt (float_of_int (Array.length g))) in
  let carre a = a *. a in
  let distance_oiseau a b =
    let x_a = float_of_int (a mod dimension_tableau) in
    let y_a = float_of_int (a / dimension_tableau) in
    let x_b = float_of_int (b mod dimension_tableau) in
    let y_b = float_of_int (b / dimension_tableau) in
    sqrt (carre (x_a -. x_b) +. carre (y_a -. y_b))
  in
  let n = Array.length g in
  let distance = Array.make n infinity in
  let chemin = Array.make n [] in
  chemin.(s1) <- [ s1 ];
  distance.(s1) <- 0.;
  let filep = create () in
  push s1 0. filep;
  let condition = ref true in
  while (not (is_empty filep)) && !condition do
    let x = pop filep in
    if x = s2 then condition := false
    else
      List.iter
        (fun (t, p) ->
          if distance.(t) > distance.(x) +. p +. distance_oiseau x t then (
            distance.(t) <- distance.(x) +. p;
            push t (distance.(x) +. p +. distance_oiseau x t) filep;
            chemin.(t) <- t :: chemin.(x)))
        g.(x)
  done;
  (distance.(s2), List.rev chemin.(s2))

(*let resultat = a_star (list_from_mat_adja (cout_autour planete_terre));;*)
