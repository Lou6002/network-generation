open Repr
open Terrain

let planete_terre = Array.make_matrix 10 10 0.;;

let matrice_aleatoire m n =
  Random.self_init ();
  for i = 0 to (n-1) do
    for j = 0 to n - 1 do
      m.(i).(j) <- Random.float 20.
    done;
  done;;

let _ = matrice_aleatoire planete_terre 10;;

let temps_execution_2 =
  let debut = Unix.gettimeofday () in
  let res = (creation_reseau_routier (planete_terre) [(0,0);(2,3);(9,0);(9,9)]) in
  impression_lucie_tuples res 100 "resultat.txt";
  let fin = Unix.gettimeofday () in
  (res, fin -. debut);;


