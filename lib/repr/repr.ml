open Graphics
open Graph

let draw_ville taille i j =
  set_color (rgb 231 76 60);
  fill_rect (i * taille) (j * taille) taille taille

let draw_route taille i j =
  set_color black;
  fill_rect (i * taille) (j * taille) taille taille

let draw_tunnel taille i j =
  set_color (rgb 190 190 190);
  fill_rect (i * taille) (j * taille) taille taille

let draw_pont taille i j =
  set_color (rgb 84 40 0);
  fill_rect (i * taille) (j * taille) taille taille

let draw_eau taille i j =
  set_color blue;
  fill_rect (i * taille) (j * taille) taille taille

let draw_sol taille i j =
  set_color (rgb 29 131 72);
  fill_rect (i * taille) (j * taille) taille taille

let draw_inter taille i j =
  set_color (rgb 255 255 0);
  fill_rect (i * taille) (j * taille) taille taille

let main tab n size =
  open_graph "";
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      match tab.(i).(j) with
      | _, Ville -> draw_ville size j (n - i - 1)
      | _, Route _ -> draw_route size j (n - i - 1)
      | _, Tunnel _ -> draw_tunnel size j (n - i - 1)
      | _, Pont _ -> draw_pont size j (n - i - 1)
      | _, Eau -> draw_eau size j (n - i - 1)
      | _, Sol -> draw_sol size j (n - i - 1)
      | _, Intersection -> draw_inter size j (n - i - 1)
    done
  done;
  let _ = wait_next_event [ Key_pressed ] in
  close_graph ()
