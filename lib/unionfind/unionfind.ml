type t = { parent : (int, int) Hashtbl.t; rang : (int, int) Hashtbl.t }

let creation n =
  let rang = Hashtbl.create (n + 1) in
  let parent = Hashtbl.create (n + 1) in
  let init j =
    Hashtbl.add rang j 0;
    Hashtbl.add parent j j
  in
  for i = 1 to n do
    init i
  done;
  { parent; rang }

let rec trouver c x =
  let y = Hashtbl.find c.parent x in
  if y = x then x
  else
    let y_r = trouver c y in
    (*Hashtbl.replace c.parent x y_r;*)
    y_r

let unir c x y =
  let x_r = trouver c x in
  let y_r = trouver c y in
  if x_r <> y_r then
    let rang_x = Hashtbl.find c.rang x_r in
    let rang_y = Hashtbl.find c.rang y_r in
    if rang_x > rang_y then Hashtbl.replace c.parent y_r x_r
    else (
      Hashtbl.replace c.parent x_r y_r;
      if rang_x = rang_y then Hashtbl.replace c.rang y_r (rang_y + 1))
