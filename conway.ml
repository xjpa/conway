type cell = Dead | Alive
type grid = cell list list

let get_cell grid x y =
  try
    match List.nth (List.nth grid x) y with
    | Alive -> Alive
    | Dead -> Dead
  with _ -> Dead

let count_alive_neighbors grid x y =
  let directions = [(-1, -1); (-1, 0); (-1, 1); (0, -1); (0, 1); (1, -1); (1, 0); (1, 1)] in
  List.fold_left (fun acc (dx, dy) -> 
    match get_cell grid (x + dx) (y + dy) with
    | Alive -> acc + 1
    | Dead -> acc
  ) 0 directions

let compute_next grid =
  List.mapi (fun x row ->
    List.mapi (fun y cell ->
      match cell, count_alive_neighbors grid x y with
      | Alive, 2 | Alive, 3 | Dead, 3 -> Alive
      | _ -> Dead
    ) row
  ) grid

let display_grid grid =
  List.iter (fun row ->
      List.iter (function
          | Dead -> print_char '.'
          | Alive -> print_char '#')
        row;
      print_newline ())
    grid

let set_random_alive grid =
  List.mapi (fun i row ->
    List.mapi (fun j _ ->
      if Random.bool () then Alive else Dead
    ) row
  ) grid

let main () =
  Random.self_init ();
  let empty_row = List.init 20 (fun _ -> Dead) in
  let grid_ref = ref (List.init 20 (fun _ -> empty_row)) in
  
  grid_ref := set_random_alive !grid_ref;

  let generations = 20 in
  for _ = 1 to generations do
    display_grid !grid_ref;
    print_newline ();
    grid_ref := compute_next !grid_ref;
  done

let () = main ()
