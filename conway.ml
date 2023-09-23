type cell = Dead | Alive
type grid = cell array array

let get_cell grid x y =
  if x < 0 || x >= Array.length grid || y < 0 || y >= Array.length grid.(0) then
    Dead
  else
    grid.(x).(y)

let count_alive_neighbors grid x y =
  let dx = [|-1; -1; -1;  0; 0;  1; 1; 1|] in
  let dy = [|-1;  0;  1; -1; 1; -1; 0; 1|] in
  let count = ref 0 in
  for i = 0 to 7 do
    match get_cell grid (x + dx.(i)) (y + dy.(i)) with
    | Alive -> incr count
    | Dead -> ()
  done;
  !count

let compute_next grid =
  let new_grid = Array.make_matrix (Array.length grid) (Array.length grid.(0)) Dead in
  for x = 0 to Array.length grid - 1 do
    for y = 0 to Array.length grid.(0) - 1 do
      match grid.(x).(y), count_alive_neighbors grid x y with
      | Alive, 2
      | Alive, 3
      | Dead,  3 -> new_grid.(x).(y) <- Alive
      | _ -> new_grid.(x).(y) <- Dead
    done
  done;
  new_grid

let display_grid grid =
  Array.iter (fun row ->
      Array.iter (function
          | Dead -> print_char '.'
          | Alive -> print_char '#')
        row;
      print_newline ())
    grid

let main () =
  let grid = 
    Array.make_matrix 10 10 Dead in

  
  grid.(1).(2) <- Alive;
  grid.(2).(3) <- Alive;
  grid.(3).(1) <- Alive;
  grid.(3).(2) <- Alive;
  grid.(3).(3) <- Alive;

  let generations = 10 in
  for _ = 1 to generations do
    display_grid grid;
    print_newline ();
    let new_grid = compute_next grid in
    Array.blit new_grid 0 grid 0 (Array.length new_grid)
  done

let () = main ()
        

