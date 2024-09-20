type property = {
  name : string option;
  price : int option;
  owner : string option;
  mutable houses : int;
  mutable hotel : int;
}

type board = property array array

let create_property name price = { name; price; owner = None; houses = 0; hotel = 0}

let property_name prop = prop.name

let string_option_to_string opt =
  match opt with
  | Some str -> str
  | None -> ""

let property_price prop = prop.price

let int_option_to_int opt =
  match opt with
  | Some x ->  x
  | None -> 0

let property_price_number prop = match prop.price with
|Some x -> x
|None -> 0

let property_owner prop = prop.owner

let create_board rows cols =
  Array.init rows (fun _ ->
      Array.init cols (fun _ -> create_property None None))

let initialize_board () =
  let board = create_board 11 11 in
  (* Row #1 *)
  board.(0).(0) <- create_property (Some "Fr Park") None;
  board.(0).(1) <- create_property (Some "Ken Ave") (Some 220);
  board.(0).(2) <- create_property (Some "Chance ") None;
  board.(0).(3) <- create_property (Some "Ind Ave") (Some 220);
  board.(0).(4) <- create_property (Some "Ill Ave") (Some 240);
  board.(0).(5) <- create_property (Some "B&O RR ") (Some 200);
  board.(0).(6) <- create_property (Some "Atl Ave") (Some 260);
  board.(0).(7) <- create_property (Some "Ven Ave") (Some 260);
  board.(0).(8) <- create_property (Some "WWorks ") (Some 150);
  board.(0).(9) <- create_property (Some "Mar Gar") (Some 280);
  board.(0).(10) <- create_property (Some "Go Jail") None;

  (* Row #2 *)
  board.(1).(0) <- create_property (Some "NY Ave ") (Some 200);
  board.(1).(1) <- create_property None None;
  board.(1).(2) <- create_property None None;
  board.(1).(3) <- create_property None None;
  board.(1).(4) <- create_property None None;
  board.(1).(5) <- create_property None None;
  board.(1).(6) <- create_property None None;
  board.(1).(7) <- create_property None None;
  board.(1).(8) <- create_property None None;
  board.(1).(9) <- create_property None None;
  board.(1).(10) <- create_property (Some "Pac Ave") (Some 300);

  (* Row #3 *)
  board.(2).(0) <- create_property (Some "Ten Ave ") (Some 180);
  board.(2).(1) <- create_property None None;
  board.(2).(2) <- create_property None None;
  board.(2).(3) <- create_property None None;
  board.(2).(4) <- create_property None None;
  board.(2).(5) <- create_property None None;
  board.(2).(6) <- create_property None None;
  board.(2).(7) <- create_property None None;
  board.(2).(8) <- create_property None None;
  board.(2).(9) <- create_property None None;
  board.(2).(10) <- create_property (Some "NC Ave ") (Some 300);

  (* Row #4 *)
  board.(3).(0) <- create_property (Some "C Chest") None;
  board.(3).(1) <- create_property None None;
  board.(3).(2) <- create_property None None;
  board.(3).(3) <- create_property None None;
  board.(3).(4) <- create_property None None;
  board.(3).(5) <- create_property None None;
  board.(3).(6) <- create_property None None;
  board.(3).(7) <- create_property None None;
  board.(3).(8) <- create_property None None;
  board.(3).(9) <- create_property None None;
  board.(3).(10) <- create_property (Some "C Chest") None;

  (* Row #5 *)
  board.(4).(0) <- create_property (Some "S James") (Some 180);
  board.(4).(1) <- create_property None None;
  board.(4).(2) <- create_property None None;
  board.(4).(3) <- create_property None None;
  board.(4).(4) <- create_property None None;
  board.(4).(5) <- create_property None None;
  board.(4).(6) <- create_property None None;
  board.(4).(7) <- create_property None None;
  board.(4).(8) <- create_property None None;
  board.(4).(9) <- create_property None None;
  board.(4).(10) <- create_property (Some "Pen Ave ") (Some 320);

  (* Row #6 *)
  board.(5).(0) <- create_property (Some "P RRoad") (Some 200);
  board.(5).(1) <- create_property None None;
  board.(5).(2) <- create_property None None;
  board.(5).(3) <- create_property None None;
  board.(5).(4) <- create_property None None;
  board.(5).(5) <- create_property None None;
  board.(5).(6) <- create_property None None;
  board.(5).(7) <- create_property None None;
  board.(5).(8) <- create_property None None;
  board.(5).(9) <- create_property None None;
  board.(5).(10) <- create_property (Some "Short L") (Some 200);

  (* Row #7 *)
  board.(6).(0) <- create_property (Some "Vir Ave") (Some 160);
  board.(6).(1) <- create_property None None;
  board.(6).(2) <- create_property None None;
  board.(6).(3) <- create_property None None;
  board.(6).(4) <- create_property None None;
  board.(6).(5) <- create_property None None;
  board.(6).(6) <- create_property None None;
  board.(6).(7) <- create_property None None;
  board.(6).(8) <- create_property None None;
  board.(6).(9) <- create_property None None;
  board.(6).(10) <- create_property (Some "Chance ") None;

  (* Row #8 *)
  board.(7).(0) <- create_property (Some "Sta Ave") (Some 140);
  board.(7).(1) <- create_property None None;
  board.(7).(2) <- create_property None None;
  board.(7).(3) <- create_property None None;
  board.(7).(4) <- create_property None None;
  board.(7).(5) <- create_property None None;
  board.(7).(6) <- create_property None None;
  board.(7).(7) <- create_property None None;
  board.(7).(8) <- create_property None None;
  board.(7).(9) <- create_property None None;
  board.(7).(10) <- create_property (Some "Park Pl") (Some 350);

  (* Row #9 *)
  board.(8).(0) <- create_property (Some "Ele Com") (Some 150);
  board.(8).(1) <- create_property None None;
  board.(8).(2) <- create_property None None;
  board.(8).(3) <- create_property None None;
  board.(8).(4) <- create_property None None;
  board.(8).(5) <- create_property None None;
  board.(8).(6) <- create_property None None;
  board.(8).(7) <- create_property None None;
  board.(8).(8) <- create_property None None;
  board.(8).(9) <- create_property None None;
  board.(8).(10) <- create_property (Some "Lux Tax") None;

  (* Row #10 *)
  board.(9).(0) <- create_property (Some "Elt Com") (Some 150);
  board.(9).(1) <- create_property None None;
  board.(9).(2) <- create_property None None;
  board.(8).(3) <- create_property None None;
  board.(9).(4) <- create_property None None;
  board.(9).(5) <- create_property None None;
  board.(9).(6) <- create_property None None;
  board.(9).(7) <- create_property None None;
  board.(9).(8) <- create_property None None;
  board.(9).(9) <- create_property None None;
  board.(9).(10) <- create_property (Some "Brdwalk") (Some 400);

  (* Row #11 *)
  board.(10).(0) <- create_property (Some "In Jail") None;
  board.(10).(1) <- create_property (Some "Con Ave") (Some 120);
  board.(10).(2) <- create_property (Some "Ver Ave") (Some 100);
  board.(10).(3) <- create_property (Some "Chance ") None;
  board.(10).(4) <- create_property (Some "Ori Ave") (Some 100);
  board.(10).(5) <- create_property (Some "R RRoad") (Some 200);
  board.(10).(6) <- create_property (Some "Inc Tax") None;
  board.(10).(7) <- create_property (Some "Bal Ave") (Some 60);
  board.(10).(8) <- create_property (Some "C Chest") None;
  board.(10).(9) <- create_property (Some "Med Ave") (Some 60);
  board.(10).(10) <- create_property (Some "  Go  ") None;

  board

let get_property1 board row col = board.(row).(col)

let set_property_owner board row col owner =
  let property = get_property1 board row col in
  board.(row).(col) <- { property with owner = Some owner }

let draw_property_box name =
  match name with
  | Some a ->
      let name_length = String.length a in
      let total_width = 8 in
      let remaining_width = total_width - name_length in
      Printf.printf "|%s%*s|" a remaining_width ""
  | None ->
      let total_width = 10 in
      Printf.printf "%*s%*s" (total_width / 2) "" (total_width / 2) ""

let print_board board =
  let rows = Array.length board in
  let cols = Array.length board.(0) in
  print_string
    " ________  ________  ________  ________  ________  ________  ________  \
     ________  ________  ________  ________ ";
  print_endline "";
  for row = 0 to rows - 1 do
    if row = 10 then (
      print_string
        " ________  ________  ________  ________  ________  ________  \
         ________  ________  ________  ________  ________ ";
      print_endline "";
      for col = 0 to cols - 1 do
        let property = board.(row).(col) in
        draw_property_box property.name
      done)
    else
      for col = 0 to cols - 1 do
        let property = board.(row).(col) in
        draw_property_box property.name
      done;
    if row = 0 then (
      print_endline "";
      print_string
        "|        ||        ||        ||        ||        ||        ||        \
         ||        ||        ||        ||        |";
      print_endline "";
      print_string
        "|________||________||________||________||________||________||________||________||________||________||________|";
      print_endline "";
      print_string
        " \
         ________                                                                                            \
         ________ ";
      print_endline "")
    else if row = 10 then (
      print_endline "";
      print_string
        "|        ||        ||        ||        ||        ||        ||        \
         ||        ||        ||        ||        |";
      print_endline "";
      print_string
        "|________||________||________||________||________||________||________||________||________||________||________|";
      print_endline "";
      print_endline "";
      print_string "PLAYER STATUS";
      print_string "";
      print_endline "")
    else if row = 9 then (
      print_endline "";
      print_string
        "|        \
         |                                                                                          \
         |        |";
      print_endline "";
      print_string
        "|________|                                                                                          \
         |________|";
      print_endline "")
    else if row = 5 then (
      print_endline "";
      print_string
        "|        |                                         \
         MONOPOLY                                         |        |";
      print_endline "";
      print_string
        "|________|                                                                                          \
         |________|";
      print_endline "";
      print_string
        " \
         ________                                                                                            \
         ________ ";
      print_endline "")
    else (
      print_endline "";
      print_string
        "|        \
         |                                                                                          \
         |        |";
      print_endline "";
      print_string
        "|________|                                                                                          \
         |________|";
      print_endline "";
      print_string
        " \
         ________                                                                                            \
         ________ ";
      print_endline "")
  done

let print_game_over_board board =
  let rows = Array.length board in
  let cols = Array.length board.(0) in
  print_string
    " ________  ________  ________  ________  ________  ________  ________  \
     ________  ________  ________  ________ ";
  print_endline "";
  for row = 0 to rows - 1 do
    if row = 10 then (
      print_string
        " ________  ________  ________  ________  ________  ________  \
         ________  ________  ________  ________  ________ ";
      print_endline "";
      for col = 0 to cols - 1 do
        let property = board.(row).(col) in
        draw_property_box property.name
      done)
    else
      for col = 0 to cols - 1 do
        let property = board.(row).(col) in
        draw_property_box property.name
      done;
    if row = 0 then (
      print_endline "";
      print_string
        "|        ||        ||        ||        ||        ||        ||        \
         ||        ||        ||        ||        |";
      print_endline "";
      print_string
        "|________||________||________||________||________||________||________||________||________||________||________|";
      print_endline "";
      print_string
        " \
         ________                                                                                            \
         ________ ";
      print_endline "")
    else if row = 10 then (
      print_endline "";
      print_string
        "|        ||        ||        ||        ||        ||        ||        \
         ||        ||        ||        ||        |";
      print_endline "";
      print_string
        "|________||________||________||________||________||________||________||________||________||________||________|";
      print_endline "";
      print_endline "";
      print_string "PLAYER STATUS";
      print_string "";
      print_endline "")
    else if row = 9 then (
      print_endline "";
      print_string
        "|        \
         |                                                                                          \
         |        |";
      print_endline "";
      print_string
        "|________|                                                                                          \
         |________|";
      print_endline "")
    else if row = 5 then (
      print_endline "";
      print_string
        "|        |                                         GAME \
         OVER!                                       |        |";
      print_endline "";
      print_string
        "|________|                                                                                          \
         |________|";
      print_endline "";
      print_string
        " \
         ________                                                                                            \
         ________ ";
      print_endline "")
    else (
      print_endline "";
      print_string
        "|        \
         |                                                                                          \
         |        |";
      print_endline "";
      print_string
        "|________|                                                                                          \
         |________|";
      print_endline "";
      print_string
        " \
         ________                                                                                            \
         ________ ";
      print_endline "")
  done

