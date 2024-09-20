open Board

type state =
  | Waiting
  | Game_Over
  | Roll

type l =
  | Buy of bool
  | Pay of int

type location = int * int

type player = {
  name : string;
  player_number : int;
  mutable balance : int;
  mutable location : location;
  mutable state : state;
  mutable jail_turns : int;
}

type players = player list

let new_player name num =
  {
    name;
    player_number = num;
    balance = 1500;
    location = (10, 10);
    state = Waiting;
    jail_turns = 0;
  }

let get_player_name p = p.name
let get_player_money p = p.balance
let get_player_location p = p.location
let get_player_state p = p.state

let pay_player amount payer receiver =
  if payer.balance >= amount then
    ( { payer with balance = payer.balance - amount },
      { receiver with balance = receiver.balance + amount } )
  else
    ({ payer with state = Game_Over }, { receiver with state = Game_Over })

let collect_money amount p = { p with balance = p.balance + amount }

let go_jail p =
  p.location <- (10, 0);
  p.jail_turns <- 3

let roll_dice () = 1 + Random.int 6

let get_player_index_name (index : int) players : string =
  (List.nth players index).name

let get_player_index_location (index : int) players : location =
  (List.nth players index).location

let get_player (index : int) players : player = List.nth players index

let move_counter_clockwise (row, col) =
  if row = 0 && col < 10 then (row, col + 1) (* Move right along the top row *)
  else if col = 10 && row < 10 then (row + 1, col)
    (* Move down along the rightmost column *)
  else if row = 10 && col > 0 then (row, col - 1)
    (* Move left along the bottom row *)
  else if col = 0 && row > 0 then (row - 1, col)
    (* Move up along the leftmost column *)
  else (row, col)
(* Do not move if not on the perimeter, this would mean the player is not on the
   actual board *)

let move_multiple_moves current_location num_moves =
  let rec move_helper location moves_remaining =
    if moves_remaining = 0 then location
    else
      let new_location = move_counter_clockwise location in
      move_helper new_location (moves_remaining - 1)
  in
  move_helper current_location num_moves

let get_property_name (b : board) (location : location) =
  let row, col = location in
  let property1 = b.(row).(col) in
  match property1.name with
  | Some name -> name
  | None -> "none"

let update_player_location p num_moves =
  let curr = p.location in
  p.location <- move_multiple_moves curr num_moves

let is_bought (l : location) (b : board) : bool =
  let row, col = l in
  let property1 = b.(row).(col) in
  match property1.owner with
  | Some _ -> true
  | None -> false

let get_location_property (l : location) (b : board) : property =
  let row, col = l in
  b.(row).(col)

let check_negative (p : player) : bool = if p.balance <= 0 then true else false
let change_state (p : player) = p.state <- Game_Over
let keep_state (p : player) = p.state <- p.state

let property_action_buy (p : player) (l : location) (b : board) : unit =
  let change_property () =
    let row, col = l in
    b.(row).(col) <- { (get_location_property l b) with owner = Some p.name }
  in
  let row, col = l in
  let property1 = b.(row).(col) in
  match property1.price with
  | Some price ->
      if check_negative p then change_state p else keep_state p;
      change_property ();
      p.balance <- p.balance - price
  | None -> ()

let property_action_rent (p_owner : player) (p_payer : player) (l : location)
    (b : board) : unit =
  let row, col = l in
  let property1 = b.(row).(col) in
  let price = property_price_number property1 in
  match property1.owner with
  | Some _ ->
      p_payer.balance <-
        p_payer.balance
        - (price * 1 / 4)
        - (price * property1.houses * 1 / 5)
        - (price * property1.hotel);
      if check_negative p_payer then change_state p_payer
      else keep_state p_payer;
      p_owner.balance <-
        p_owner.balance
        + (price * 1 / 4)
        + (price * property1.houses * 1 / 5)
        + (price * property1.hotel)
  | None -> ()

let rec fold_left_custom f acc lst =
  match lst with
  | [] -> acc
  | hd :: tl -> fold_left_custom f (f acc hd) tl

let compare_balance p1 p2 = if p1.balance > p2.balance then p1 else p2

let get_winner (p1 : player) (p2 : player) (p3 : player) (p4 : player) : player
    =
  let players_list = [ p2; p3; p4 ] in
  let max_balance_player = fold_left_custom compare_balance p1 players_list in
  max_balance_player

type player_statistics = {
  player : player;
  mutable properties_owned : string list;
}

let intialize_player_statistics player = { player; properties_owned = [] }

let update_player_statistics (stats : player_statistics) (property : property) =
  match property.owner with
  | Some _ ->
      let property_name = string_option_to_string (property_name property) in
      stats.properties_owned <- property_name :: stats.properties_owned
  | None -> ()

let create_property_statistics board player_stats =
  for i = 0 to Array.length board - 1 do
    for j = 0 to Array.length board.(i) - 1 do
      match board.(i).(j).owner with
      | Some owner ->
          if owner = get_player_name player_stats.player then
            update_player_statistics player_stats board.(i).(j)
      | None -> ()
    done
  done
