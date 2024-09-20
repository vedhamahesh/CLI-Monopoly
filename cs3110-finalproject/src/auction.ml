open Player
open Board

type auction_bid = { bidder: player; amount: int }

type auction_state = {
  mutable highest_bid: auction_bid option;
  mutable active_bidders: player list;
}

let start_auction  (players: player list) (l : location) (b: board) =
  print_endline ("Player has chosen not to buy property, the property is up for auction");
  let state = { highest_bid = None; active_bidders = players } in
  let rec auction_round (p : player list) =
    match p with
    | [] | [_] -> ()
    | bidder :: rest ->
      print_endline (bidder.name ^ ", enter your bid amount or [pass] to pass:");
      let input = read_line () in
      if input = "pass" then
        auction_round(rest)
      else
        let bid_amount = int_of_string input in
        match state.highest_bid with
        | Some { amount = current_amount; _ } when bid_amount <= current_amount ->
          print_endline "Bid must be higher than the current highest bid.";
          auction_round (bidder :: rest)
        | _ ->
          state.highest_bid <- Some { bidder; amount = bid_amount };
          auction_round (rest @ [bidder])
  in
  auction_round state.active_bidders;
  match state.highest_bid with
  | Some { bidder; amount } ->
    print_endline (bidder.name ^ " wins the auction with a bid of $" ^ string_of_int amount);
    bidder.balance <- bidder.balance - amount + 
    int_option_to_int(property_price(get_location_property l b));
    property_action_buy bidder l b
  | None ->
    print_endline "No one participated in the auction. The property remains unowned."

let property_action_buy_or_auction (current_player: player) 
(players: player list) (location: location) (board: board) =
  let property = get_location_property location board in
  print_endline ("This property is available for purchase: " ^ 
    string_option_to_string(property_name property));
  print_endline ("Price: $" ^ string_of_int(int_option_to_int(property_price property)));
  print_endline ("Type [buy] to buy the property or [pass (or any keyword)] to start an auction.");
  let input = read_line () in
  if input = "buy" then
    property_action_buy current_player location board
  else
    start_auction players location board



let build_house player prop =
  if prop.owner <> None && prop.houses < 4 && prop.hotel <> 1 then (
    prop.houses <- prop.houses + 1;
    player.balance <- player.balance - 100;
    print_endline (string_option_to_string prop.name ^ " now has " ^ string_of_int prop.houses ^ " houses.");
  ) else if prop.hotel = 1 then (
    print_endline (string_option_to_string prop.name ^ " already has a hotel.");
  ) else (
    print_endline "Cannot build more houses on this property.";
  )

let build_hotel player prop =
  if prop.owner <> None && prop.houses = 4 && prop.hotel <> 1 then (
    prop.houses <- 0;  
    prop.hotel <- 1;
    player.balance <- player.balance - 200;
    print_endline (string_option_to_string prop.name ^ " now has a hotel.");
  ) else if prop.hotel = 1 then (
    print_endline (string_option_to_string prop.name ^ " already has a hotel.");
  ) else (
    print_endline "Cannot build a hotel on this property.";
  )

let find_property_house player find_property board = 
  for i = 0 to Array.length board - 1 do
    for j = 0 to Array.length board.(i) - 1 do
      match board.(i).(j) with
      | property ->
        if property_name property = find_property
          then build_house player property
    done
  done

let find_property_hotel player find_property board = 
  for i = 0 to Array.length board - 1 do
    for j = 0 to Array.length board.(i) - 1 do
      match board.(i).(j) with
      | property ->
        if property_name property = find_property
          then build_hotel player property
    done
  done

let rec build player build_property board = 
  print_endline ("Would you like to build more on " ^ build_property ^ "?");
  print_endline "[1] Build House";
  print_endline "[2] Build Hotel";
  print_endline "[3 (or any other keywod)] Finish building";
  let build_choice = read_line () in
  match build_choice with
  | "1" -> find_property_house player (Some build_property) board; build player build_property board;
  | "2" -> find_property_hotel player (Some build_property) board; build player build_property board;
  | _ -> print_endline "Finished building";
