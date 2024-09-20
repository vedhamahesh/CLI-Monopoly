open Player

type chance = {
  description : string;
  action : player -> unit;
}

type community_chest = {
  description : string;
  benefit : player -> unit;
}


let draw_chance () : chance =
  let chance_cards =
    [
      {
        description = "Return to the start (Advance to Go)";
        action = (fun p -> p.location <- (0, 0));
      };
      { 
        description = "Go directly to Jail."; 
        action = (fun p -> go_jail p) 
      };
      {
        description = "Advance to Ill Ave";
        action = (fun p -> p.location <- (0, 4));
      };
      {
        description = "Advance to Park Pl";
        action = (fun p -> p.location <- (7, 10));
      };
      {
        description = "Take a walk on the Boardwalk (Advance to Brdwalk)";
        action = (fun p -> p.location <- (9, 10));
      };
      {
        description = "Pay Poor Tax of $15";
        action = (fun p -> p.balance <- p.balance - 15);
      };
      {
        description = "Your Building and Loan matures: collect $150";
        action = (fun p -> p.balance <- p.balance + 150);
      };
      {
        description = "Bank pays you dividend of $50";
        action = (fun p -> p.balance <- p.balance + 50);
      };
      {
        description = "Speeding fine of $50";
        action = (fun p -> p.balance <- p.balance - 50);
      };
      {
        description = "You won a crossword puzzle competition!";
        action = (fun p -> p.balance <- p.balance + 100);
      };
      {
        description = "Advance to Atl Ave";
        action = (fun p -> p.location <- (0, 6));
      };
      {
        description = "Receive for wedding services";
        action = (fun p -> p.balance <- p.balance + 25);
      };
      {
        description = "A friend drives you around, and parks in free parking";
        action = (fun p -> p.location <- (0,0));
      };
      {
        description = "Advance to S James";
        action = (fun p -> p.location <- (4,0));
      };
      {
        description = "You find a 10 dollar bill lying on floor";
        action = (fun p -> p.balance <- p.balance + 10);
      };
      {
        description = "Pay street repairs";
        action = (fun p -> p.balance <- p.balance - 150);
      };
    ]
  in
  List.nth chance_cards (Random.int (List.length chance_cards))

let draw_community () : community_chest =
  let community_cards =
    [
      {
        description = "Bank Error in your favor: collect $200";
        benefit = (fun p -> p.balance <- p.balance + 200);
      };
      {
        description = "XMAS fund matures";
        benefit = (fun p -> p.balance <- p.balance + 100);
      };
      {
        description = "Inheritance: Receive $160";
        benefit = (fun p -> p.balance <- p.balance + 160);
      };
      {
        description = "Income Tax Refund";
        benefit = (fun p -> p.balance <- p.balance + 20);
      };
      {
        description = "Life Insurance matures: collect $80";
        benefit = (fun p -> p.balance <- p.balance + 80);
      };
      {
        description = "Receive for services";
        benefit = (fun p -> p.balance <- p.balance + 25);
      };
      {
        description = "Doctor's fee";
        benefit = (fun p -> p.balance <- p.balance - 50);
      };
      {
        description = "Second prize in beauty contest";
        benefit = (fun p -> p.balance <- p.balance + 120);
      };
      {
        description = "It's your birthday!";
        benefit = (fun p -> p.balance <- p.balance + 40);
      };
      {
        description = "School fees";
        benefit = (fun p -> p.balance <- p.balance - 60);
      };
      {
        description = "Stock selling";
        benefit = (fun p -> p.balance <- p.balance + 50);
      };
      {
        description = "Organized bake sale";
        benefit = (fun p -> p.balance <- p.balance + 35);
      };
      {
        description = "You found a wallet and returned it. The person was very thankful!";
        benefit = (fun p -> p.balance <- p.balance + 60);
      };
      {
        description = "Covering dinner for friends";
        benefit = (fun p -> p.balance <- p.balance - 40);
      };
      {
        description = "Donated to dog shelter";
        benefit = (fun p -> p.balance <- p.balance - 25);
      };
      { 
        description = "Consultancy fee"; 
        benefit = (fun p -> p.balance <- p.balance - 35); 
      }; 
    ]
  in
  List.nth community_cards (Random.int (List.length community_cards))

let handle_chance (p : player) : unit =
  let card = draw_chance () in
  Printf.printf "Chance Card: %s\n" card.description;
  card.action p

let handle_community (p : player) : unit =
  let community = draw_community () in
  Printf.printf "Community Card: %s\n" community.description;
  community.benefit p

let handle_inc (p : player) : unit = p.balance <- p.balance - 200
let handle_lux (p : player) : unit = p.balance <- p.balance - 100

let visiting_jail (p : player) : unit =
  print_endline (p.name ^ " is currently visiting jail")

let returned_to_go (p : player) : unit =
  print_endline (p.name ^ " has returned to the starting square (go)")

let free_parking (p : player) : unit =
  print_endline (p.name ^ "is at a free parking lot")

let handle_jail_doubles (p : player) : unit =
  let roll1 = roll_dice () in
  let roll2 = roll_dice () in
  Printf.printf "Player %s rolled %d and %d\n" p.name roll1 roll2;
  if roll1 = roll2 then (
    Printf.printf "Player %s rolled doubles and gets out of jail!\n" p.name;
    p.jail_turns <- 0)
  else (
    Printf.printf "Player %s failed to roll doubles\n" p.name;
    p.jail_turns <- p.jail_turns - 1;
    if p.jail_turns = 0 then
      print_endline (p.name ^ " has spent three turns in jail\n"))

let handle_jail (p : player) : unit =
  Printf.printf "Player %s, choose an option:\n" p.name;
  Printf.printf "Type [1] for Option 1. Roll a pair of dice for doubles\n";
  Printf.printf "Type [2] for Option 2. Pay $200 fine\n";
  Printf.printf
    "Type [3 (or any other keyword)] for Option 3. Choose to wait in jail";
  let choice = read_line () in
  match choice with
  | "1" -> handle_jail_doubles p
  | "2" ->
      p.balance <- p.balance - 200;
      p.jail_turns <- 0
  | _ ->
      p.jail_turns <- p.jail_turns - 1;
      if p.jail_turns = 0 then
        print_endline (p.name ^ " has spent three turns in jail\n")
