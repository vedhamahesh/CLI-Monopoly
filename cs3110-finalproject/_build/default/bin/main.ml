open Monopoly.Board
open Monopoly.Player
open Monopoly.Special
open Monopoly.Auction

let play () =
  let board = initialize_board () in
  let get_names () =
    print_endline "What is Player 1's name?";
    let player1 : player =
      {
        name = read_line ();
        player_number = 1;
        balance = 1500;
        location = (10, 10);
        state = Waiting;
        jail_turns = 0;
      }
    in
    print_endline "What is Player 2's name?";
    let player2 : player =
      {
        name = read_line ();
        player_number = 2;
        balance = 1500;
        location = (10, 10);
        state = Waiting;
        jail_turns = 0;
      }
    in
    print_endline "What is Player 3's name?";
    let player3 : player =
      {
        name = read_line ();
        player_number = 3;
        balance = 1500;
        location = (10, 10);
        state = Waiting;
        jail_turns = 0;
      }
    in
    print_endline "What is Player 4's name?";
    let player4 : player =
      {
        name = read_line ();
        player_number = 4;
        balance = 1500;
        location = (10, 10);
        state = Waiting;
        jail_turns = 0;
      }
    in
    let players_list = [ player1; player2; player3; player4 ] in
    players_list
  in

  let players_list = get_names () in

  while not (List.exists (fun p -> p.balance <= 0) players_list) do
    let game_board = initialize_board () in
    for n = 1 to List.length players_list do
      let current = get_player_index_name (n - 1) players_list in
      let curr_player = get_player (n - 1) players_list in
      if curr_player.jail_turns > 0 then handle_jail curr_player
      else
        let curr_stats = intialize_player_statistics curr_player in
        create_property_statistics board curr_stats;
        print_endline
          ("It's " ^ current
         ^ "'s turn, if you wish to build houses or hotels type [build] now.");
        print_endline
          "If you do not own any properties or choose not to build, type [skip \
           (or any other keyword)]";
        let did_build = read_line () = "build" in
        if did_build then (
          print_endline
            ("Properties owned by " ^ current ^ ": "
            ^ String.concat "; " curr_stats.properties_owned);
          print_endline "Which property would you like to build on?";
          print_endline
            "Type property exactly as you see it or you will not be able to \
             build";
          let build_property = read_line () in
          if List.mem build_property curr_stats.properties_owned then
            build curr_player build_property board
          else print_endline "You do not own this property.")
        else print_endline (current ^ " chose not to build");
        print_endline
          ("It's " ^ current ^ "'s turn, please type [roll] to roll die.");
        let did_roll = read_line () = "roll" in
        if did_roll then (
          let rolled = roll_dice () in
          print_board game_board;
          print_string (current ^ " rolled " ^ string_of_int rolled ^ ".");
          print_endline "";
          let curr_player = get_player (n - 1) players_list in
          update_player_location curr_player rolled;
          print_string
            (current ^ " is now at this property: "
            ^ get_property_name board curr_player.location);
          print_endline "";
          let curr_location = curr_player.location in
          if
            get_location_property curr_location board
            |> property_name = Some "Chance "
          then handle_chance curr_player
          else if
            get_location_property curr_location board
            |> property_name = Some "C Chest"
          then handle_community curr_player
          else if
            get_location_property curr_location board
            |> property_name = Some "Inc Tax"
          then handle_inc curr_player
          else if
            get_location_property curr_location board
            |> property_name = Some "Lux Tax"
          then handle_lux curr_player
          else if
            get_location_property curr_location board
            |> property_name = Some "Fr Park"
          then free_parking curr_player
          else if
            get_location_property curr_location board
            |> property_name = Some "In Jail"
          then visiting_jail curr_player
          else if
            get_location_property curr_location board
            |> property_name = Some "  Go  "
          then returned_to_go curr_player
          else if
            get_location_property curr_location board
            |> property_name = Some "Go Jail"
          then go_jail curr_player
          else if is_bought curr_location board then (
            let player1name = get_player_index_name 0 players_list in
            let player2name = get_player_index_name 1 players_list in
            let player3name = get_player_index_name 2 players_list in

            let powner =
              match (get_location_property curr_location board).owner with
              | Some a -> a
              | None -> "No owner"
            in
            if powner = player1name then
              property_action_rent
                (get_player 0 players_list)
                curr_player curr_location board
            else if powner = player2name then
              property_action_rent
                (get_player 1 players_list)
                curr_player curr_location board
            else if powner = player3name then
              property_action_rent
                (get_player 2 players_list)
                curr_player curr_location board
            else
              property_action_rent
                (get_player 3 players_list)
                curr_player curr_location board;

            print_endline
              (curr_player.name ^ " landed on an owned property and paid rent."))
          else
            property_action_buy_or_auction curr_player players_list
              curr_location board;

          print_string
            (current ^ "'s balance is " ^ string_of_int curr_player.balance);
          print_endline "")
        else
          print_string
            ("Entered command [roll] incorrectly, sorry " ^ current
           ^ " lost their turn")
    done
  done;
  let game_over_board = initialize_board () in
  print_game_over_board game_over_board;
  print_endline
    ("The winner is "
   ^ (get_winner
        (get_player 0 players_list)
        (get_player 1 players_list)
        (get_player 2 players_list)
        (get_player 3 players_list))
       .name);
  print_endline
    ((get_player 0 players_list).name ^ " finished with "
    ^ string_of_int (get_player 0 players_list).balance
    ^ "$");
  print_endline
    ((get_player 1 players_list).name ^ " finished with "
    ^ string_of_int (get_player 1 players_list).balance
    ^ "$");
  print_endline
    ((get_player 2 players_list).name ^ " finished with "
    ^ string_of_int (get_player 2 players_list).balance
    ^ "$");
  print_endline
    ((get_player 3 players_list).name ^ " finished with "
    ^ string_of_int (get_player 3 players_list).balance
    ^ "$");
  let p1 = get_player 0 players_list in
  let p2 = get_player 1 players_list in
  let p3 = get_player 2 players_list in
  let p4 = get_player 3 players_list in

  let player1_stats = intialize_player_statistics p1 in
  let player2_stats = intialize_player_statistics p2 in
  let player3_stats = intialize_player_statistics p3 in
  let player4_stats = intialize_player_statistics p4 in
  create_property_statistics board player1_stats;
  create_property_statistics board player2_stats;
  create_property_statistics board player3_stats;
  create_property_statistics board player4_stats;
  let stat1 = String.concat "; " player1_stats.properties_owned in
  print_endline ("Properties owned by " ^ p1.name ^ ": " ^ stat1);
  let stat2 = String.concat "; " player2_stats.properties_owned in
  print_endline ("Properties owned by " ^ p2.name ^ ": " ^ stat2);
  let stat3 = String.concat "; " player3_stats.properties_owned in
  print_endline ("Properties owned by " ^ p3.name ^ ": " ^ stat3);
  let stat4 = String.concat "; " player4_stats.properties_owned in
  print_endline ("Properties owned by " ^ p4.name ^ ": " ^ stat4)

let () =
  print_endline "Welcome to Monopoly!";
  play ();
  print_endline ""
