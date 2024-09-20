(* Test Plan:

   Here is the comprehensive test file for our take on the popular board game
   Monopoly! In this comment, we hope to clear up any confusion, answer
   questions, and provide complete context to the code below.

   Firstly, an overview on what each testing module does:

   Test_Board Module:

   - Tests game board initialization using Test_Board.initialize_board.

   - Checks creation of properties using Test_Board.create_property.

   - Checks getting properties from the board using Test_Board.get_property1.

   - Checks that several other functions like property_name, property_owner, and
   property_price return intended values.

   Test_Player Module:

   - Tests player creation with different names and initial states.

   - Checks intended output of player-related functions like get_player_name,
   get_player_money, get_player_location, and get_player_state.

   - Checks players' ability to pay and receive money using the pay_player and
   collect_money functions.

   - Checks for players transitioning throughout the board correctly using
   functions like go_jail, roll_dice, and move_counter_clockwise.

   - Check that property actions, such as is_bought, get_location_property, and
   property_action_buy/rent return intended output.

   - Check player comparison in functions like compare_balance and get_winner.

   Test_Special Module:

   - Check that retrieval of chance/community chest cards works as intended. In
   particular, looks for correct location updating or player balance changes as
   stated by the cards.

   - Verifies that Luxury Tax and Income Tax spots affect player balance.

   Test_Auction Module:

   - A lot of functions in this module were pretty untestable since they returned
   type unit and relied on user inputfor the functions to work, 
   which means a person would have to be constantly typing in inputs to test the functions

   - As a result we mostly tested build_house, build_hotel, find_property_house
   and find_property_hotel as they were the only ones verifiable with OUnit and did
   not rely on user input

   -We also test property_action_rent again in this module to see how the rent changes
   when we build houses and hotels on those properties since the have a multiplier effect
   –––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––

   Now, we'll dive more into testing methods and where they apply, as well as
   clarify where we employed OUnit vs manual testing.

   Testing Approach:

   - A majority, of our testing approach was Glass Box testing. We first
   designed complete implementations of functions within each module. Then, we
   employed testing based on the paths available in the code of each function,
   making use of edge cases.

   - Randomized testing is utilized for functions that involve chance cards and
   community cards to ensure robustness.

   OUnit Testing:

   - The test suite utilizes OUnit for automated testing.

   - OUnit is employed for both unit testing of individual functions and
   integration testing of the overall system. In fact, every single one of our
   modules' tests are through OUnit.
   –––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––

   Limitations/Potential Omits:

   - Due to how complex Monopoly can get, it might be challenging to cover all
   possible scenarios exhaustively.

   - Some functions could depend on external factors, making it difficult to
   isolate and test certain behaviors.
   –––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––

   Some lasting thoughts:

   Overall, this test suite provides comprehensive coverage of the Monopoly game
   implementation and ensures both the correctness and robustness of the system.
   Through a mixture of glass box and randomized testing, we were able to
   explore almost all paths of our logic as well as simulate variability to
   verify correct handling of unexpected events like landing on income or luxury
   tax. In this manner, we accurately scanned for potential deterministic and
   non-deterministic issues, ensuring overall correctness. *)

open OUnit2
module Test_Board = Monopoly.Board
module Test_Player = Monopoly.Player
module Test_Special = Monopoly.Special
module Test_Auction = Monopoly.Auction

let board = Test_Board.initialize_board ()

(*Small 2x2 sample test board*)
let board2 =
  let board1 = Test_Board.create_board 2 2 in
  (* Row #1 *)
  board1.(0).(0) <- Test_Board.create_property (Some "LA") (Some 300);
  board1.(0).(1) <- Test_Board.create_property (Some "SF") (Some 220);

  (* Row #2 *)
  board1.(1).(0) <- Test_Board.create_property (Some "NYC") (Some 400);
  board1.(1).(1) <- Test_Board.create_property (Some "Boston") (Some 200);

  board1

let player1 = Test_Player.new_player "Grant" 1
let player2 = Test_Player.new_player "Vedha" 2
let player3 = Test_Player.new_player "Michelle" 3
let player4 = Test_Player.new_player "Chris" 4
let payer1, receiver1 = Test_Player.pay_player 200 player1 player2
let payer2, receiver2 = Test_Player.pay_player 1600 player3 player4
let players = [ player1; player2; player3; player4 ]

let tests =
  [
    "Property/BoardFunctions"
    >::: [
           ( "test_get_property1" >:: fun _ ->
             assert_equal
               (Test_Board.create_property (Some "Fr Park") None)
               (Test_Board.get_property1 board 0 0) );
           ( "test_get_property2" >:: fun _ ->
             assert_equal
               (Test_Board.create_property (Some "Ori Ave") (Some 100))
               (Test_Board.get_property1 board 10 4) );
           ( "test_get_property3" >:: fun _ ->
             assert_equal
               (Test_Board.create_property None None)
               (Test_Board.get_property1 board 9 2) );
           ( "test_get_property4" >:: fun _ ->
             assert_equal
               (Test_Board.create_property (Some "Chance ") None)
               (Test_Board.get_property1 board 6 10) );
           ( "test_get_property5" >:: fun _ ->
             assert_equal
               (Test_Board.create_property (Some "Short L") (Some 200))
               (Test_Board.get_property1 board 5 10) );
           ( "test_get_property6" >:: fun _ ->
             assert_equal
               (Test_Board.create_property (Some "LA") (Some 300))
               (Test_Board.get_property1 board2 0 0) );
           ( "test_get_property7" >:: fun _ ->
             assert_equal
               (Test_Board.create_property (Some "SF") (Some 220))
               (Test_Board.get_property1 board2 0 1) );
           ( "test_get_property8" >:: fun _ ->
             assert_equal
               (Test_Board.create_property (Some "NYC") (Some 400))
               (Test_Board.get_property1 board2 1 0) );
           ( "test_get_property9" >:: fun _ ->
             assert_equal
               (Test_Board.create_property (Some "Boston") (Some 200))
               (Test_Board.get_property1 board2 1 1) );
           ( "test_property_name" >:: fun _ ->
             assert_equal (Some "Fr Park")
               (Test_Board.create_property (Some "Fr Park") None
               |> Test_Board.property_name) );
           ( "test_property_owner" >:: fun _ ->
             assert_equal None
               (Test_Board.create_property (Some "Fr Park") None
               |> Test_Board.property_owner) );
           ( "test_property_price" >:: fun _ ->
             assert_equal None
               (Test_Board.create_property (Some "Fr Park") None
               |> Test_Board.property_price) );
         ];
    "PlayerFunctions"
    >::: [
           ( "test_player_name" >:: fun _ ->
             assert_equal "Grant" (Test_Player.get_player_name player1) );
           ( "test_player_initial_money" >:: fun _ ->
             assert_equal 1500 (Test_Player.get_player_money player1) );
           ( "test_player_initial_location" >:: fun _ ->
             assert_equal (10, 10) (Test_Player.get_player_location player1) );
           ( "test_player_initial_state" >:: fun _ ->
             assert_equal Test_Player.Waiting
               (Test_Player.get_player_state player1) );
           ( "test_player_payer_money" >:: fun _ ->
             assert_equal 1300 (Test_Player.get_player_money payer1) );
           ( "test_player_payer_money" >:: fun _ ->
             assert_equal 1300 (Test_Player.get_player_money payer1) );
           ( "test_player_receiver_money" >:: fun _ ->
             assert_equal 1700 (Test_Player.get_player_money receiver1) );
           ( "test_player_payer_insufficient" >:: fun _ ->
             assert_equal Test_Player.Game_Over
               (Test_Player.get_player_state payer2) );
           ( "test_player_receiver_insufficient" >:: fun _ ->
             assert_equal 1500 (Test_Player.get_player_money receiver2) );
           ( "test_player_collect_money1" >:: fun _ ->
             assert_equal 1700
               (Test_Player.collect_money 200 player1
               |> Test_Player.get_player_money) );
           ( "test_player_collect_money2" >:: fun _ ->
             assert_equal 1500
               (Test_Player.collect_money 0 player1
               |> Test_Player.get_player_money) );
           ( "test_player_collect_money3" >:: fun _ ->
             assert_equal 1300
               (Test_Player.collect_money (-200) player1
               |> Test_Player.get_player_money) );
           ( "test_player_go_jail1" >:: fun _ ->
             let p = Test_Player.new_player "Alice" 1 in
             Test_Player.go_jail p;
             assert_equal (10, 0) (Test_Player.get_player_location p) );
           ( "test_player_go_jail2" >:: fun _ ->
             let p = Test_Player.new_player "Alice" 1 in
             Test_Player.go_jail p;
             assert_equal 3 p.jail_turns );
           ( "test_player_rolls_dice" >:: fun _ ->
             assert_equal true
               (let x = Test_Player.roll_dice () in
                1 <= x && x <= 6) );
           ( "test_players_get_player" >:: fun _ ->
             assert_equal player4 (Test_Player.get_player 3 players) );
           ( "test_players_index_name" >:: fun _ ->
             assert_equal "Grant" (Test_Player.get_player_index_name 0 players)
           );
           ( "test_players_index_location" >:: fun _ ->
             assert_equal (10, 10)
               (Test_Player.get_player_index_location 1 players) );
           ( "test_move_counter_clockwise1" >:: fun _ ->
             assert_equal (0, 9) (Test_Player.move_counter_clockwise (0, 8)) );
           ( "test_move_counter_clockwise2" >:: fun _ ->
             assert_equal (0, 1) (Test_Player.move_counter_clockwise (0, 0)) );
           ( "test_move_counter_clockwise3" >:: fun _ ->
             assert_equal (10, 1) (Test_Player.move_counter_clockwise (10, 2))
           );
           ( "test_move_counter_clockwise4" >:: fun _ ->
             assert_equal (1, 10) (Test_Player.move_counter_clockwise (0, 10))
           );
           ( "test_move_counter_clockwise5" >:: fun _ ->
             assert_equal (9, 0) (Test_Player.move_counter_clockwise (10, 0)) );
           ( "test_move_counter_clockwise6" >:: fun _ ->
             assert_equal (10, 9) (Test_Player.move_counter_clockwise (10, 10))
           );
           ( "test_move_counter_clockwise7" >:: fun _ ->
             assert_equal (5, 5) (Test_Player.move_counter_clockwise (5, 5)) );
           ( "test_move_counter_clockwise8" >:: fun _ ->
             assert_equal (10, 10) (Test_Player.move_counter_clockwise (9, 10))
           );
           ( "test_move_counter_clockwise9" >:: fun _ ->
             assert_equal (0, 8) (Test_Player.move_counter_clockwise (0, 7)) );
           ( "test_move_counter_clockwise10" >:: fun _ ->
             assert_equal (2, 3) (Test_Player.move_counter_clockwise (2, 3)) );
           ( "test_move_mutliple_moves1" >:: fun _ ->
             assert_equal (0, 1) (Test_Player.move_multiple_moves (0, 0) 1) );
           ( "test_move_mutliple_moves2" >:: fun _ ->
             assert_equal (0, 5) (Test_Player.move_multiple_moves (0, 0) 5) );
           ( "test_move_mutliple_moves3" >:: fun _ ->
             assert_equal (1, 10) (Test_Player.move_multiple_moves (0, 0) 11) );
           ( "test_move_mutliple_moves4" >:: fun _ ->
             assert_equal (6, 10) (Test_Player.move_multiple_moves (0, 10) 6) );
           ( "test_move_mutliple_moves5" >:: fun _ ->
             assert_equal (5, 5) (Test_Player.move_multiple_moves (5, 5) 10) );
           ( "test_move_mutliple_moves6" >:: fun _ ->
             assert_equal (0, 1) (Test_Player.move_multiple_moves (10, 0) 11) );
           ( "test_move_mutliple_moves7" >:: fun _ ->
             assert_equal (10, 6) (Test_Player.move_multiple_moves (10, 10) 4)
           );
           ( "test_move_mutliple_moves8" >:: fun _ ->
             assert_equal (4, 10) (Test_Player.move_multiple_moves (2, 10) 2) );
           ( "test_move_mutliple_moves9" >:: fun _ ->
             assert_equal (10, 10) (Test_Player.move_multiple_moves (10, 10) 40)
           );
           ( "test_move_mutliple_moves10" >:: fun _ ->
             assert_equal (0, 0) (Test_Player.move_multiple_moves (10, 10) 100)
           );
           ( "test_move_mutliple_moves11" >:: fun _ ->
             assert_equal (10, 0) (Test_Player.move_multiple_moves (10, 6) 6) );
           ( "test_move_mutliple_moves12" >:: fun _ ->
             assert_equal (10, 7) (Test_Player.move_multiple_moves (7, 10) 6) );
           ( "test_move_mutliple_moves13" >:: fun _ ->
             assert_equal (2, 10) (Test_Player.move_multiple_moves (0, 7) 5) );
           ( "test_move_mutliple_moves14" >:: fun _ ->
             assert_equal (7, 10) (Test_Player.move_multiple_moves (2, 10) 5) );
           ( "test_move_mutliple_moves15" >:: fun _ ->
             assert_equal (0, 1) (Test_Player.move_multiple_moves (3, 0) 4) );
           ( "test_move_mutliple_moves16" >:: fun _ ->
             assert_equal (0, 5) (Test_Player.move_multiple_moves (0, 1) 4) );
           ( "test_move_mutliple_moves17" >:: fun _ ->
             assert_equal (10, 7) (Test_Player.move_multiple_moves (10, 10) 3)
           );
           ( "test_move_mutliple_moves18" >:: fun _ ->
             assert_equal (10, 8) (Test_Player.move_multiple_moves (9, 10) 3) );
           ( "test_move_mutliple_moves19" >:: fun _ ->
             assert_equal (10, 5) (Test_Player.move_multiple_moves (10, 7) 2) );
           ( "test_move_mutliple_moves20" >:: fun _ ->
             assert_equal (0, 1) (Test_Player.move_multiple_moves (1, 0) 2) );
           ( "test_move_mutliple_moves21" >:: fun _ ->
             assert_equal (3, 0) (Test_Player.move_multiple_moves (4, 0) 1) );
           ( "test_move_mutliple_moves22" >:: fun _ ->
             assert_equal (4, 10) (Test_Player.move_multiple_moves (3, 10) 1) );
           ( "test_get_property_name1" >:: fun _ ->
             assert_equal "Fr Park" (Test_Player.get_property_name board (0, 0))
           );
           ( "test_get_property_name2" >:: fun _ ->
             assert_equal "none" (Test_Player.get_property_name board (5, 5)) );
           ( "test_get_property_name3" >:: fun _ ->
             assert_equal "NC Ave "
               (Test_Player.get_property_name board (2, 10)) );
           ( "test_update_player_location_same_location" >:: fun _ ->
             Test_Player.update_player_location player1 0;

             let updated_location = Test_Player.get_player_location player1 in

             assert_equal (10, 10) updated_location );
           ( "test_update_player_location_multiple_moves1" >:: fun _ ->
             let player = Test_Player.new_player "Jay" 5 in
             Test_Player.update_player_location player 5;

             let updated_location = Test_Player.get_player_location player in

             assert_equal (10, 5) updated_location );
           ( "test_update_player_location_multiple_moves2" >:: fun _ ->
             let player = Test_Player.new_player "Jane" 6 in
             Test_Player.update_player_location player 400;

             let updated_location = Test_Player.get_player_location player in

             assert_equal (10, 10) updated_location );
           ( "test_update_player_location_multiple_moves3" >:: fun _ ->
             Test_Player.update_player_location player3 0;

             Test_Player.update_player_location player3 35;

             let updated_location = Test_Player.get_player_location player3 in

             assert_equal (5, 10) updated_location );
           ( "test_update_player_location_multiple_moves4" >:: fun _ ->
             Test_Player.update_player_location player1 5;

             Test_Player.update_player_location player2 5;

             Test_Player.update_player_location player3 5;

             Test_Player.update_player_location player4 5;

             let updated_location = Test_Player.get_player_location player1 in

             assert_equal (10, 5) updated_location );
           ( "test_update_player_location_multiple_moves5" >:: fun _ ->
            let player = Test_Player.new_player "Jacob" 1 in
             Test_Player.update_player_location player 5;

             Test_Player.update_player_location player 0;

             Test_Player.update_player_location player 2;

             let updated_location = Test_Player.get_player_location player in

          assert_equal (10,3) updated_location;
        );
        "test_is_bought1" >:: (fun _ ->          
          assert_equal false (Test_Player.is_bought (0,0) board2);
          Test_Player.property_action_buy player1 (0,0) board2;
          assert_equal true (Test_Player.is_bought (0,0) board2);
        );
        "test_is_bought2" >:: (fun _ ->          
          assert_equal false (Test_Player.is_bought (1,1) board2);
          Test_Player.property_action_buy player1 (1,1) board2;
          assert_equal true (Test_Player.is_bought (1,1) board2);
        );
        "test_get_location_property1" >:: (fun _ ->          
          assert_equal (Some "LA") 
          (Test_Player.get_location_property (0,0) board2 |> Test_Board.property_name);      
        );
        "test_get_location_property2" >:: (fun _ ->          
          assert_equal (Some "SF") 
          (Test_Player.get_location_property (0,1) board2 |> Test_Board.property_name);
        );
        "test_get_location_property3" >:: (fun _ ->          
          assert_equal (Some "NYC") 
          (Test_Player.get_location_property (1,0) board2 |> Test_Board.property_name);
        );
        "test_get_location_property4" >:: (fun _ ->          
          assert_equal (Some "Boston") 
          (Test_Player.get_location_property (1,1) board2 |> Test_Board.property_name);
        );
        "test_check_negative" >:: (fun _ ->          
          let p1 = Test_Player.new_player "Jane" 1 in assert_equal false (Test_Player.check_negative p1);
          p1.balance <- -100;
          assert_equal true (Test_Player.check_negative p1)
        );
        "test_change_state" >:: (fun _ ->          
          let p1 = Test_Player.new_player "Jane" 1 in Test_Player.change_state p1;
          assert_equal Test_Player.Game_Over (Test_Player.get_player_state p1)
        );
        "test_keep_state" >:: (fun _ ->          
          let p1 = Test_Player.new_player "Jane" 1 in Test_Player.keep_state p1;
          assert_equal Test_Player.Waiting (Test_Player.get_player_state p1)
        );        
        "test_property_action_buy2" >:: (fun _ ->          
          Test_Player.property_action_buy player2 (0,1) board2;
          assert_equal 1280 (Test_Player.get_player_money player2);
          assert_equal (Some "Vedha") (Test_Player.get_location_property (0,1) board2 |>
          Test_Board.property_owner );
        );
        "test_property_action_buy3" >:: (fun _ ->          
          Test_Player.property_action_buy player3 (1,0) board2;
          assert_equal 1100 (Test_Player.get_player_money player3);
          assert_equal (Some "Michelle") (Test_Player.get_location_property (1,0) board2 |>
          Test_Board.property_owner );
        );
        "test_property_action_buy4" >:: (fun _ ->          
          Test_Player.property_action_buy player4 (1,1) board2;
          assert_equal 1300 (Test_Player.get_player_money player4);
          assert_equal (Some "Chris") (Test_Player.get_location_property (1,1) board2 |>
          Test_Board.property_owner );
        );
        "test_property_action_rent1" >:: (fun _ ->          
          let p1 = Test_Player.new_player "Alice" 1 in
          let p2 = Test_Player.new_player "Bob" 2 in
          let location = (0, 0) in
          Test_Player.property_action_buy p1 location board2;
          Test_Player.property_action_rent p1 p2 location board2;
          assert_equal 1425 (Test_Player.get_player_money p2);
          assert_equal 1275 (Test_Player.get_player_money p1)
        );
        "test_property_action_rent2" >:: (fun _ ->          
          let p1 = Test_Player.new_player "Alice" 1 in
          let p2 = Test_Player.new_player "Bob" 2 in
          let location = (0, 1) in
          Test_Player.property_action_buy p1 location board2;
          Test_Player.property_action_rent p1 p2 location board2;
          assert_equal 1445 (Test_Player.get_player_money p2);
          assert_equal 1335 (Test_Player.get_player_money p1)
        );
        "test_property_action_rent3" >:: (fun _ ->          
          let p1 = Test_Player.new_player "Alice" 1 in
          let p2 = Test_Player.new_player "Bob" 2 in
          let location = (1, 0) in
          Test_Player.property_action_buy p1 location board2;
          Test_Player.property_action_rent p1 p2 location board2;
          assert_equal 1400 (Test_Player.get_player_money p2);
          assert_equal 1200 (Test_Player.get_player_money p1)
        );
        "test_property_action_rent4" >:: (fun _ ->          
          let p1 = Test_Player.new_player "Alice" 1 in
          let p2 = Test_Player.new_player "Bob" 2 in
          let location = (1, 1) in
          Test_Player.property_action_buy p1 location board2;
          Test_Player.property_action_rent p1 p2 location board2;
          assert_equal 1450 (Test_Player.get_player_money p2);
          assert_equal 1350 (Test_Player.get_player_money p1)
        );
        "test_compare_balance" >:: (fun _ ->          
          let p1 = Test_Player.new_player "Alice" 1 in
          let p2 = Test_Player.new_player "Bob" 2 in
          let location = (0, 1) in
          Test_Player.property_action_buy p1 location board2;
          assert_equal (p2) (Test_Player.compare_balance p1 p2)
        );
        "test_compare_balance2" >:: (fun _ ->          
          let p1 = Test_Player.new_player "Jake" 1 in
          let p2 = Test_Player.new_player "Zach" 2 in
          let location = (1, 1) in
          Test_Player.property_action_buy p1 location board2;
          Test_Player.property_action_buy p2 location board2;
          assert_equal (p2) (Test_Player.compare_balance p1 p2)
        );
        "test_compare_balance3" >:: (fun _ ->          
          let p1 = Test_Player.new_player "Alice" 1 in
          let p2 = Test_Player.new_player "Bob" 2 in
          let location = (1, 1) in
          Test_Player.property_action_buy p1 location board2;
          Test_Player.property_action_rent p1 p2 location board2;
          Test_Player.property_action_rent p1 p2 location board2;
          Test_Player.property_action_rent p1 p2 location board2;
          assert_equal (p1) (Test_Player.compare_balance p1 p2)
        );
        "test_get_winner" >:: (fun _ ->          
          let p1 = Test_Player.new_player "Alice" 1 in
          let p2 = Test_Player.new_player "Bob" 2 in
          let p3 = Test_Player.new_player "Jake" 3 in
          let p4 = Test_Player.new_player "Zach" 4 in
          let location = (1, 1) in
          Test_Player.property_action_buy p1 location board2;
          Test_Player.property_action_buy p2 location board2;
          Test_Player.property_action_buy p4 location board2;
          assert_equal (p3) (Test_Player.get_winner p1 p2 p3 p4)
        );
        "test_create_property_statistics own two properties board2" >:: (fun _ -> 
          let player1 = Test_Player.new_player "Jason" 3 in
          let property_stats = Test_Player.intialize_player_statistics player1 in       
          Test_Player.property_action_buy player1 (0,0) board2;
          Test_Player.property_action_buy player1 (1,0) board2;
          Test_Player.create_property_statistics board2 property_stats;
          assert_equal (property_stats.properties_owned) (["NYC"; "LA"])
        );
        "test_create_property_statistics own no properties board2" >:: (fun _ ->   
          let player = Test_Player.new_player "Jacob" 1 in
          let property_stats = Test_Player.intialize_player_statistics player in       
          Test_Player.create_property_statistics board2 property_stats;
          assert_equal (property_stats.properties_owned) ([])
        );
        "test_create_property_statistics own all properties board2" >:: (fun _ ->   
          let property_stats = Test_Player.intialize_player_statistics player4 in  
          Test_Player.property_action_buy player4 (0,0) board2;
          Test_Player.property_action_buy player4 (1,0) board2;
          Test_Player.property_action_buy player4 (1,1) board2;
          Test_Player.property_action_buy player4 (0,1) board2;     
          Test_Player.create_property_statistics board2 property_stats;
          assert_equal (property_stats.properties_owned) (["Boston"; "NYC"; "SF"; "LA"])
        );
        
        ];

    "SpecialFunctions" >::: [
      "test_handle_chance"  >:: (fun _ ->
        (*test_handle_chance draws a random card from the chance card pile, and then
           asserts that this card matches to at least one of the possible chance cards*)
        let test_handle_chance i = 
          let s = string_of_int i in
          Printf.printf "Chance Card: %s\n" s;
          let p1 = Test_Player.new_player "Grant" 1 in 
          Test_Special.handle_chance p1;
          let test1 = Test_Player.new_player "Grant" 1 in
          let test2 = Test_Player.new_player "Grant" 1 in 
          Test_Player.update_player_location test2 10;
          test2.jail_turns <- 3;
          let test3 = Test_Player.new_player "Grant" 1 in 
          Test_Player.update_player_location test3 24;
          let test4 = Test_Player.new_player "Grant" 1 in 
          Test_Player.update_player_location test4 37;
          let test5 = Test_Player.new_player "Grant" 1 in 
          Test_Player.update_player_location test5 39;
          let test6 = Test_Player.new_player "Grant" 1 in
          test6.balance <- test6.balance - 15;
          let test7 = Test_Player.new_player "Grant" 1 in 
          test7.balance <- test7.balance + 150;
          let test8 = Test_Player.new_player "Grant" 1 in
          test8.balance <- test8.balance + 50;
          let test9 = Test_Player.new_player "Grant" 1 in
          test9.balance <- test9.balance - 50;
          let test10 = Test_Player.new_player "Grant" 1 in
          test10.balance <- test10.balance + 100;
          let test11 = Test_Player.new_player "Grant" 1 in
          Test_Player.update_player_location test11 26;
          let test12 = Test_Player.new_player "Grant" 1 in 
          Test_Player.update_player_location test12 16;
          let test13 = Test_Player.new_player "Grant" 1 in 
          test13.balance <- test13.balance + 10;
          let test14 = Test_Player.new_player "Grant" 1 in 
          Test_Player.update_player_location test14 20;
          let test15 = Test_Player.new_player "Grant" 1 in 
          test15.balance <- test15.balance + 25;
          let test16 = Test_Player.new_player "Grant" 1 in
          test16.balance <- test16.balance - 150;
          assert_equal (true) (test1 = p1 || 
          test2 = p1 || test3 = p1 || test4 = p1 || test5 = p1 
          || test6 = p1 || test7 = p1 || test8 = p1 || test9 = p1 || test10 = p1
          || test11 = p1 || test12 = p1 || test13 = p1 || test14 = p1 || test15 = p1 || test16 = p1)
        in for i = 1 to 32 do
          test_handle_chance i
        done
      );
      "test_handle_community"  >:: (fun _ ->
        (*test_handle_community draws a random card from the community card pile, and then
           asserts that this card matches to at least one of the possible community cards*)
        let test_handle_community i = 
          let s = string_of_int i in
          Printf.printf "Community Card: %s\n" s;
          let p1 = Test_Player.new_player "Grant" 1 in 
          Test_Special.handle_community p1;
          let test1 = Test_Player.new_player "Grant" 1 in
          test1.balance <- test1.balance + 200;
          let test2 = Test_Player.new_player "Grant" 1 in
          test2.balance <- test2.balance + 160;
          let test3 = Test_Player.new_player "Grant" 1 in
          test3.balance <- test3.balance + 100;
          let test4 = Test_Player.new_player "Grant" 1 in
          test4.balance <- test4.balance + 20;
          let test5 = Test_Player.new_player "Grant" 1 in
          test5.balance <- test5.balance + 80;
          let test6 = Test_Player.new_player "Grant" 1 in
          test6.balance <- test6.balance + 25; 
          let test7 = Test_Player.new_player "Grant" 1 in
          test7.balance <- test7.balance - 50; 
          let test8 = Test_Player.new_player "Grant" 1 in
          test8.balance <- test8.balance + 120; 
          let test9 = Test_Player.new_player "Grant" 1 in
          test9.balance <- test9.balance + 40; 
          let test10 = Test_Player.new_player "Grant" 1 in
          test10.balance <- test10.balance - 60; 
          let test11 = Test_Player.new_player "Grant" 1 in
          test11.balance <- test11.balance + 50; 
          let test12 = Test_Player.new_player "Grant" 1 in
          test12.balance <- test12.balance + 60; 
          let test13 = Test_Player.new_player "Grant" 1 in
          test13.balance <- test13.balance - 40; 
          let test14 = Test_Player.new_player "Grant" 1 in
          test14.balance <- test14.balance - 25; 
          let test15 = Test_Player.new_player "Grant" 1 in
          test15.balance <- test15.balance - 35; 
          let test16 = Test_Player.new_player "Grant" 1 in
          test16.balance <- test16.balance + 35; 
          assert_equal (true) (test1 = p1 || 
          test2 = p1 || test3 = p1 || test4 = p1 || test5 = p1 || test6 = p1 
          ||test7 = p1 || test8 = p1 || test9 = p1 || test10 = p1 || test11 = p1 ||test12 = p1
          ||test13 = p1 || test14 = p1 || test15 = p1 || test16 = p1)
        in for i = 1 to 32 do
          test_handle_community i
        done
      );
      "test_inc_tax" >:: (fun _ ->          
        let p1 = Test_Player.new_player "Alice" 1 in
        Test_Special.handle_inc p1;
        let test1 = Test_Player.new_player "Alice" 1 in
        test1.balance <- test1.balance - 200;
        assert_equal (test1) (p1)
      );
      "test_lux_tax" >:: (fun _ ->          
        let p1 = Test_Player.new_player "Alice" 1 in
        Test_Special.handle_lux p1;
        let test1 = Test_Player.new_player "Alice" 1 in
        test1.balance <- test1.balance - 100;
        assert_equal (test1) (p1)
      );
    
      ];
      "AuctionFunctions" >::: [
        "test_houses_none" >:: (fun _ ->   
            let property = Test_Player.get_location_property (0,0) board2 in
            assert_equal 0 (property.houses)
          );
        "test_build_house_one" >:: (fun _ ->   
          Test_Player.property_action_buy player1 (1,0) board2;
          let property = Test_Player.get_location_property (1,0) board2 in
          Test_Auction.build_house player1 property;
          assert_equal 1 (property.houses)
        );
        "test_property_action_rent with one house" >:: (fun _ ->          
          let p1 = Test_Player.new_player "Alice" 1 in
          let p2 = Test_Player.new_player "Bob" 2 in
          Test_Player.property_action_buy player1 (0,1) board;
          let property = Test_Player.get_location_property (0,1) board in
          Test_Auction.build_house p1 property;
          Test_Player.property_action_buy p1 (0,1) board;
          Test_Player.property_action_rent p1 p2 (0,1) board;
          assert_equal 1401 (Test_Player.get_player_money p2);
          assert_equal 1279 (Test_Player.get_player_money p1)
        );
        "test_build_house_multiple" >:: (fun _ ->   
          Test_Player.property_action_buy player1 (1,1) board2;
          let property = Test_Player.get_location_property (1,1) board2 in
          Test_Auction.build_house player1 property;
          Test_Auction.build_house player1 property;
          Test_Auction.build_house player1 property;
          assert_equal 3 (property.houses)
        );
        "test_property_action_rent with multiple houses" >:: (fun _ ->          
          let p1 = Test_Player.new_player "Alice" 1 in
          let p2 = Test_Player.new_player "Bob" 2 in
          Test_Player.property_action_buy player1 (9,10) board;
          let property = Test_Player.get_location_property (9,10) board in
          Test_Auction.build_house p1 property;
          Test_Auction.build_house p1 property;
          Test_Auction.build_house p1 property;
          Test_Player.property_action_buy p1 (9,10) board;
          Test_Player.property_action_rent p1 p2 (9,10) board;
          assert_equal 1160 (Test_Player.get_player_money p2);
          assert_equal 1140 (Test_Player.get_player_money p1)
        );
        "test_build_house_try_more_than_four" >:: (fun _ ->   
          Test_Player.property_action_buy player1 (1,1) board2;
          let property = Test_Player.get_location_property (1,1) board2 in
          Test_Auction.build_house player1 property;
          Test_Auction.build_house player1 property;
          Test_Auction.build_house player1 property;
          Test_Auction.build_house player1 property;
          Test_Auction.build_house player1 property;
          Test_Auction.build_house player1 property;
          assert_equal 4 (property.houses)
        );
        "test_hotel_none" >:: (fun _ ->   
          let property = Test_Player.get_location_property (1,1) board2 in
          assert_equal 0 (property.hotel)
        );
        "test_hotel_build_none1" >:: (fun _ ->   
          Test_Player.property_action_buy player1 (1,1) board2;
          let property = Test_Player.get_location_property (0,1) board2 in
          Test_Auction.build_hotel player1 property;
          assert_equal 0 (property.hotel)
        );
        "test_hotel_build_none2" >:: (fun _ ->   
          Test_Player.property_action_buy player1 (1,1) board2;
          let property = Test_Player.get_location_property (0,1) board2 in
          Test_Auction.build_house player1 property;
          Test_Auction.build_hotel player1 property;
          assert_equal 0 (property.hotel)
        );
        "test_hotel_build_hotel" >:: (fun _ ->   
          Test_Player.property_action_buy player1 (2,0) board;
          let property = Test_Player.get_location_property (2,0) board in
          Test_Auction.build_house player1 property;
          Test_Auction.build_house player1 property;
          Test_Auction.build_house player1 property;
          Test_Auction.build_house player1 property;
          Test_Auction.build_hotel player1 property;
          assert_equal 1 (property.hotel)
        );
        "test_property_action_rent with hotel" >:: (fun _ ->          
          let p1 = Test_Player.new_player "Alice" 1 in
          let p2 = Test_Player.new_player "Bob" 2 in
          Test_Player.property_action_buy player1 (10,1) board;
          let property = Test_Player.get_location_property (10,1) board in
          Test_Auction.build_house p1 property;
          Test_Auction.build_house p1 property;
          Test_Auction.build_house p1 property;
          Test_Auction.build_house p1 property;
          Test_Auction.build_hotel p1 property;
          Test_Player.property_action_buy p1 (10,1) board;
          Test_Player.property_action_rent p1 p2 (10,1) board;
          assert_equal 1350 (Test_Player.get_player_money p2);
          assert_equal 930 (Test_Player.get_player_money p1)
        );
        "test_hotel_try_more_than_one" >:: (fun _ ->   
          Test_Player.property_action_buy player1 (4,0) board;
          let property = Test_Player.get_location_property (4,0) board in
          Test_Auction.build_house player1 property;
          Test_Auction.build_house player1 property;
          Test_Auction.build_house player1 property;
          Test_Auction.build_house player1 property;
          Test_Auction.build_hotel player1 property;
          Test_Auction.build_hotel player1 property;
          assert_equal 1 (property.hotel)
        );
        "test_find_property_house unowned" >:: (fun _ ->   
          let property = Test_Player.get_location_property (7,0) board in
          let name = Test_Board.property_name(property) in
          Test_Auction.find_property_house player1 name board;
          assert_equal 0 (property.houses)
        );
        "test_find_property_house find incorrect property_name" >:: (fun _ ->   
          Test_Player.property_action_buy player1 (8,0) board;
          let property = Test_Player.get_location_property (8,0) board in
          let name = Some "Lansing" in
          Test_Auction.find_property_house player1 name board;
          assert_equal 0 (property.houses)
        );
        "test_find_property_house find once" >:: (fun _ ->   
          Test_Player.property_action_buy player1 (7,0) board;
          let property = Test_Player.get_location_property (7,0) board in
          let name = Test_Board.property_name(property) in
          Test_Auction.find_property_house player1 name board;
          assert_equal 1 (property.houses)
        );
        "test_find_property_house find multiple times" >:: (fun _ ->   
          Test_Player.property_action_buy player1 (8,0) board;
          let property = Test_Player.get_location_property (8,0) board in
          let name = Test_Board.property_name(property) in
          Test_Auction.find_property_house player1 name board;
          Test_Auction.find_property_house player1 name board;
          Test_Auction.find_property_house player1 name board;
          assert_equal 3 (property.houses)
        );
        "test_find_property_hotel unowned" >:: (fun _ ->   
          let property = Test_Player.get_location_property (9,0) board in
          let name = Test_Board.property_name(property) in
          Test_Auction.find_property_hotel player1 name board;
          assert_equal 0 (property.hotel)
        );
        "test_find_property_hotel insufficient number of houses" >:: (fun _ ->   
          let property = Test_Player.get_location_property (9,0) board in
          let name = Test_Board.property_name(property) in
          Test_Auction.build_house player1 property;
          Test_Auction.build_house player1 property;
          Test_Auction.find_property_hotel player1 name board;
          assert_equal 0 (property.hotel)
        );
        "test_find_property_hotel find incorrect property_name" >:: (fun _ ->   
          Test_Player.property_action_buy player1 (10,4) board;
          let property = Test_Player.get_location_property (10,4) board in
          let name = Some "Lansing" in
          Test_Auction.build_house player1 property;
          Test_Auction.build_house player1 property;
          Test_Auction.build_house player1 property;
          Test_Auction.build_house player1 property;
          Test_Auction.find_property_hotel player1 name board;
          assert_equal 0 (property.hotel)
        );
        "test_find_property_house find 1 " >:: (fun _ ->   
          Test_Player.property_action_buy player1 (10,7) board;
          let property = Test_Player.get_location_property (10,7) board in
          let name = Test_Board.property_name(property) in
          Test_Auction.build_house player1 property;
          Test_Auction.build_house player1 property;
          Test_Auction.build_house player1 property;
          Test_Auction.build_house player1 property;
          Test_Auction.find_property_hotel player1 name board;
          assert_equal 1 (property.hotel)
        );
        "test_find_property_house try find multiple " >:: (fun _ ->   
          Test_Player.property_action_buy player1 (10,9) board;
          let property = Test_Player.get_location_property (10,9) board in
          let name = Test_Board.property_name(property) in
          Test_Auction.build_house player1 property;
          Test_Auction.build_house player1 property;
          Test_Auction.build_house player1 property;
          Test_Auction.build_house player1 property;
          Test_Auction.find_property_hotel player1 name board;
          Test_Auction.find_property_hotel player1 name board;
          assert_equal 1 (property.hotel)
        );

        
      ];
      
      

   ]

let suite = "search test suite" >::: List.flatten [ tests ]
let _ = run_test_tt_main suite
