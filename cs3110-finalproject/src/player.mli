open Board

(**
  Player functions.

  Player functions deal with actions that affect player properties, such as location,
  state, balance, etc.
  *)

type state =
  | Waiting
  | Game_Over
  | Roll

type l =
  | Buy of bool
  | Pay of int

type player = {
  name : string;
  player_number : int;
  mutable balance : int;
  mutable location : int * int;
  mutable state : state;
  mutable jail_turns : int;
}

type players = player list
type location = int * int

val roll_dice : unit -> int
(**Rolls a dice, taking a random integer from 1-6*)

val new_player : string -> int -> player
(**[new_player name] initializes a new player with a player_number [number] and
   name [name]*)

val get_player_name : player -> string
(**[get_player_name p] creates a string*int of the players with the string of
   players name *)

val get_player_location : player -> location
(**[get_player_location p] returns the current location of the player *)

val get_player_money : player -> int
(**[get_player_money p] creates a int of the players with the player balance *)

val get_player_state : player -> state
(**[get_player_state p] creates a int of the players with the player state *)

val pay_player : int -> player -> player -> player * player
(**[pay_player amnt payer rcver] is when player [payer] pays [amnt] of money to
   another player [rcver] *)

val collect_money : int -> player -> player
(**[collect_money amnt p] is when a player [p] collects money*)

val go_jail : player -> unit
(**[go_jail p] is when a player is sent to jail*)

val get_player_index_name : int -> players -> string
(**Input an int the resulting index in players list with the players string
   value name will be returned*)

val move_counter_clockwise : location -> location
(**Given a starting location as the input, the output is the counter clockwise
   location after moving one step*)

val move_multiple_moves : location -> int -> location
(**The function takes in the initial location and the int value of the number of
   moves to make and outputs the new counter clockwise location after moving the
   input number of moves*)

val get_player_index_location : int -> players -> location
(**Given an input of the index of a player in players list the output is the
   location of that player*)

val get_player : int -> players -> player
(**Given an index of type int of players list the output is of the type player
   at that index*)

val get_property_name : board -> location -> string
(**Takes in a board and location and returns the string value of the property at
   that location on the board*)

val update_player_location : player -> int -> unit
(**Takes in a player and moves the player from their current location to the
   corresponding location after moving counter clockwise with the input of type
   int number of moves, returns a unit*)

val property_action_rent : player -> player -> location -> board -> unit
(**Updates the players balance of both the buyer and receiver in a rent action. *)

val property_action_buy : player -> location -> board -> unit
(**Updates the balance and owener of a location on the board if a player buys a
   property*)

val is_bought : location -> board -> bool
(**Checks to see if a location on a board is currently owned and returns true,
   else returns false*)

val get_location_property : location -> board -> property
(**Given a location on a board as the input, the output of the corresponding
   property is given*)

val check_negative : player -> bool
(**Checks a players balance and returns true if it is negative, else false*)

val change_state : player -> unit
(**If a player has a negative balance the state type of the player is changed to
   Game Over*)

val keep_state : player -> unit
(**Returns the identity value of a players state*)

val compare_balance : player -> player -> player
(**Returns the player that has the greater balance *)

val get_winner : player -> player -> player -> player -> player
(**Returns the player with the greatest balance*)

type player_statistics = {
  player : player;
  mutable properties_owned : string list;
}

val intialize_player_statistics : player -> player_statistics
(**Initializes a new player with an empty string of properties owned*)

val update_player_statistics : player_statistics -> property -> unit
(**Updates a players property list as they buy properties*)

val create_property_statistics :
  property array array -> player_statistics -> unit
(**Updates a players statistics from a board input by checking if a property is
   owned*)
