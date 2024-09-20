(**
    Board functions.

    Board functions deals with all things related to creating the board and initializing
    properties on it.
    *)

type property = {
  name : string option;
  price : int option;
  owner : string option;
  mutable houses : int;
  mutable hotel : int;
}

type board = property array array

val create_property : string option -> int option -> property
(** [create_property name price] creates a new property with the given [name]
    and [price]. *)

val property_name : property -> string option
(**Takes in a property as the input and returns a string option of the
   properties name*)

val string_option_to_string : string option -> string
(**Takes in a string option and returns the string or an empty string*)

val property_price : property -> int option
(**Takes in a property as an input and returns the int value of the price*)

val int_option_to_int : int option -> int
(**Takes in an int option and returns the int or returns 0*)

val property_price_number : property -> int
(**Takes in a property option and returns the int value for price or returns 0*)

val property_owner : property -> string option
(**Takes in a property and returns the string value of the property owner, None
   if no owner*)

val create_board : int -> int -> board
(** [create_board rows cols] creates a new Monopoly board with the specified
    number of [rows] and [cols]. *)

val initialize_board : unit -> board
(** [initialize_board ()] initializes a 5 by 5 Monopoly board with default
    properties. *)

val get_property1 : board -> int -> int -> property
(** [get_property1 board row col] retrieves the property at the specified [row]
    and [col] on the [board]. *)

val set_property_owner : board -> int -> int -> string -> unit
(** [set_property_owner board row col owner] sets the owner of the property at
    the specified [row] and [col] on the [board] to [owner]. *)

val draw_property_box : string option -> unit
(** [draw_property_box name] takes Some string in [name] and draws the property/square
    associated with it. *)

val print_board : board -> unit
(** [print_board board] prints the current state of the [board] in the terminal. *)

val print_game_over_board : board -> unit
(** [print_game_over_board board] prints the game over state of the [board] in
    the terminal. *)
