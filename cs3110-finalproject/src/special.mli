open Player

(** 
    Special functions:

    Contains functions that handle what happens to a player
    when they land on a special square (non-property square).
*)

type chance = {
  description : string;
  action : player -> unit;
}

type community_chest = {
  description : string;
  benefit : player -> unit;
}

val draw_chance : unit -> chance
(** [draw_chance ()] returns a randomly selected chance card. *)

val draw_community : unit -> community_chest
(** [draw_community ()] returns a randomly selected community chest card. *)

val handle_chance : player -> unit
(** [handle_chance p] takes a player [p] as an argument, draws a chance card,
    prints the card's description, and performs the associated action on the
    player. *)

val handle_community : player -> unit
(** [handle_community p] takes a player [p] as an argument, draws a community
    chest card, prints the card's description, and provides the associated
    benefit to the player. *)

val handle_inc : player -> unit
(** [handle_inc p] takes a player [p] as an argument and deducts $200 from the
    player's balance. *)

val handle_lux : player -> unit
(** [handle_lux p] takes a player [p] as an argument and deducts $100 from the
    player's balance. *)

val visiting_jail : player -> unit
(** [visiting_jail p] takes a player [p] as an argument and prints a message
    indicating that the player is currently visiting jail. *)

val returned_to_go : player -> unit
(** [returned_to_go p] takes a player [p] as an argument and prints a message
    indicating that the player has returned to the starting square (Go). *)

val free_parking : player -> unit
(** [free_parking p] takes a player [p] as an argument and prints a message
    indicating that the player is at a free parking lot. *)

val handle_jail : player -> unit
(** [handle_jail p] takes a player [p] as an argument, presents the player with
    three options when in jail, and executes the chosen option. Options include:
    - Option 1: Roll a pair of dice for doubles to get out of jail.
    - Option 2: Pay a $200 fine to get out of jail.
    - Option 3: Choose to wait in jail for another turn. 
    The player is prompted to choose an option by typing the corresponding number. 
    If the player  chooses option 1, the [handle_jail_doubles] function is called. 
    If the player chooses option 2, a $200 fine is deducted from the player's
    balance, and the player is released from jail. If the player chooses
    option 3 (or any other input), the player spends another turn in jail. *)
