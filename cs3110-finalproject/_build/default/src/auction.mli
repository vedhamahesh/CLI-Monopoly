open Player
open Board

(**
  Auction functions.

  Auction functions deal with all related transactions that are prompted in game,
  including whether to buy, auction, or build.
  *)

type auction_bid = {
  bidder : player;
  amount : int;
}

type auction_state = {
  mutable highest_bid : auction_bid option;
  mutable active_bidders : player list;
}

val start_auction : player list -> location -> board -> unit
(** [start_auction players location board] initiates an auction for an unowned
    property.
    - Parameters:
    - [players]: The list of players eligible to bid.
    - [location]: The location of the property to be auctioned.
    - [board]: The game board.
    - Side Effects: Prints messages to the console and updates player balances.*)

val property_action_buy_or_auction :
  player -> player list -> location -> board -> unit
(** [property_action_buy_or_auction current_player players location board]
    prompts the current player to buy a property or start an auction.
    - Parameters:
    - [current_player]: The player making the decision.
    - [players]: The list of players eligible to bid in an auction.
    - [location]: The location of the property to be bought or auctioned.
    - [board]: The game board.
    - Side Effects: Prints messages to the console and calls appropriate
      functions based on player input. *)

val build_house : player -> property -> unit
(** [build_house player property] builds a house on a specified property if
    conditions are met.
    - Parameters:
    - [player]: The player building the house.
    - [property]: The property on which to build the house.
    - Side Effects: Prints messages to the console and updates player balances. *)

val build_hotel : player -> property -> unit
(** [build_hotel player property] builds a hotel on a specified property if
    conditions are met.
    - Parameters:
    - [player]: The player building the hotel.
    - [property]: The property on which to build the hotel.
    - Side Effects: Prints messages to the console and updates player balances. *)

val find_property_house :
  player -> string option -> property array array -> unit
(** [find_property_house player find_property board] finds a property by name
    and builds a house on it if conditions are met.
    - Parameters:
    - [player]: The player building the house.
    - [find_property]: The name of the property to find.
    - [board]: The game board.
    - Side Effects: Calls [build_house] and updates player balances. *)

val find_property_hotel :
  player -> string option -> property array array -> unit
(** [find_property_hotel player find_property board] finds a property by name
    and builds a hotel on it if conditions are met.
    - Parameters:
    - [player]: The player building the hotel.
    - [find_property]: The name of the property to find.
    - [board]: The game board.
    - Side Effects: Calls [build_hotel] and updates player balances. *)

val build : player -> string -> property array array -> unit
(** [build player build_property board] prompts the player to build more on a
    property (house or hotel).
    - Parameters:
    - [player]: The player deciding to build.
    - [build_property]: The name of the property to build on.
    - [board]: The game board.
    - Side Effects: Prints messages to the console and calls appropriate
      functions based on player input. *)
