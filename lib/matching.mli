module BidMap : Map.S with type key = float
module AskMap : Map.S with type key = float

val execute_fill :
  Order.order -> Order.order ->
  Order.fill * Order.order option * Order.order option

val match_buy :
  Order.order ->
  Order.order list BidMap.t ->
  Order.order list AskMap.t ->
  Order.fill list ->
  Order.order option * Order.order list BidMap.t * Order.order list AskMap.t * Order.fill list

val match_sell :
  Order.order ->
  Order.order list BidMap.t ->
  Order.order list AskMap.t ->
  Order.fill list ->
  Order.order option * Order.order list BidMap.t * Order.order list AskMap.t * Order.fill list
