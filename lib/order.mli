type side = Buy | Sell
type order_type = Limit | Market

type order = {
  id         : int;
  side       : side;
  order_type : order_type;
  price      : float;
  quantity   : int;
  timestamp  : int;
}

type fill = {
  buy_order_id  : int;
  sell_order_id : int;
  price         : float;
  quantity      : int;
}

val create_limit  : int -> side -> float -> int -> int -> order
val create_market : int -> side -> int -> int -> order
val pp_side       : side -> string
val pp_order      : order -> string
val pp_fill       : fill -> string
