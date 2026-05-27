type side = Buy | Sell

(* Limit: rests in book if not filled
   Market: crosses at any price
   IOC (Immediate-or-Cancel): fills as much as possible, cancels remainder
   FOK (Fill-or-Kill): must fill entirely or the order is cancelled *)
type order_type = Limit | Market | IOC | FOK

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

let create_limit id side price quantity timestamp =
  { id; side; order_type = Limit; price; quantity; timestamp }

let create_market id side quantity timestamp =
  { id; side; order_type = Market; price = 0.0; quantity; timestamp }

let create_ioc id side price quantity timestamp =
  { id; side; order_type = IOC; price; quantity; timestamp }

let create_fok id side price quantity timestamp =
  { id; side; order_type = FOK; price; quantity; timestamp }

let pp_side = function Buy -> "BUY" | Sell -> "SELL"

let pp_order_type = function
  | Limit  -> "LIMIT"
  | Market -> "MARKET"
  | IOC    -> "IOC"
  | FOK    -> "FOK"

let pp_order o =
  Printf.sprintf "[#%d] %s %s @ %.2f qty=%d t=%d"
    o.id (pp_side o.side) (pp_order_type o.order_type)
    o.price o.quantity o.timestamp

let pp_fill f =
  Printf.sprintf "FILL buy#%d x sell#%d @ %.2f qty=%d"
    f.buy_order_id f.sell_order_id f.price f.quantity
