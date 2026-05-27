(* BidMap: highest price first (descending) for price-time priority *)
module BidMap = Map.Make (struct
  type t = float
  let compare a b = Float.compare b a
end)

(* AskMap: lowest price first (ascending) *)
module AskMap = Map.Make (struct
  type t = float
  let compare = Float.compare
end)

let execute_fill (incoming : Order.order) (resting : Order.order)
    : Order.fill * Order.order option * Order.order option =
  let fill_qty = min incoming.quantity resting.quantity in
  let fill =
    match incoming.side with
    | Order.Buy ->
      { Order.buy_order_id  = incoming.id
      ; sell_order_id = resting.id
      ; price         = resting.price
      ; quantity      = fill_qty }
    | Order.Sell ->
      { Order.buy_order_id  = resting.id
      ; sell_order_id = incoming.id
      ; price         = resting.price
      ; quantity      = fill_qty }
  in
  let leftover q = q - fill_qty in
  let rem_incoming =
    if leftover incoming.quantity > 0
    then Some { incoming with Order.quantity = leftover incoming.quantity }
    else None
  in
  let rem_resting =
    if leftover resting.quantity > 0
    then Some { resting with Order.quantity = leftover resting.quantity }
    else None
  in
  (fill, rem_incoming, rem_resting)

let update_ask_level price rem_resting rest asks =
  match rem_resting with
  | Some r -> AskMap.add price (r :: rest) asks
  | None   ->
    if rest = [] then AskMap.remove price asks
    else AskMap.add price rest asks

let update_bid_level price rem_resting rest bids =
  match rem_resting with
  | Some r -> BidMap.add price (r :: rest) bids
  | None   ->
    if rest = [] then BidMap.remove price bids
    else BidMap.add price rest bids

(* Whether the incoming order price crosses the resting price *)
let crosses_ask (incoming : Order.order) price =
  match incoming.Order.order_type with
  | Order.Market | Order.IOC -> true
  | Order.Limit | Order.FOK  -> incoming.Order.price >= price

let crosses_bid (incoming : Order.order) price =
  match incoming.Order.order_type with
  | Order.Market | Order.IOC -> true
  | Order.Limit | Order.FOK  -> incoming.Order.price <= price

(* Simulate a FOK buy: count how many units are available at or below limit *)
let fok_available_buy limit_price asks =
  AskMap.fold (fun price orders acc ->
    if price <= limit_price then
      acc + List.fold_left (fun a (o : Order.order) -> a + o.quantity) 0 orders
    else acc
  ) asks 0

let fok_available_sell limit_price bids =
  BidMap.fold (fun price orders acc ->
    if price >= limit_price then
      acc + List.fold_left (fun a (o : Order.order) -> a + o.quantity) 0 orders
    else acc
  ) bids 0

let rec match_buy (incoming : Order.order) bids asks fills =
  match AskMap.min_binding_opt asks with
  | None -> (Some incoming, bids, asks, List.rev fills)
  | Some (_, []) ->
    let asks' = AskMap.filter (fun _ lst -> lst <> []) asks in
    match_buy incoming bids asks' fills
  | Some (price, resting :: rest) ->
    if not (crosses_ask incoming price) then
      (Some incoming, bids, asks, List.rev fills)
    else
      let (fill, rem_incoming, rem_resting) = execute_fill incoming resting in
      let new_asks = update_ask_level price rem_resting rest asks in
      (match rem_incoming with
       | None      -> (None, bids, new_asks, List.rev (fill :: fills))
       | Some left -> match_buy left bids new_asks (fill :: fills))

let rec match_sell (incoming : Order.order) bids asks fills =
  match BidMap.min_binding_opt bids with
  | None -> (Some incoming, bids, asks, List.rev fills)
  | Some (_, []) ->
    let bids' = BidMap.filter (fun _ lst -> lst <> []) bids in
    match_sell incoming bids' asks fills
  | Some (price, resting :: rest) ->
    if not (crosses_bid incoming price) then
      (Some incoming, bids, asks, List.rev fills)
    else
      let (fill, rem_incoming, rem_resting) = execute_fill incoming resting in
      let new_bids = update_bid_level price rem_resting rest bids in
      (match rem_incoming with
       | None      -> (None, new_bids, asks, List.rev (fill :: fills))
       | Some left -> match_sell left new_bids asks (fill :: fills))

(* Match with IOC semantics: fills what it can, discards remainder *)
let match_ioc_buy incoming bids asks =
  let (_, new_bids, new_asks, fills) = match_buy incoming bids asks [] in
  (None, new_bids, new_asks, fills)

let match_ioc_sell incoming bids asks =
  let (_, new_bids, new_asks, fills) = match_sell incoming bids asks [] in
  (None, new_bids, new_asks, fills)

(* Match with FOK semantics: fills entirely or cancels with no book changes *)
let match_fok_buy (incoming : Order.order) bids asks =
  let available = fok_available_buy incoming.Order.price asks in
  if available < incoming.Order.quantity then
    (None, bids, asks, [])
  else
    let (_, new_bids, new_asks, fills) = match_buy incoming bids asks [] in
    (None, new_bids, new_asks, fills)

let match_fok_sell (incoming : Order.order) bids asks =
  let available = fok_available_sell incoming.Order.price bids in
  if available < incoming.Order.quantity then
    (None, bids, asks, [])
  else
    let (_, new_bids, new_asks, fills) = match_sell incoming bids asks [] in
    (None, new_bids, new_asks, fills)
