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

(* Update a price level after a fill: put back partial resting or remove empty level *)
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

let rec match_buy (incoming : Order.order) bids asks fills =
  match AskMap.min_binding_opt asks with
  | None -> (Some incoming, bids, asks, List.rev fills)
  | Some (_, []) ->
    (* Empty level shouldn't happen but clean it up *)
    let asks' = AskMap.filter (fun _ lst -> lst <> []) asks in
    match_buy incoming bids asks' fills
  | Some (price, resting :: rest) ->
    let crosses =
      match incoming.Order.order_type with
      | Order.Market -> true
      | Order.Limit  -> incoming.Order.price >= price
    in
    if not crosses then (Some incoming, bids, asks, List.rev fills)
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
    let crosses =
      match incoming.Order.order_type with
      | Order.Market -> true
      | Order.Limit  -> incoming.Order.price <= price
    in
    if not crosses then (Some incoming, bids, asks, List.rev fills)
    else
      let (fill, rem_incoming, rem_resting) = execute_fill incoming resting in
      let new_bids = update_bid_level price rem_resting rest bids in
      (match rem_incoming with
       | None      -> (None, new_bids, asks, List.rev (fill :: fills))
       | Some left -> match_sell left new_bids asks (fill :: fills))
