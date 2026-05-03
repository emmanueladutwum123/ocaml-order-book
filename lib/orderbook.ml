type t = {
  bids          : Order.order list Matching.BidMap.t;
  asks          : Order.order list Matching.AskMap.t;
  next_timestamp : int;
}

let empty =
  { bids = Matching.BidMap.empty
  ; asks = Matching.AskMap.empty
  ; next_timestamp = 0
  }

let append_to_level price order map_find map_add map =
  let existing = match map_find price map with Some lst -> lst | None -> [] in
  map_add price (existing @ [order]) map

let add_order (order : Order.order) (book : t) : t * Order.fill list =
  let stamped = { order with Order.timestamp = book.next_timestamp } in
  let next_ts = book.next_timestamp + 1 in
  let (rem, new_bids, new_asks, fills) =
    match stamped.Order.side with
    | Order.Buy  ->
      Matching.match_buy  stamped book.bids book.asks []
    | Order.Sell ->
      Matching.match_sell stamped book.bids book.asks []
  in
  let final_bids, final_asks =
    match rem with
    | None -> (new_bids, new_asks)
    | Some o ->
      (match o.Order.side with
       | Order.Buy  ->
         ( append_to_level o.Order.price o
             Matching.BidMap.find_opt Matching.BidMap.add new_bids
         , new_asks )
       | Order.Sell ->
         ( new_bids
         , append_to_level o.Order.price o
             Matching.AskMap.find_opt Matching.AskMap.add new_asks ))
  in
  ({ bids = final_bids; asks = final_asks; next_timestamp = next_ts }, fills)

let cancel_order id book =
  let drop map_module map =
    map_module (fun _price orders ->
      let lst = List.filter (fun o -> o.Order.id <> id) orders in
      if lst = [] then None else Some lst
    ) map
  in
  { book with
    bids = drop Matching.BidMap.filter_map book.bids
  ; asks = drop Matching.AskMap.filter_map book.asks
  }

(* BidMap min_binding is the highest price (because comparator reverses order) *)
let best_bid book =
  match Matching.BidMap.min_binding_opt book.bids with
  | Some (price, _ :: _) -> Some price
  | _ -> None

let best_ask book =
  match Matching.AskMap.min_binding_opt book.asks with
  | Some (price, _ :: _) -> Some price
  | _ -> None

let spread book =
  match (best_bid book, best_ask book) with
  | (Some b, Some a) -> Some (a -. b)
  | _                -> None

let midpoint book =
  match (best_bid book, best_ask book) with
  | (Some b, Some a) -> Some ((a +. b) /. 2.0)
  | _                -> None

let depth book n =
  let take_levels seq =
    seq
    |> Seq.filter_map (fun (price, orders) ->
        let qty = List.fold_left (fun acc o -> acc + o.Order.quantity) 0 orders in
        if qty > 0 then Some (price, qty) else None)
    |> Seq.take n
    |> List.of_seq
  in
  let bid_levels = take_levels (Matching.BidMap.to_seq book.bids) in
  let ask_levels = take_levels (Matching.AskMap.to_seq book.asks) in
  (bid_levels, ask_levels)

let pp_book book =
  let buf = Buffer.create 256 in
  Buffer.add_string buf "=== ORDER BOOK ===\n";
  Buffer.add_string buf (Printf.sprintf "%-12s %-10s | %-10s %-12s\n"
    "BID QTY" "BID" "ASK" "ASK QTY");
  Buffer.add_string buf (String.make 48 '-' ^ "\n");
  let (bids, asks) = depth book 5 in
  let bid_arr = Array.of_list bids in
  let ask_arr = Array.of_list asks in
  let rows = max (Array.length bid_arr) (Array.length ask_arr) in
  for i = 0 to rows - 1 do
    let bid_s =
      if i < Array.length bid_arr then
        let (p, q) = bid_arr.(i) in
        Printf.sprintf "%-12d %-10.2f" q p
      else String.make 24 ' '
    in
    let ask_s =
      if i < Array.length ask_arr then
        let (p, q) = ask_arr.(i) in
        Printf.sprintf "%-10.2f %-12d" p q
      else ""
    in
    Buffer.add_string buf (Printf.sprintf "%s| %s\n" bid_s ask_s)
  done;
  (match spread book with
   | Some s ->
     let mid = match midpoint book with Some m -> m | None -> 0.0 in
     Buffer.add_string buf
       (Printf.sprintf "Spread: %.4f  Mid: %.4f\n" s mid)
   | None -> ());
  Buffer.contents buf

let volume_at_price side price book =
  let sum orders =
    List.fold_left (fun acc o -> acc + o.Order.quantity) 0 orders
  in
  match side with
  | Order.Buy ->
    (match Matching.BidMap.find_opt price book.bids with
     | Some orders -> sum orders
     | None -> 0)
  | Order.Sell ->
    (match Matching.AskMap.find_opt price book.asks with
     | Some orders -> sum orders
     | None -> 0)
