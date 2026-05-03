let make_buy id price qty =
  Order.create_limit id Order.Buy price qty id

let make_sell id price qty =
  Order.create_limit id Order.Sell price qty id

(* ---- Basic book state ---- *)

let test_empty_book () =
  let b = Orderbook.empty in
  Alcotest.(check (option (float 1e-9))) "no best bid" None (Orderbook.best_bid b);
  Alcotest.(check (option (float 1e-9))) "no best ask" None (Orderbook.best_ask b);
  Alcotest.(check (option (float 1e-9))) "no spread"   None (Orderbook.spread b)

let test_add_buy () =
  let (b, fills) = Orderbook.add_order (make_buy 1 100.0 10) Orderbook.empty in
  Alcotest.(check int)          "no fills"   0          (List.length fills);
  Alcotest.(check (option (float 1e-9))) "best bid" (Some 100.0) (Orderbook.best_bid b)

let test_add_sell () =
  let (b, fills) = Orderbook.add_order (make_sell 1 101.0 10) Orderbook.empty in
  Alcotest.(check int)          "no fills"   0          (List.length fills);
  Alcotest.(check (option (float 1e-9))) "best ask" (Some 101.0) (Orderbook.best_ask b)

(* ---- Matching ---- *)

let test_crossing_fill () =
  let (b, _)     = Orderbook.add_order (make_sell 1 100.0 10) Orderbook.empty in
  let (b, fills) = Orderbook.add_order (make_buy  2 100.0 10) b in
  Alcotest.(check int)   "one fill"     1      (List.length fills);
  let f = List.hd fills in
  Alcotest.(check int)   "fill qty"     10     f.Order.quantity;
  Alcotest.(check (float 1e-9)) "fill price" 100.0    f.Order.price;
  Alcotest.(check (option (float 1e-9))) "book bid cleared" None (Orderbook.best_bid b);
  Alcotest.(check (option (float 1e-9))) "book ask cleared" None (Orderbook.best_ask b)

let test_partial_fill () =
  (* Resting ask of 5; incoming buy of 10 → fill 5, leave bid of 5 *)
  let (b, _)     = Orderbook.add_order (make_sell 1 100.0 5) Orderbook.empty in
  let (b, fills) = Orderbook.add_order (make_buy  2 100.0 10) b in
  Alcotest.(check int) "one fill"        1   (List.length fills);
  Alcotest.(check int) "fill qty = 5"    5   (List.hd fills).Order.quantity;
  Alcotest.(check (option (float 1e-9))) "remaining bid" (Some 100.0) (Orderbook.best_bid b)

let test_price_time_priority () =
  (* Two resting sells at 100; buy should fill the first one *)
  let (b, _) = Orderbook.add_order (make_sell 1 100.0 5) Orderbook.empty in
  let (b, _) = Orderbook.add_order (make_sell 2 100.0 5) b in
  let (_, fills) = Orderbook.add_order (make_buy 3 100.0 5) b in
  Alcotest.(check int) "one fill"   1 (List.length fills);
  let f = List.hd fills in
  Alcotest.(check int) "sell side = order #1" 1 f.Order.sell_order_id

let test_market_order () =
  let (b, _)     = Orderbook.add_order (make_sell 1 100.0 10) Orderbook.empty in
  let mkt        = Order.create_market 2 Order.Buy 10 2 in
  let (b, fills) = Orderbook.add_order mkt b in
  Alcotest.(check int) "one fill" 1 (List.length fills);
  Alcotest.(check (option (float 1e-9))) "ask cleared" None (Orderbook.best_ask b)

(* ---- Book operations ---- *)

let test_cancel () =
  let (b, _) = Orderbook.add_order (make_buy 1 100.0 10) Orderbook.empty in
  let b      = Orderbook.cancel_order 1 b in
  Alcotest.(check (option (float 1e-9))) "bid gone" None (Orderbook.best_bid b)

let test_spread () =
  let (b, _) = Orderbook.add_order (make_buy  1  99.0 10) Orderbook.empty in
  let (b, _) = Orderbook.add_order (make_sell 2 101.0 10) b in
  Alcotest.(check (option (float 1e-9))) "spread = 2" (Some 2.0) (Orderbook.spread b)

let test_no_self_trade () =
  (* Two buy orders should never fill against each other *)
  let (b, _)     = Orderbook.add_order (make_buy 1 100.0 10) Orderbook.empty in
  let (_, fills) = Orderbook.add_order (make_buy 2  99.0  5) b in
  Alcotest.(check int) "no fills between buys" 0 (List.length fills)

let test_multi_level_fill () =
  (* Buy sweeps through two ask levels *)
  let (b, _)     = Orderbook.add_order (make_sell 1 100.0 5) Orderbook.empty in
  let (b, _)     = Orderbook.add_order (make_sell 2 101.0 5) b in
  let (b, fills) = Orderbook.add_order (make_buy  3 105.0 10) b in
  Alcotest.(check int) "two fills"    2 (List.length fills);
  Alcotest.(check (option (float 1e-9))) "book empty" None (Orderbook.best_ask b)

let () =
  let open Alcotest in
  run "OrderBook" [
    "basics", [
      test_case "empty book"           `Quick test_empty_book;
      test_case "add buy order"        `Quick test_add_buy;
      test_case "add sell order"       `Quick test_add_sell;
    ];
    "matching", [
      test_case "crossing fill"        `Quick test_crossing_fill;
      test_case "partial fill"         `Quick test_partial_fill;
      test_case "price-time priority"  `Quick test_price_time_priority;
      test_case "market order"         `Quick test_market_order;
      test_case "multi-level sweep"    `Quick test_multi_level_fill;
    ];
    "book ops", [
      test_case "cancel order"         `Quick test_cancel;
      test_case "spread"               `Quick test_spread;
      test_case "no self-trade"        `Quick test_no_self_trade;
    ];
  ]
