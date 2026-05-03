type sim_config = {
  num_steps          : int;
  fair_value_start   : float;
  fair_value_vol     : float;
  order_arrival_rate : float;
  market_order_pct   : float;
  spread_width       : float;
}

type sim_result = {
  total_trades : int;
  total_volume : int;
  avg_spread   : float;
  pnl          : float;
  max_position : int;
  sharpe       : float;
}

let default_config = {
  num_steps          = 1000;
  fair_value_start   = 100.0;
  fair_value_vol     = 0.05;
  order_arrival_rate = 0.8;
  market_order_pct   = 0.3;
  spread_width       = 0.20;
}

(* Box-Muller transform for standard normal samples *)
let randn () =
  let u1 = max (Random.float 1.0) 1e-15 in
  let u2 = Random.float 1.0 in
  sqrt (-2.0 *. log u1) *. cos (2.0 *. Float.pi *. u2)

let run_simulation cfg =
  Random.self_init ();
  let book       = ref Orderbook.empty in
  let next_id    = ref 1 in
  let fresh_id () = let id = !next_id in incr next_id; id in

  let position     = ref 0 in
  let cash         = ref 0.0 in
  let total_trades = ref 0 in
  let total_volume = ref 0 in
  let spread_sum   = ref 0.0 in
  let spread_count = ref 0 in
  let max_pos      = ref 0 in
  let pnl_history  = ref [] in
  let fair_value   = ref cfg.fair_value_start in

  let our_bid_id = ref (-1) in
  let our_ask_id = ref (-1) in

  let apply_fills fills =
    List.iter (fun (f : Order.fill) ->
      incr total_trades;
      total_volume := !total_volume + f.quantity;
      if f.buy_order_id = !our_bid_id then begin
        position := !position + f.quantity;
        cash := !cash -. f.price *. float_of_int f.quantity
      end;
      if f.sell_order_id = !our_ask_id then begin
        position := !position - f.quantity;
        cash := !cash +. f.price *. float_of_int f.quantity
      end
    ) fills
  in

  for _step = 1 to cfg.num_steps do
    fair_value := !fair_value +. cfg.fair_value_vol *. randn ();
    let fv = !fair_value in

    (* Cancel stale quotes *)
    if !our_bid_id >= 0 then
      book := Orderbook.cancel_order !our_bid_id !book;
    if !our_ask_id >= 0 then
      book := Orderbook.cancel_order !our_ask_id !book;

    (* Post new market-maker quotes *)
    let half = cfg.spread_width /. 2.0 in
    let bid_price = fv -. half in
    let ask_price = fv +. half in
    let mm_qty    = 10 in

    let bid = Order.create_limit (fresh_id ()) Order.Buy  bid_price mm_qty 0 in
    let ask = Order.create_limit (fresh_id ()) Order.Sell ask_price mm_qty 0 in
    our_bid_id := bid.id;
    our_ask_id := ask.id;
    let (b2, f1) = Orderbook.add_order bid !book in
    book := b2;
    apply_fills f1;
    let (b3, f2) = Orderbook.add_order ask !book in
    book := b3;
    apply_fills f2;

    (* Simulate external order flow *)
    if Random.float 1.0 < cfg.order_arrival_rate then begin
      let side = if Random.bool () then Order.Buy else Order.Sell in
      let oid  = fresh_id () in
      let incoming =
        if Random.float 1.0 < cfg.market_order_pct then
          Order.create_market oid side (1 + Random.int 20) 0
        else
          let sign   = match side with Order.Buy -> 1.0 | Order.Sell -> -1.0 in
          let offset = sign *. Random.float 0.5 in
          Order.create_limit oid side (fv +. offset) (1 + Random.int 20) 0
      in
      let (new_book, fills) = Orderbook.add_order incoming !book in
      book := new_book;
      apply_fills fills
    end;

    (* Track spread *)
    (match Orderbook.spread !book with
     | Some s -> spread_sum := !spread_sum +. s; incr spread_count
     | None   -> ());

    (* Mark-to-market PnL *)
    let mtm = !cash +. float_of_int !position *. fv in
    pnl_history := mtm :: !pnl_history;
    if abs !position > !max_pos then max_pos := abs !position
  done;

  let final_pnl =
    match !pnl_history with p :: _ -> p | [] -> 0.0
  in
  let avg_spread =
    if !spread_count > 0
    then !spread_sum /. float_of_int !spread_count
    else 0.0
  in
  (* Compute daily Sharpe from step returns *)
  let pnl_arr = Array.of_list (List.rev !pnl_history) in
  let n = Array.length pnl_arr in
  let returns =
    if n > 1 then
      Array.init (n - 1) (fun i -> pnl_arr.(i + 1) -. pnl_arr.(i))
    else [||]
  in
  let mean_r =
    if Array.length returns > 0 then
      Array.fold_left (+.) 0.0 returns /. float_of_int (Array.length returns)
    else 0.0
  in
  let std_r =
    let nr = Array.length returns in
    if nr > 1 then
      let v = Array.fold_left (fun acc r -> acc +. (r -. mean_r) ** 2.0) 0.0 returns
              /. float_of_int (nr - 1) in
      sqrt v
    else 1.0
  in
  let sharpe = if std_r > 0.0 then mean_r /. std_r *. sqrt 252.0 else 0.0 in

  { total_trades = !total_trades
  ; total_volume = !total_volume
  ; avg_spread
  ; pnl          = final_pnl
  ; max_position = !max_pos
  ; sharpe
  }

let pp_result r =
  Printf.sprintf
    "=== SIMULATION RESULTS ===\n\
     Trades:       %d\n\
     Volume:       %d\n\
     Avg Spread:   %.4f\n\
     PnL:          %.2f\n\
     Max Position: %d\n\
     Sharpe:       %.3f\n"
    r.total_trades r.total_volume r.avg_spread r.pnl r.max_position r.sharpe
