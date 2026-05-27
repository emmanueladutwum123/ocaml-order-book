let () = Random.self_init ()

let book    = ref Orderbook.empty
let next_id = ref 1
let next_ts = ref 0

let fresh_id () = let id = !next_id in incr next_id; id
let fresh_ts () = let t  = !next_ts in incr next_ts; t

let submit o =
  Printf.printf "Added: %s\n" (Order.pp_order o);
  let (b, fills) = Orderbook.add_order o !book in
  book := b;
  List.iter (fun f -> print_endline ("  " ^ Order.pp_fill f)) fills

let cmd_buy         p q = submit (Order.create_limit   (fresh_id ()) Order.Buy  p q (fresh_ts ()))
let cmd_sell        p q = submit (Order.create_limit   (fresh_id ()) Order.Sell p q (fresh_ts ()))
let cmd_market_buy    q = submit (Order.create_market  (fresh_id ()) Order.Buy    q (fresh_ts ()))
let cmd_market_sell   q = submit (Order.create_market  (fresh_id ()) Order.Sell   q (fresh_ts ()))
let cmd_ioc_buy     p q = submit (Order.create_ioc     (fresh_id ()) Order.Buy  p q (fresh_ts ()))
let cmd_ioc_sell    p q = submit (Order.create_ioc     (fresh_id ()) Order.Sell p q (fresh_ts ()))
let cmd_fok_buy     p q = submit (Order.create_fok     (fresh_id ()) Order.Buy  p q (fresh_ts ()))
let cmd_fok_sell    p q = submit (Order.create_fok     (fresh_id ()) Order.Sell p q (fresh_ts ()))

let cmd_cancel id =
  book := Orderbook.cancel_order id !book;
  Printf.printf "Cancelled order #%d\n" id

let cmd_stats () =
  Printf.printf "Trades: %d  Volume: %d%s\n"
    (Orderbook.total_trades !book)
    (Orderbook.total_volume !book)
    (match Orderbook.vwap !book with
     | Some v -> Printf.sprintf "  VWAP: %.4f" v
     | None   -> "")

let run_interactive () =
  print_endline "OCaml Limit Order Book — price-time priority matching engine";
  print_endline "Commands:";
  print_endline "  buy <price> <qty>        — limit buy";
  print_endline "  sell <price> <qty>       — limit sell";
  print_endline "  market_buy <qty>         — market buy";
  print_endline "  market_sell <qty>        — market sell";
  print_endline "  ioc_buy <price> <qty>    — immediate-or-cancel buy";
  print_endline "  ioc_sell <price> <qty>   — immediate-or-cancel sell";
  print_endline "  fok_buy <price> <qty>    — fill-or-kill buy";
  print_endline "  fok_sell <price> <qty>   — fill-or-kill sell";
  print_endline "  cancel <id>              — cancel order by id";
  print_endline "  book                     — print order book";
  print_endline "  stats                    — trades, volume, VWAP";
  print_endline "  quit";
  let running = ref true in
  while !running do
    print_string "\n> ";
    flush stdout;
    let line = try input_line stdin with End_of_file -> "quit" in
    let tokens =
      String.split_on_char ' ' (String.trim line)
      |> List.filter (fun s -> s <> "")
    in
    (match tokens with
     | ["buy";         p; q] -> cmd_buy         (float_of_string p) (int_of_string q)
     | ["sell";        p; q] -> cmd_sell        (float_of_string p) (int_of_string q)
     | ["market_buy";     q] -> cmd_market_buy                      (int_of_string q)
     | ["market_sell";    q] -> cmd_market_sell                     (int_of_string q)
     | ["ioc_buy";     p; q] -> cmd_ioc_buy     (float_of_string p) (int_of_string q)
     | ["ioc_sell";    p; q] -> cmd_ioc_sell    (float_of_string p) (int_of_string q)
     | ["fok_buy";     p; q] -> cmd_fok_buy     (float_of_string p) (int_of_string q)
     | ["fok_sell";    p; q] -> cmd_fok_sell    (float_of_string p) (int_of_string q)
     | ["cancel";       id ] -> cmd_cancel (int_of_string id)
     | ["book"]              -> print_string (Orderbook.pp_book !book)
     | ["stats"]             -> cmd_stats ()
     | ["quit"] | ["exit"]   -> running := false
     | []                    -> ()
     | _                     -> print_endline "Unknown command.")
  done

let run_simulation steps =
  let cfg = { Market_sim.default_config with num_steps = steps } in
  Printf.printf "Running market simulation (%d steps)...\n%!" steps;
  let result = Market_sim.run_simulation cfg in
  print_string (Market_sim.pp_result result)

let () =
  let args = Array.to_list Sys.argv |> List.tl in
  let steps_of_flag arg =
    if String.length arg > 8 && String.sub arg 0 8 = "--steps=" then
      Some (int_of_string (String.sub arg 8 (String.length arg - 8)))
    else None
  in
  match args with
  | ["--simulate"] ->
    run_simulation 1000
  | ["--simulate"; flag] ->
    (match steps_of_flag flag with
     | Some n -> run_simulation n
     | None   -> run_simulation 1000)
  | _ ->
    run_interactive ()
