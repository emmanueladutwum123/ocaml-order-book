let () = Random.self_init ()

let book   = ref Orderbook.empty
let next_id = ref 1
let next_ts = ref 0

let fresh_id () = let id = !next_id in incr next_id; id
let fresh_ts () = let t  = !next_ts in incr next_ts; t

let apply_fills fills =
  List.iter (fun f -> print_endline ("  " ^ Order.pp_fill f)) fills

let cmd_buy price qty =
  let o = Order.create_limit (fresh_id ()) Order.Buy price qty (fresh_ts ()) in
  Printf.printf "Added: %s\n" (Order.pp_order o);
  let (b, fills) = Orderbook.add_order o !book in
  book := b;
  apply_fills fills

let cmd_sell price qty =
  let o = Order.create_limit (fresh_id ()) Order.Sell price qty (fresh_ts ()) in
  Printf.printf "Added: %s\n" (Order.pp_order o);
  let (b, fills) = Orderbook.add_order o !book in
  book := b;
  apply_fills fills

let cmd_market_buy qty =
  let o = Order.create_market (fresh_id ()) Order.Buy qty (fresh_ts ()) in
  Printf.printf "Market BUY qty=%d\n" qty;
  let (b, fills) = Orderbook.add_order o !book in
  book := b;
  apply_fills fills

let cmd_market_sell qty =
  let o = Order.create_market (fresh_id ()) Order.Sell qty (fresh_ts ()) in
  Printf.printf "Market SELL qty=%d\n" qty;
  let (b, fills) = Orderbook.add_order o !book in
  book := b;
  apply_fills fills

let cmd_cancel id =
  book := Orderbook.cancel_order id !book;
  Printf.printf "Cancelled order #%d\n" id

let run_interactive () =
  print_endline "OCaml Limit Order Book — Jane Street style";
  print_endline "Commands:";
  print_endline "  buy <price> <qty>       — add limit buy";
  print_endline "  sell <price> <qty>      — add limit sell";
  print_endline "  market_buy <qty>        — market buy";
  print_endline "  market_sell <qty>       — market sell";
  print_endline "  cancel <id>             — cancel order";
  print_endline "  book                    — display order book";
  print_endline "  quit";
  let running = ref true in
  while !running do
    print_string "\n> ";
    flush stdout;
    let line =
      try input_line stdin
      with End_of_file -> "quit"
    in
    let tokens =
      String.split_on_char ' ' (String.trim line)
      |> List.filter (fun s -> s <> "")
    in
    (match tokens with
     | ["buy";  p; q]       -> cmd_buy        (float_of_string p) (int_of_string q)
     | ["sell"; p; q]       -> cmd_sell       (float_of_string p) (int_of_string q)
     | ["market_buy";  q]   -> cmd_market_buy  (int_of_string q)
     | ["market_sell"; q]   -> cmd_market_sell (int_of_string q)
     | ["cancel"; id]       -> cmd_cancel      (int_of_string id)
     | ["book"]             -> print_string (Orderbook.pp_book !book)
     | ["quit"] | ["exit"]  -> running := false
     | []                   -> ()
     | _                    ->
       print_endline "Unknown command. Try: buy 100.0 10")
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
