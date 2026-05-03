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

val default_config   : sim_config
val run_simulation   : sim_config -> sim_result
val pp_result        : sim_result -> string
