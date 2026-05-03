type t

val empty          : t
val add_order      : Order.order -> t -> t * Order.fill list
val cancel_order   : int -> t -> t
val best_bid       : t -> float option
val best_ask       : t -> float option
val spread         : t -> float option
val midpoint       : t -> float option
val depth          : t -> int -> (float * int) list * (float * int) list
val pp_book        : t -> string
val volume_at_price : Order.side -> float -> t -> int
