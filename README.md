# ocaml-order-book

A functional limit order book engine in OCaml, built as a demonstration of Jane Street–style systems programming.

## What It Is

A complete limit order book (LOB) simulator with:
- **Price-time priority matching** — bids sorted highest-first, asks sorted lowest-first; FIFO within each price level
- **Limit and market orders** — with partial fills and order cancellation
- **Interactive REPL** — enter orders from the command line and watch fills happen
- **Market simulation** — random order flow around a Brownian-motion fair value, with P&L and Sharpe tracking

## Architecture

The codebase is split into four modules with clean `.mli` interfaces:

| Module | Role |
|--------|------|
| `Order` | Algebraic data types: `side`, `order_type`, `order`, `fill` |
| `Matching` | Core matching engine; owns the `BidMap`/`AskMap` map modules |
| `Orderbook` | Immutable book state; delegates matching to `Matching` |
| `Market_sim` | Stochastic order-flow simulation |

**Key design decisions:**

- **Immutable maps** — `Map.Make` with a custom comparator. No mutable arrays or hash tables. Each `add_order` returns a new book value; the old one is unchanged.
- **Algebraic data types** — `Buy | Sell` and `Limit | Market` are distinct variants, not strings or ints. The compiler rejects missing cases.
- **Separate map comparators** — `BidMap` reverses `Float.compare` so `min_binding_opt` always returns the best (highest) bid. `AskMap` uses normal ascending order.
- **Pattern matching throughout** — matching engine uses recursive functions that decompose the map and order structure.

## Build

Requires OCaml ≥ 4.14, opam, and dune.

```bash
# Install dependencies
opam install . --deps-only
# or: opam install alcotest

# Build
dune build

# Run interactive mode
dune exec bin/main.exe

# Run simulation (1000 steps)
dune exec bin/main.exe -- --simulate

# Run simulation (custom steps)
dune exec bin/main.exe -- --simulate --steps=5000

# Run tests
dune test
```

## Interactive Commands

```
> buy 100.50 10       -- add limit buy at 100.50, qty 10
> sell 101.00 5       -- add limit sell at 101.00, qty 5
> market_buy 10       -- market buy qty 10
> market_sell 5       -- market sell qty 5
> cancel 3            -- cancel order #3
> book                -- print current order book
> quit
```

## Example Session

```
> buy 100.0 10
Added: [#1] BUY LIMIT @ 100.00 qty=10 t=0
> buy 100.0 5
Added: [#2] BUY LIMIT @ 100.00 qty=5 t=1
> sell 100.0 8
Added: [#3] SELL LIMIT @ 100.00 qty=8 t=2
  FILL buy#1 x sell#3 @ 100.00 qty=8
> book
=== ORDER BOOK ===
BID QTY      BID        | ASK        ASK QTY
------------------------------------------------
2            100.00     |
Spread: (no ask)
```

## Testing

Tests use [Alcotest](https://github.com/mirage/alcotest) and cover:
- Empty book invariants
- Limit order insertion
- Full and partial crossing fills
- Price-time priority (earliest order at same price fills first)
- Market orders sweep the book
- Order cancellation
- Spread calculation
- No self-trade between same-side orders
- Multi-level sweep (buy sweeps through two ask levels)
