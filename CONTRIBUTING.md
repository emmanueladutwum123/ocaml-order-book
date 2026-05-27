# Contributing

## Build

```bash
opam install . --deps-only --with-test
dune build
dune test
```

## Adding order types

New order types go in `lib/order.ml` (add the variant) and `lib/matching.ml`
(add a `match_*` function). Export via `lib/matching.mli` and dispatch in
`lib/orderbook.ml`'s `add_order`. Add Alcotest cases in `test/test_orderbook.ml`.

## Running the REPL

```bash
dune exec bin/main.exe
```

## Running the market simulation

```bash
dune exec bin/main.exe -- --simulate --steps=1000
```
