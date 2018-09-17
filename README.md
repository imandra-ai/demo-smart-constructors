# Demo

This is a demo intended to show the power of smart constructors and private aliases in OCaml. It contains a small library for arithmetic and boolean expressions, as well as pretty-printers, evaluation functions, and a symbolic derivation function.

## Usage:

The easiest way to play with the library is to load it in utop:

    $ dune utop

Then load the `demo.ml` script, and play with expressions:

    utop [0]: e1;;
    - : aexpr = ((2 * x) + (3 * (y ^ 10)))

    utop [1]: #use "demo.ml";;
    (* ... *)


The output of `demo.ml` can be found in [output.txt](./output.txt)

