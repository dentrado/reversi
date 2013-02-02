# Reversi

An Reversi/Othello AI written in Clojure.

The game-tree is generated and and searched lazily (minimax with alphabeta pruning)
as described in
[Why functional programming matters](http://www.cse.chalmers.se/~rjmh/Papers/whyfp.html)

Many of the strategy/heuristic functions are based on the strategies described
in chapter 18 of [Paradigms of AI Programming](http://norvig.com/paip.html)

## Usage

FIXME

## Todo

Complete the stability-heuristic, at the moment only parts of the algorithm in
PAIP is implemented and it's only slightly better than the naive
(color counting) heuristic. 

## License

Copyright © 2012 Martin Forsgren

Distributed under the Eclipse Public License, the same as Clojure.
