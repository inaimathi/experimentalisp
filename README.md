# experimentaLISP
###### Because I don't have too much to do already

## Status

#### Haskell/JS

- working REPL
- working fexprs

- still need to clean up the eval and apply functions. The environment is inherently mutating, and we need to reflect that (or pull some rather fancy footwork with respect to mutual recursion)

#### Racket

- working REPL
- working fexprs
- working partials

- still need to write a separate reader (we want to explore some reader macro territory, which means we'll be dealing with things that aren't standard LISP forms)

#### SML

- half a reader

- still need to write everything

## Various Notes

- We need some kind of port abstraction. In three flavours: `in`, `out` and `in/out`. That'll let us deal with a whole bunch of stuff inside the language, including files, sockets, and standard in/out.
- We should probably separate `fexpr`s into two stages; the expansion and the evaluation of the expansion. If done properly, we can then provide `expand` in the guest language for a pretty cheap macroexpander.
- `the-env`; returns the current environment as an expression
- That environment must have the operations `bind`, `bound?` and `lookup` implemented.
	- `lookup` must take an environment and a symbol, and return the value to which said symbol is bound in the given environment.
	- `bound?` must take an environment and a symbol, and return `true` if that symbol is bound in that environment
	- `bind` must take an environment and a symbol, and return a new environment in which that symbol is bound. It should not mutate the existing environment.

- Typeclasses:
	- Each typeclass is an environment mapping to the different types that implement it to the implementation of the specific methods entailed by that typeclass
	- Assuming first-class environments, this'd be fairly simple to implement. We would also need a map of methods to their source typeclass
	- It looks like the generic function approach might be easier to implement, and it would have one fewer lookup level (each function would directly dispatch on arguments, rather than pointing to a typeclass entry)
		- Side-note: it looks like some of the problems that typeclass implementations are running into will be solved, approximately, by moving closer to the generic-function approach

## How to use it

Don't.

Not yet, at any rate, I've barely got the reader written. If you absolutely _must_:

1. Clone this repo

#### JavaScript version

2. Open `Experimentascript.html` in a browser _(note that you still need [Haskell](https://www.haskell.org/haskellwiki/Haskell) and [Haste](http://haste-lang.org/) to actually edit it, but you can run it without those)_

#### Racket version

2. Install [Racket](http://racket-lang.org/)
3. Run `racket -t experimentalisp.rkt -m`

#### Haskell version

2. Install [Haskell](https://www.haskell.org/haskellwiki/Haskell)

###### Compiled

3. Run `ghc Experimentalisp.hs`
4. Run the resulting binary (which should be named `Experimentalisp`)

###### Interpreted

3. Run `ghci Experimentalisp.hs`
4. Evaluate the `main` function in the resulting Haskell REPL

## What this is

`experimentaLISP` is an experimental lisp dialect. It's meant to help a few curious individuals explore the implications and interactions of certain language features that may or may not be in favor with the mainstream Lisp communities. At the moment, these include fexprs, first-class environments, first class reader macros, types, purity, various approaches to polymorphic functions, and some ideas about modules.

## What this isn't

`experimentaLISP` is not, and is not trying to be

##### a Scheme implementation

Scheme uses hygienic macros, doesn't concern itself overly with reader macros and doesn't allow full access to environments. It also has [7 RSes](http://www.scheme-reports.org/) worth of baggage/good sense that I'm not interested in re-implementing. If you want to use a Scheme or Scheme-like language, I suggest [Chicken](http://www.call-cc.org/) or [Racket](http://racket-lang.org/).

##### a Common Lisp implementation

Common Lisp uses a compile-time macro system, doesn't concern itself overly with reader macros, doesn't allow full access to environments and doesn't particularly care about avoiding side-effects. It also has some 30 years worth of baggage/good sense that I'm not interested in re-implementing. If you want to use a Common Lisp, I recommend [SBCL](http://www.sbcl.org/) or [CCL](http://ccl.clozure.com/).

##### a language you'd use in production

By its nature, a project like this is going to be highly unstable and prone to explosions. Use it to sate your curiosity about certain language features, or perhaps as a starting point for your own experiments, not as the foundation for large software projects.

##### especially portable

`experimentaLISP` is not portable, except to the extent that our substrate languages are portable. Also, while there _is_ a feature list comparison up top, there may be variations between implementations. We're using a multi-language development approach with an aim to test out implementation details from different perspectives, rather than to have portable infrastructure. If you're looking for a Lisp dialect that you can use in a wide variety of substrates and platforms, look into [Shen](http://shenlanguage.org/).
