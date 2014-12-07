# experimentaLISP
###### Because I don't have too much to do already

## How to use it

Don't. Not yet, at any rate, I've barely got the reader written. If you absolutely _must_:

- Install [Haskell](https://www.haskell.org/haskellwiki/Haskell)
- Clone this repo

then either

- Run `ghc Experimentalisp.hs`
- Run the resulting binary (which should be named `Experimentalisp`)

or

- Run `ghci Experimentalisp.hs`
- Evaluate the `main` function in the resulting Haskell REPL

## What this is

`experimentaLISP` is an experimental lisp dialect. It's meant to help a few curious individuals explore the implications and interactions of certain language features that may or may not be in favor with the mainstream Lisp communities. At the moment, these include fexprs, first-class environments, first class reader macros, types, purity, various approaches to polymorphic functions, and some ideas about modules.

## What this isn't

`experimentaLISP` is not, and is not trying to be

##### a Scheme implementation

Scheme uses hygienic macros, doesn't concern itself overly with reader macros and doesn't allow full access to environments. It also has [7 RSes](http://www.scheme-reports.org/) worth of baggage/good sense that I'm not interested in re-implementing. If you want to use a Scheme or Scheme-like language, I suggest [Chicken](http://www.call-cc.org/) or [Racket](http://racket-lang.org/).

##### a Common Lisp implementation

Common Lisp uses a compile-time macro system, doesn't concern itself overly with reader macros, doesn't allow full access to environments and doesn't particularly care about avoiding side-effects. It also has some 30 years worth of baggage/good sense that I'm not interested in re-implementing. If you want to use a Common Lisp, I recommend [SBCL](http://www.sbcl.org/) or CCL.

##### a language you'd use in production

By its nature, a project like this is going to be highly unstable and prone to explosions. So, don't use it if you can't afford explosions.

##### especially portable

`experimentaLISP` is not portable, except to the extent that our substrate language (currently Haskell) is portable. If you're looking for a Lisp dialect that you can use in a wide variety of substrates and platforms, I suggest [Shen](http://shenlanguage.org/).
