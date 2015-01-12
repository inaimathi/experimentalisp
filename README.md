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

## Various Notes

- We probably want primitive fexprs (that would let us add some additional stuff from inside the language)
- An alternative to `&rest` arguments is just making `list` a special form. Trying it out.
- We should probably separate `fexpr`s into two stages; the expansion and the evaluation of the expansion. If done properly, we can then provide `expand` in the guest language for a pretty cheap macroexpander.
- `the-env`; returns the current environment as an expression
- That environment must have the operations `bind`, `bound?`, `lookup` and `extend` implemented.
	- `lookup` must take an environment and a symbol, and return the value to which said symbol is bound in the given environment.
	- `bound?` must take an environment and a symbol, and return `true` if that symbol is bound in that environment
	- `bind` must take an environment and a symbol, and return a new environment in which that symbol is bound. It should not mutate the existing environment.
	- `extend` must take an environment and add a frame to it

###### Ports:

- We need some kind of port abstraction. In three flavours: `in`, `out` and `in/out`. That'll let us deal with a whole bunch of stuff inside the language, including files, sockets, and standard in/out.

Example:

    (open-file "test.txt")
	> <port IN file:test.txt> ;; should get de-allocated here; no reference to it exists
	(def p (listen localhost 4040))
	> p
	p
	> <port IN socket-server:4040>
	(def client (accept p))
	> client
	client
	> <port IN/OUT socket: [local]/[remote]>
	(read-char! client)
	> \H
	(read-char! (open-file "test.txt")) ;; same deal as above
	> \t
	(def f (open-file "test.txt"))
	> f
	(read-char! f)
	> \t
	(read-char! f)
	> \e
	(close! f)
	> NIL
	(read-char! f)
	> <ERROR: read-from-closed-port (f . <port IN file:test.txt>)>
	
	
	

###### Types:

- Think about procedure types. Specifically, think about cons/car/cdr (you clearly need type variables here, and you need to understand the concept of container types as it applies)
- Sane interface seems to be `type`/`type-of`/`type-check`

Example:

    (type (cons Number Number))
    > (Number . Number)
    (type-of (cons 3 4))
    > (Number . Number)
    (type-check (cons 3 4) (type (cons Number Number)))
    > true
    (type-check (cons 3 4) (type (-> Number Number Number)))
    > false
    (type-check + (type (-> Number Number Number)))
    > true
	(type-of +)
	> ((-> Number Number Number))
	(type-of (list 1 2 3 4 5 6 7 8))
	> (Number)
	(type-of "test")
	> String ;; maybe `(Char)`?

###### Typeclasses:

- Each typeclass is an environment mapping to the different types that implement it to the implementation of the specific methods entailed by that typeclass
- Assuming first-class environments, this'd be fairly simple to implement. We would also need a map of methods to their source typeclass.
- It looks like the generic function approach might be easier to implement, and it would have one fewer lookup level (each function would directly dispatch on arguments, rather than pointing to a typeclass entry)
	- Side-note: it looks like some of the problems that typeclass implementations are running into will be solved, approximately, by moving closer to the generic-function approach
- If we ditch partials and go the multiple arity route, it suddenly starts looking sensible to just allow the same name for functions of different type and arity. That implicitly gives you more flexible generic functions, as long as you can do the more complicated dispatch. That dispatch seems like it'll get difficult. Especially for overlapping functions. How do you sort them? How do you even find all of the relevant ones from a particular environment perspective? It sounds like the sanest approach is to throw your hands up and say "screw it, we're going from `the-env` to the global environment and picking the first matching one on the way". Argleblargh! I need to think harder about this.

###### Errors:

- An environment is a set of symbol bindings AND a set of handler bindings. If an error happens, we match in the handler bindings (the default, top-level environment should have a default handler that drops the user into a prompt)
- Errors should probably get their own type in the LispVal declaration (as well as their own type in the type system)

Example:

    (error test-error 5)
    > <ERROR: test-error 5>
    (raise (error test-error 5))
    > ;; top-level handler effect
    (handle test-error (fn (err) 'caught-error))
    > NIL
    (raise (error test-error 5))
    > caught-error

###### Modules:

- Seemslike they could basically _be_ environments
	- except for the parameterizing thing, for which they'd have to be functions which return environments
	- and except for the fact that they should be able to implicitly return values interned in them
		- should they? a `get` doesn't seem like a bad thing. If the length is concerning you, `.` is also an option
		- there is the difference that we want modules to protect certain values. What we could do is model environments as modules that `provide` all of their values
- You want to support
	- use foo (include an environment in the image; this is basically just an environment merge)
	- from foo import bar (put a specific symbol from an environment into the current one; `(def bar (lookup bar foo))`)
	- from foo import bar as baz (put a specific symbol from an environment into the current one; `(def baz (lookup bar foo))`)
	- parameterizing environments
- That may actually be backwards too; it seems like you could get the same effect by saying "here's my code, run it in this environment". Or maybe "...run with this qualified environment". Worth exploring; this seems like one of the big wins of having first-class envs.
- The environment approach may be a bit _too_ porous. What kind of use cases do you really have for this stuff? Lets start with the obvious ones:
	- If you have a piece of code from some untrsted external source, you want to be able to say something like "Run this in an environment where the IO procedures are switched out with nerfed ones".
	- If you have a piece of code that implements something with the data structure `foo`, you want to be able to say "Run this, but replace `foo` with the equivalently-interacting `bar` (for example, maybe switch out the particular tree-representation in a `Set` implementation)
- By default, a file that doesn't start with a module form will implicitly be treated as though all its contents are in an unparametereized module that provides everything defined in it
- Modules should have multiple option lists you can pass before starting to define symbols. Specifically,
	- `provide`
		- If not given, the module acts as though it `provide`s all defined symbols
		- If given, it instead only provides the specified symbols
		- Maybe an extra option that lets you specify symbols you're NOT exporting (if there's more exports than imports)?
	- values that need to be given to this module at initialization time (for instance, you might want the user to provide some default values or concrete data structure implementations)
	- `require`
		- modules not `ask`ed that need to be present in order for this to work
		- The below example states that `math` depends on nothing (`NIL`). It will actually depend on something like `base`, because it needs to have access to the basic language primitives `fn`, `+`/`-`/`/`/`*`, `fexpr` and `def`.
		- does this need to be user-specified? Seems like we could walk the tree of a modules' symbols and figure it out

Example:

    (module (provide add sub div mul)
         add (fn (a b) (+ a b)) 
         sub (fn (a b) (- a b)) 
         div (fn (a b) (/ a b)) 
         mul (fn (a b) (* a b)) 
         foo (fn (a) (write a)))
    > <module>
    (def math 
      (module (provide add sub div mul)
         add (fn (a b) (+ a b)) 
         sub (fn (a b) (- a b)) 
         div (fn (a b) (/ a b)) 
         mul (fn (a b) (* a b)) 
         foo (fn (a) (write a))))
    > math
    NIL
	(math add)
	> <fn (a b)>
    ((math add) 3 4)
    > 7
    (def plus (math add))
    > NIL
    (plus 3 4)
    > 7
	(provides math)
	> (add sub div mul)
	(math foo)
	> <ERROR: accessing-private-symbol (math foo)>
	(math bar)
	> <ERROR: accessing-nonexistent-symbol (math bar)>
	(requires math)
	> NIL
	(asks math)
	> NIL
	(def trie
        (module (table)
    	 empty (table empty)
    	 insert (fn (trie word)
    		    ;; do trie insertion using table here
    		    )
    	 member? (fn (trie word)
    		     ;; do trie lookup using table here
    		     )
    	 lookup (fn (trie word)
    		    ;; traverse the given trie here
    		    )
    	 remove (fn (trie word)
    		    ;; do trie removal using table here
    		    )))
	> trie
	trie
	> <fn (table)>
	(trie hash-table)
    > <module>
	(trie array-map)
	> <module>
	(trie avl-tree)
	> <module>

###### Counterpoint: [Fuck Modules](http://erlang.org/pipermail/erlang-questions/2011-May/058768.html)

- Forget modules entirely. Allow functions of different arity and type signature, but with the same name to exist, keep track of dependencies on a per-function level.
- Does this buy you anything composition-wise? I could see `start` and `end` functions getting weird, so you still probably want some kind of namespacing.
- Installation/deployment might get a bit easier. Custom compilation might get a bit easier. Give it some though.
- Still need namespaces, probably qualified by username or something, but no parameterization (we're doing module delimitation on a function level)

Pie in the sky example (because, as you'll see, this is hard):

    > (fold + (list 2 3 4 5 6 7 8 9 10))
	;; checking for fold/2  :: (-> (-> Number Number Number) (Number) Number)
	;; found one candidate: inaimathi.core.fold/2 :: (-> (-> a b a) (a) b)
	;; installing...
	54
	> (fold + '(a b c))
	;; checking for +/2 :: (-> Symbol a b)
	;; found one candidate: dxnn.sicp.symbolic.+/2 :: (-> a b a)
	;; installing...
	((a + b) + c)
	> (start 4343)
	;; checking for start/1 :: (-> Number a)
	;; found two candidates
	;;   1. inaimathi.house.start/1 :: (-> Number NIL)
	;;   2. inaimathi.ports.hunchentoot.start/1 :: (-> Number NIL)
	;; Please select 1 or 2
	1
	;; found 48 dependencies for inaimathi.house.start/1
	;; installing...................................................
	Listening on 4343...
	C-c C-c
	>

###### Other examples

    ;;; hello-world.exp
    (def main () (print "Hello world!))

In repl:

    (load (hello-world))
	> <fn ()> ;; the main function of the loaded module
	((load (hello-world)))
	>> Hello world!
	> NIL
	hello-world
	> <module>
	(load-as test (hello-world))
	> <fn ()>
	test
	> <module>
	(load (nonexistent))
	> <ERROR: cant-find-module (nonexistent)>


In command line:

    $ exp hello-world.exp
    Hello world!
	$

The idea is that, since its first non-comment form is not a `module` term, `hello-world.exp` implicitly defines the `hello-world` module, which requires no configuration options and `provide`s all defined forms.
