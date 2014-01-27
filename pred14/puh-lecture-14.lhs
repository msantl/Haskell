University of Zagreb
Faculty of Electrical Engineering and Computing

PROGRAMMING IN HASKELL

Academic Year 2013/2014

LECTURE 14: Miscellaneous

v1.0

(c) 2013 Jan Šnajder

==============================================================================

> import Data.List
> import Control.Monad
> import Data.Maybe
> import System.IO

== STRICTNESS ================================================================

Haskell is a lazy language. It won't evaluate an expression until this is
required.

> e1 = head [1,2..]

How does that work? Instead of evaluating the complete list, Haskell generates
a so-called THUNK or SUSPENSION -- a pointer to the expression and a data
structure containing all required to evaluate the expression. A thunk will only
be evaluated when required.

Another example of laziness:

  (&&) :: Bool -> Bool -> Bool
  False && _ = False
  True  && x = x

This function does not evaluate it's second argument unless the first argument
is 'True'. We say that the function is NON-STRICT in its second argument. This
means that 'False && undefined' will work, but 'undefined && False' won't.
Similarly, function (||) is non-strict in its second argument, so 'True ||
undefined' will work.

A side note:
We've been a bit sloppy here because we equate NON-STRICTNESS with LAZINESS.
Strictness is a concept from lambda calculus. NON-STRICTNESS means an
expression can be reduced to a value, even if some of its parts are undefined.
LAZY EVALUATION is one of the possible ways to implement non-strictness. It
means that we only evaluate expressions when needed, and create thunks in the
meantime. Read more here:
http://www.haskell.org/haskellwiki/Lazy_vs._non-strict

While in most cases we want the evaluation to be lazy, occasionally lazy
evaluation becomes problematic. For example, we absolutely need lazy evaluation
for this to work:

> filterOdd :: [a] -> [a]
> filterOdd = map snd . filter (odd . fst) . zip [0..]

But here lazy evaluation is harmful:

> filesize :: FilePath -> IO Int 
> filesize f = withFile f ReadMode $ \h -> do 
>   s <- hGetContents h 
>   return $ length s

The problem is that 'hGetContents' reads from the file lazily, and hence it also
produces its result (a string) lazily. Although the 'length' function will
force 'hGetContents' to produce the entire string, the problem is that 'length'
is also lazy and won't be evaluated unless absolutely necessary. Consequently,
by applying 'filesize' to file name nothing will be read in from a file. It is
only when we want to print out the result on screen that the file will
eventually be read. But the problem is that, when this happens, the file handle
will already be closed and 'hGetContents' will return an empty string.

Another, perhaps more insightful example:

> e2 = foldl (+) 0 [0..100]
> e2' = foldl (+) 0 [0..1000000000]

We use left fold here, which is defined as follows:

  foldl :: (a -> b -> a) -> a -> [b] -> a
  foldl f z []     = z                  
  foldl f z (x:xs) = foldl f (f z x) xs

Recall that left fold is space efficient because its tail recursive. But
despite this, we have a problem here because 'f z x' won't really be evaluated.
Instead, it will build up thunks:

  foldl (+) 0 [1..10] =>
  foldl (+) 0 (1:[2..10]) =>
  foldl (+) (0+1) (2:[3..10]) =>
  foldl (+) (0+1+2) (3:[4..10]) =>
  foldl (+) (0+1+2+3) (4:[5..10]) => ...

This causes a so-called MEMORY LEAKAGE. The expressions builds up in memory,
consuming more and more space, until it's finally evaluated. The problem is
that the thunks consume more memory than the values they would evaluate to. To
prevent this from happening, we need a way to force the evaluation of 'f z x'.
There's a function that does that:

  seq :: a -> b -> b

Function 'seq' evaluates its first argument before it returns its second
argument.

For example:

> e3 = let x = undefined in 2
> e4 = let x = undefined in x `seq` 2
> e5 = let x = undefined in x `seq` snd (x,5)

We can now define a strict version of fold:
  
  foldl' f z []     = z
  foldl' f z (x:xs) = let z' = f z x in z' `seq` foldl' f z' xs

> e6 = foldl' (+) 0 [0..1000000000]

And here the solution to the first problem:

> filesize' :: FilePath -> IO Int 
> filesize' f = withFile f ReadMode $ \h -> do 
>   s <- hGetContents h 
>   s `seq` return (length s) 

We can also define a strict version of the application operator ($):

  ($!) :: (a -> b) -> a -> b
  f $! x = x `seq` f x

'f $! x' will first evaluate the argument 'x', and then apply a function to it.

For example:

> e8 = let x = undefined in const 5 x
> e9 = let x = undefined in const 5 $! x

It is important to know that `seq` does not evaluate "too deep". Expressions
have structure, and `seq` only scratches the surface.

> e10 = let x = undefined in x `seq` 2
> e11 = let x = Just undefined in x `seq` 2

More precisely, `seq` only evaluates the outmost data constructor (we say that
it evaluates an expression to a WEAK HEAD NORMAL FORM). When this is not
enough, we need to make sure that 'seq' is applied recursively to
subexpressions. You can use the 'deepseq' package for this:
http://hackage.haskell.org/package/deepseq

== MODULES ===================================================================

A Haskell program (an executable or a library) is typically a collection of
modules. The standard modules are defined in a library called "Haskell
Hierarchical Libraries":
http://www.haskell.org/ghc/docs/latest/html/libraries/

We already know how to import modules or specific functions from modules:

  import Data.List
  import Data.List (lookup)           -- imports only 'lookup'
  import Data.List hiding (lookup)    -- imports all but 'lookup'
  import qualified Data.Map
  import qualified Data.Map as M

Defining your own modules in Haskell is simple. Every module is defined in a
separate file. Modules can be organized hierarchically, as in 'Data.List' or
'Data.Time.Calendar'. To accomplish this, just need to organize the files in a
corresponding folder hierarchy. For example:

Data/List.hs           ==> module 'Data.List'
Data/Time.hs           ==> module 'Data.Time'
Data/Time/Calendar.hs  ==> module 'Data.Time.Calendar'

A module defines functions, data structures, and data types. A module typically
exports only certain definition that constitute the API, while others stay
hidden (helper functions etc.).

For example:

--- file Data/Trie.hs ---

module Data.Trie (
  Trie,
  insert, 
  lookup,
  remove,
  ... ) where

data Trie k a = 
  Leaf a | Branch [(k,Trie k a)] deriving (Eq,Ord,Show,Read)

insert :: Eq k => [k] -> a -> Trie k a -> Trie k a
...

lookup :: Eq k => [k] -> Trie k a -> Maybe a
...

remove :: Eq k => [k] -> Trie k a -> Trie k a
...

-----------------------------

In the above example, only the type constructor 'Trie' is exported, but not
its data constructors 'Leaf' and 'Branch'. This means that the user of the
module won't be able to use these data constructors. Consequently, she won't be
able to build her own tries nor will she be able to pattern match on a trie. In
this way we have restricted the use of our structure through the API: the user
can only invoke the operations that we have provided. If we later decide to
change the internal representation of a trie, the API will stay the same.

If we want to export the data structures as well, we do it like this:

module Data.Trie (
  Trie (..),

If we want to export only some of them, we do it like this:
  
module Data.Trie (
  Trie (Leaf),

== PACKAGES ==================================================================

Modules are distributed in packages. A package contains a module or a number of
module. Again, this can be executables or libraries. To install packages, you
need CABAL:

http://www.haskell.org/haskellwiki/Cabal-Install

== RESOURCES =================================================================

* Haskell home:
  http://www.haskell.org

* GHC
  http://www.haskell.org/ghc/

Libraries

* Haskell Hierarchical Libraries
  http://www.haskell.org/ghc/docs/latest/html/libraries/

* Hackage
  http://hackage.haskell.org

* Hoogle
  http://www.haskell.org/hoogle/

* Hayoo! (searches over Hackage)
  http://holumbus.fh-wedel.de/hayoo/hayoo.html

Learning

* Try Haskell
  http://tryhaskell.org/

* Learn you a Haskell for a Great Good
  http://learnyouahaskell.com/

* FPComplete School of Haskell
  https://haskell.fpcomplete.com/school

* Haskell Wiki
  http://en.wikibooks.org/wiki/Haskell

* Real World Haskell
  http://book.realworldhaskell.org/read/

* Parallel and Concurrent Programming in Haskell
  http://chimera.labs.oreilly.com/books/1230000000929/index.html

* 24 days of Haskell
  http://ocharles.org.uk/blog/

Community

* Haskell Reddit
  http://www.reddit.com/r/haskell

* Haskell mailing lists
  http://www.haskell.org/haskellwiki/Mailing_Lists

* Haskell IRC channel
  http://www.haskell.org/haskellwiki/IRC_channel

* Haskell Communities and Activities Report (HCAR)
  http://www.haskell.org/haskellwiki/Haskell_Communities_and_Activities_Report

* Haskell Weekly News
  http://planet.haskell.org/

* Monad Reader
  http://themonadreader.wordpress.com/

* Haskellers
  http://www.haskellers.com/

== WHAT NEXT? ================================================================

First, you need to consolidate your working knowledge of Haskell.

Start by reading "Learn you a Haskell for a Greater Good". If you feel
confident in the material covered in this course, you can immediately start
with chapter 10.

You can then deepen your knowledge by reading "Real World Haskell".

From there you can take basically take two different paths: a practical one
(advanced Haskell programming, functional programming design) or a theoretical
one (theory underlining FP, lambda calculus, category theory, functional data
structures). You can, of course, take a middle road.

== PUH 2 =====================================================================

PUH 2 will take it from where PUH left off. It will focus on two aspects:

* Advanced Haskell programming ==> "Real World Haskell" and beyond
* Functional algorithms design ==> "Pearls of Functional Algorithm Design"

Similar to PUH, it's going to be more technical and less theoretical (a
programming course rather than a theoretical CS course).

Desiderata:

* monad transformers
* applicative programming
* monadic parser combinators
* the unavoidable stuff: GUI, networking, databases, etc.
* GHC language extensions
* parallel and concurrent programming
* foreign function interface
* profiling and code optimization
* template Haskell
* arrows
* generalized algebraic types
* generics
* lenses
* pipes
* zippers
* useful Hackage packages
* zygohistomorphic prepromorphisms

