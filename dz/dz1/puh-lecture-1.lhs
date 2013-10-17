University of Zagreb
Faculty of Electrical Engineering and Computing

PROGRAMMING IN HASKELL

Academic Year 2013/2014

LECTURE 1: Basic language constructs, lists, tuples

v1.0

(c) 2013 Jan Šnajder

==============================================================================

> import Data.Char
> import Data.List

=== DEFINING VALUES AND FUNCTIONS ============================================

> x = 2

Functions are also values, so we can define them similarly. There are no
parentheses surrounding the variable:

> inc x = x + 1

Functions of many variables:

> digits2Number x y = x*10 + y

So, don't write 'digits2Number(x,y)'. That's very non-Haskell!

We can now apply these functions. Again, there are no parentheses:

> y = inc 2
> z = digits2Number 4 2

Function names should be written with the inital letter in lowercase. Other
than that, the usual rules for identifiers apply.

Some built in functions: 'max', 'min', 'succ', 'div', 'mod'.

Infix format:

> w = 25 `div` 2

Now, when we define values in the interactive interpreter, we have to put 'let'
in front:

--> let x = 2
--> let inc x = x + 1

Why this is so will be blatantly clear by the end of the course.

=== STRINGS AND CHARACTERS ===================================================

> name = "Humpty Dumpty"

> letter = 'H'

Concatenating strings:

> s = "One " ++ "two " ++ "three"

You cannot concatenate letters! This won't work:

'a' ++ 'b'

Length will give you the length of a string:

> n1 = length "The quick brown fox jumps over the lazy dog"
> n2 = length s

=== IF-THEN-ELSE AND GUARDS ==================================================

> condDec x = if x > 0 then x - 1 else x 

> foo x = (if even x then x*2 else 2) + 1

Not the same as:

> foo' x = if even x then x*2 else 2 + 1

> bigNumber x = if x >= 1000 then True else False

Avoid explicitly returning True/False; instead, simply return the whole Boolean
expression.

> bigNumber' x = x >= 1000

Playing with strings a bit:

> merge s1 s2 = s1 ++ (if s1 < s2 then " is not " else " is ") ++ s2

> merge2 s1 s2 = 
>   s1 ++ " is " ++ (if s1 < s2 then "not " else "") ++ s2

Guards:

> merge3 s1 s2 
>   | s1 < s2   = s1 ++ " is " ++ s2
>   | otherwise = s1 ++ " is not " ++ s2

> grade score | score < 50 = 1
>             | score < 63 = 2
>             | score < 76 = 3
>             | score < 89 = 4
>             | otherwise  = 5

> showSalary amount bonus
>   | bonus /= 0 = "Salary is " ++ show amount ++ ", and a bonus " ++ show bonus 
>   | otherwise  = "Salary i " ++ show amount

=== EXERCISE 1 ===============================================================

1.1. 
- Define 'concat3' that concatenates three string, but drops the middle one
  if it's shorter than 2 characters (use 'length' function).

1.2.
- Give a simpler definition of 'showSalary', using only one if-then-else
  construct.
- Additionally check that salary is non-negative. If it's negative, return an
  adequate message.

1.3
- Define a function 'showDigit' that spells out a digit (1 -> "one", etc.), 
  using guards.
- Using 'showDigit', define a function 'showDecimal' that spells out a 
  two-digit decimal number (23 -> "twenty three"). 

=== LISTS ====================================================================

> l1 = [1,2,3]

Operator ':' (so-called "cons"):

> l1' = (1:(2:(3:[])))
> l1'' = 1:2:3:[]

List concatenation:

> l2 = [1,2,3] ++ [4,5,6]

> myConcat l1 l2 = l1 ++ l2

Turning elements into singleton lists:

> listify x = [x]
> listify' x = x:[]

Extracting parts of a list: head, tail, init, last.

Taking or dropping the initial part of a list:

> l3 = take 3 [9,2,10,3,4]
> l4 = drop 3 [9,2,10,3,4]

Reversing a list:

> l5 = reverse [1,2,3]

Strings are lists of characters:

> l6 = "this is a list"

> l7 = head l3

> l8 = 'H' : "askell"

Is a string a palindrome?

> isPalindrome s = s == reverse s

Lists cannot be heterogeneous (contain elements of different types). E.g., we
cannot have: [1,'a',3].

Replicate, cycle, and repeat:

> l9  = repeat 'a'
> l10 = cycle [1,2,3]
> l11 = replicate 10 'a'

How to implement 'replicate' with repeat?

> replicate' n x = take n $ repeat x 

List intervals:

> l12 = [1..100]

> l13 = [1,3..999]

> l14 = take 10 [1,3..100]

> l15 = [1..]

> l16 = ['a'..'z']

Laziness in action:

> l17 = take 10 [1..]
> l18 = head [1..]

What's going on here?:

> l19 = tail [1..]
> n = length [1..]

A list without its first and last element:

> trim l = tail (init l)
> trim' l = init $ tail l

> blanks = repeat ' '

> padTo10 s = s ++ take (10 - length s) blanks

Be careful with this:

> l20 = head []

Lists of lists:

> l21 = [[1,2,3],[4,5,6],[7,8,9,10]]
> l22 = ["red","green","blue"]

Again, remember that lists cannot be heterogenous, thus we cannot have
[1,2,[3,4]] nor ["red",'a'].

Concatenating list of sublists:

> l23 = concat l21

Minimum and maximum of a list:

> m1 = minimum [4,1,2,3]

> m2 = maximum "Haskell for the win!"

Looking up elements from a list:

> e1 = [1,3..100] !! 17

> e2 = l21 !! 1 !! 2

Our own implementation of 'chr':

> intToChar i = ['A'..] !! (i - 65)

What happens if i < 65? Let's fix this:

> intToChar' i | i >= 65   = ['A'..] !! (i - 65)
>              | otherwise = error "Index should be at least 65"

Logical operation on lists:

> r1 = and [True,True,False]
> r2 = or [True,True,False]

Removing duplicates with 'nub':

> l24 = nub [1,2,3,1,1,2]
> l25 = nub "Give me every letter only once!"

Sorting a list:

> l26 = sort [1,4,5,6,1,2]
> l27 = sort "Alphabet"

Checking list membership: 'elem' and 'notElem':

> hasA xs = elem 'a' xs

=== EXERCISE 2 ===============================================================

2.1.
- Define a function that returns a list without the first and last three
  elements.

2.2.
- Define a function 'initals s1 s2' that takes a person's name and a surename 
  as input and returns a string consisting of person's initials.
  initials "James" "Bond" => "J. B."

2.3.
- Define a function that concatenates two strings, so that the longest string
  always comes first.

2.4.
- Define a function 'safeHead' that returns an empty list if 'l' is an empty
  list, otherwise it returns its first element.

2.5.
- Define a function 'hasDuplicates' that checks whether a list contains
  duplicate elements (use 'nub').

=== LIST COMPREHENSIONS ======================================================

> doubles = [x*2 | x <- [1..10]]

> doublesFromTo a b = [x*2 | x <- [a..b]]

> sums1 = [x + y | x <- [1..10], y <- [1..10]]

> sums2 = [x + y | x <- [1..10], y <- [1..10], x < y]

> sums3 = [x + y | x <- [1..10], y <- [1..10], x < y, odd x || even y]

List of sublist lengths:

> lengths xss = [length xs | xs <- xss]

> totalLength xss = sum $ lengths xss

Combining strings:

> food = [s1 ++ " " ++ s2 | 
>         s1 <- ["cold","warm","fresh"], 
>         s2 <- ["cake","soup","salad"]]

Since strings are actually lists of characters, we can do list comprehensions
with strings:

> codes = [ [c1,c2] | c1 <- "abc", c2 <- "123"]

> caesarCode s = [succ c | c <- s, c /= ' ']

> onlyDigits s = [c | c <- s, isDigit c]

> upperCase s = [toUpper c | c <- s]

=== EXERCISE 3 ===============================================================

3.1
- Redefine 'doublesFromTo' so that it also works when b<a.

3.2.
- Redefine 'ceasarCode n' so that it shifts all letters a specified number of 
  positions 'n', converts all input to lowercase, and ensures that letters 
  remain within the ['a'..'z'] interval.

==============================================================================

(TO BE CONTINUED.)

