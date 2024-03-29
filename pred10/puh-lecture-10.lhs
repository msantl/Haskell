University of Zagreb
Faculty of Electrical Engineering and Computing

PROGRAMMING IN HASKELL

Academic Year 2013/2014

LECTURE 10: Input/output operations 1

v1.0

(c) 2013 Jan Šnajder

==============================================================================

> import Data.Char
> import Data.List
> import Control.Monad

== IO ACTIONS ================================================================

Everything we've been doing so far was PURE FUNCTIONAL programming. We have
never introduced any SIDE EFFECTS: every function returned a result that was
the same regardless of the context in which the function was applied, and also
no function has altered the system state. For example, when we wanted to add an
element to a list, the function did not change the original list, but instead
it returned a new list with the an element appended to it.

IO operations are different because they *must* result in a side effect. Input
functions (e.g., reading from a keyboard) cannot always return the same value,
while output functions (e.g., writing to a screen) must be able to change the
system state. So, now we're going to learn how to write impure code.

In Haskell, IO operations are accomplished using ACTIONS. Actions are in fact
values of the type

  IO a

where 'a' is the type of action's return value. E.g.:

  IO String
  IO Int
  IO (Int,String)
  IO ()

The last type is for actions that return nothing. The '()' type is called UNIT.
It is actually an empty tuple type (a "0-ary tuple"). An empty tuple (the
value) is also written as '()'. So, value '()' is of type '()', in other words:
'() :: ()'.

'IO' is actually a type constructor of '* -> *' kind. We can think of it as a
"box" that stores actions that are not functionally pure.

In Haskell, we pay a lot attention to separating the purely functional (non-IO)
code from impure (IO) code. As a matter of fact, because all impure actions are
wrapped up into the 'IO' type, this separation occurs naturally.

(Remark: the 'IO' type is actually a MONAD. More precisely: the 'IO' type is an
instance of the 'Monad' type class. We often simply say "the IO monad". But
let's forget this for now.)

Finally, here you have "Hello, world!" in Haskell:

> main = putStrLn "Hello, world!"

This is an action. What is it's type? What is the type of the 'putStrLn'
function?

  main :: IO ()
  putStrLn :: String -> IO ()

Function 'putStrLn' takes a string and returns an action (that outputs the
string to the screen).

You can compile the above program (using 'ghc --make') and run it. Every
program that has a 'main' function can be compiled (and linked). You must have
no more than one 'main' function. When the program is run, the 'main' function
is executed.

The 'main' function can invoke other functions that also do some actions. E.g.,
we could have had:

  main = sayHelloToTheWorld
  sayHelloToTheWorld = putStrLn "Hello, world!"

Now, doing a single action is rarely enough. If we need to execute many
actions, we need to sequence them into one action. We do this using a special
language construct 'do':

> main2 = do
>   putStrLn "here"
>   putStrLn "we"
>   putStrLn "go"

All statements in a do block must be aligned one below the other. The 'main2'
function executes three actions, but these are tied into a single action, so
it's type is again 'IO ()'. The actions within a do block are executed
sequentially, from top to bottom, as in imperative languages.

Another example:

> main3 = do
>   putStrLn "Enter you lucky number"
>   number <- getLine
>   putStrLn $ "I guess your lucky number is " ++ number

What is the type of 'getLine'?

The '<-' operator takes an action on its right side and a variable on its left
side and stores the return value of the action into the variable. Otherwise
said, the '<-' operator unwraps the return value of type 'IO a' from an action
into a value type of 'a'. This can only be done within a do block of a function
whose type is 'IO a'.

Will the following work?

  foo = "Your name is " ++ getLine

Or this:

  main4 = do
    putStrLn "Enter your lucky number"
    putStrLn $ "Your lucky number is " ++ getLine

Function 'putStrLn' expects a string. Similarly, function (++) expects a
string. But function 'getLine' is not of type 'String' but of type 'IO String'.
To get to the string, we need to unwrap it using the '<-' operator.

Here's something that will work:

> askNumber :: IO String
> askNumber = do
>   putStrLn "Enter your lucky number"
>   getLine

> main5 :: IO ()
> main5 = do
>   number <- askNumber
>   putStrLn $ "Your lucky number is " ++ number

The return value of a the whole do block is the return value of the last
action. This is why the return value of 'askNumber' is a string returned by the
'getLine' action.

Every action results in a value that we can store. But we don't necessarily
need to store it. For example:

> main6 :: IO ()
> main6 = do
>   x <- putStrLn "Enter your lucky number"
>   getLine
>   putStrLn "Thanks!"

The result of the 'putStrLn' action is '()', so it doesn't make sense to store
it. The result of the 'getLine' action is a string but we chose not to store it
(this makes sense if want the user to press any key before we continue).

== EXERCISE 1 =================================================================

1.1.
- Define a 'main' function that reads in two strings and prints them out
  concatenated and reversed.

1.2.
- Write a function 'threeNumbers' that reads in three numbers and prints out
  their sum.
- Call this function from within a 'main' function, then compile and run the
  program.

== RETURN =====================================================================

What if we wish to modify a string taken from the input, before we return if
from the action? We can do this as follows:

> askNumber2 :: IO String
> askNumber2 = do
>   putStrLn "Enter your lucky number"
>   number <- getLine
>   return $ number ++ "0"

The function 'return :: a -> IO a' gets a value and turns into an action
result.

'return' need not be the last action. It can appear anywhere in a
do block. 'return' does not cause a return from a function -- it just wraps up
a value into a 'IO' type (or, more generally, into a Monad). E.g.:

> askNumber3 :: IO String
> askNumber3 = do
>   putStrLn "Enter your lucky number"
>   number <- getLine
>   return $ number ++ "0"
>   getLine

This doesn't make a lot of sense but illustrates the point. The return value of
a do block is the return value of the last action, regardless whether a
'return' function has appeared before that. So, it is nothing like a return in
an imperative function (it does not really return from function)!

(Obviously, "return" is not the best choice for the name of this function. A
name like "wrap" or "inject" would have been a better choice.)

We can of course branch the control flow within an action:

> askNumber4 :: IO String
> askNumber4 = do
>   putStrLn "Enter your lucky number"
>   number <- getLine
>   if number == "" then return "7"
>     else return number

What is important is that both branches return an action of the same type,
which turns an 'if-then-else' into an action. In the above example, because
if-then-else is the last action in a do block, its type must be 'IO String', as
specified by the type signature.

We can also write the above function like this:

> askNumber5 :: IO String
> askNumber5 = do
>   putStrLn "Enter your lucky number"
>   number <- getLine
>   return $ if number == "" then "7" else number

Would the following work?

  askNumber6 :: IO String
  askNumber6 = do
    putStrLn "Enter your lucky number"
    number <- getLine
    if number == "" then "7" else number

And why is this not good?
  
  main7 :: IO ()
  main7 = do
    putStrLn "Enter your lucky number"
    number <- getLine
    return number

We can of course use recursion:

> askNumber7 :: IO String
> askNumber7 = do
>   putStrLn "Enter your lucky number"
>   number <- getLine
>   if number == "" then askNumber7 else return number 

This is alright because both branches are actions. Now, what if we want to
execute several actions in one of the branches? We again need to sequence them
into a single action using a do block:

> askNumber8 :: IO String
> askNumber8 = do
>   putStrLn "Enter your lucky number"
>   number <- getLine
>   if number == "" then do
>     putStr "No input! "
>     askNumber8 
>   else return number 

== EXERCISE 2 =================================================================

2.1.
- Define a function 'threeStrings' that reads in three strings and outputs them
  to the screen as one string, while it returns its total length.
  treeStrings :: IO Int

2.2.
- Define a function 'askNumber9' that reads in a number and returns that number
  converted into an 'Int'. Input should be repeated until the user enters a
  number (a string containing only digits).
    askNumber9 :: IO Int
- Define a function 'main' that calls 'askNumber9' and outputs the number to
  the screen.
- Build and run the program.

2.3.
- Define a function 'askUser m p' that returns an action that prints out 'm',
  reads in a string from the input, repeats the input until the input
  string satisfies the function 'p', and then returns the input string.
    askUser :: String -> (String -> Bool) -> IO String
- Generalize this function to
    askUser' :: Read a => String -> (String -> Bool) -> IO a
- Define a 'main' function that prints out the read-in value to the screen.
- Build and run the program.

2.4.
- Define a function that reads in strings until the user inputs an empty
  string, and then returns a list of strings received as input.
    inputStrings :: IO [String]

== WHERE & LET ===============================================================

Within IO actions, we can use the 'let' command to assign values to variables:

> askName1 :: IO String
> askName1 = do
>   s1 <- getLine
>   s2 <- getLine
>   let forename = map toUpper s1
>       lastname = map toUpper s2
>   return $ forename ++ " " ++ lastname

Note that the expression on the RHS of 'let' must be pure (non-IO). In other
words, it cannot be wrapped up in an 'IO' type. If we want to assign to a
variable a value that is the return of an action, we need to use '<-'.

So, this is wrong:

  askName2 :: IO String
  askName2 = do
    s1 <- getLine
    s2 <- getLine
    forename <- map toUpper s1
    lastname <- map toUpper s2
    return $ s1 ++ " " ++ s2

This is wrong, too:
  
 askName3 :: IO String
 askName3 = do
   let s1 = getLine
       s2 = getLine
       forename = map toUpper s1
       lastname = map toUpper s2
   return $ s1 ++ " " ++ s2

You can also use a 'where' block, but it has to be placed outside of a do
block:

> askName4 :: IO String
> askName4 = do
>   s1 <- getLine
>   s2 <- getLine
>   return $ upperCase s1 ++ " " ++ upperCase s2
>   where upperCase = map toUpper

== COMMON IO FUNCTIONS ========================================================

  putStr :: String -> IO ()
  putStrLn :: String -> IO ()
  putChar :: Char -> IO ()
  print :: Show a => a -> IO ()

For example, 'putStr' implemented using 'putChr':

> putStr1 :: String -> IO ()
> putStr1 [] = return ()
> putStr1 (x:xs) = do
>   putChar x
>   putStr1 xs

The difference between 'print' and 'putStr' is that the former can be applied
to any type that is a member of 'Show' type class. Actually, 'print' is defined
as 'putStrLn . show'. As a consequence, if we apply print on a string, it will
be printed with quotes:

  print "Hello, world!"

In the 'Control.Monad' module you will find a number of functions for managing
the actions:

  when :: Monad m => Bool -> m () -> m ()
  sequence :: Monad m => [m a] -> m [a]
  mapM :: Monad m => (a -> m b) -> [a] -> m [b]
  forever :: Monad m => m a -> m b

Here 'm' is any type that is a member of the 'Monad' type class. We've already
said that 'IO' is a member of this class, so all the above functions will work
with IO actions.

The 'when' function executes the given action if the condition is met,
otherwise it executes 'return ()'.
  
> main8 = do
>   input <- getLine
>   when (input == "") $ 
>     putStrLn "Input is empty"

which is the same as:

> main9 = do
>   input <- getLine
>   if (input == "") then 
>     putStrLn "Input is empty"
>   else return ()

The 'sequence' function takes a list of actions and returns a single action
that runs these actions in sequence and returns a list of their return values.

On other words, this:

> action1 = sequence [getLine,getLine] 

is the same as

> action2 = do
>   x1 <- getLine
>   x2 <- getLine
>   return [x1,x2]

> main10 = do
>   putStrLn "Introducir tres números"
>   xs <- sequence [getLine,getLine,getLine]
>   putStrLn $ "Gracias. Ha introducido " ++ unwords xs

What about this?

  main11 = do
     xs <- sequence [putStrLn "Introducir tres números",getLine,getLine,getLine]
     putStrLn $ "Gracias. Ha introducido " ++ unwords (tail xs)

'sequence' is useful for mapping an IO action over a list:

> main12 = do
>   sequence $ map print [1..10]

or, more succinctly (because we only have one action):

> main13 = sequence $ map print [1..10]

What is the type of the above function?

Instead, we can define it like this:

> main14 = do
>   sequence $ map print [1..10]
>   return ()

Is this ok?

> main15 = do
>   sequence $ map (putStrLn . show) [1..10]
>   return ()

Because 'sequence $ map' is required quite often, there is a standard function
'mapM' that does exactly that:

> main16 = mapM print [1..10]

If don't want to collect the results of the individual actions, but only care
about their side effects, we use the function 'mapM_' instead:

> main17 = mapM_ print [1..10]

The difference between 'mapM' and 'mapM_' is visible from their signatures:

  mapM  :: Monad m => (a -> m b) -> [a] -> m [b]
  mapM_ :: Monad m => (a -> m b) -> [a] -> m ()

There's a similar function called 'forM'. It is the same as 'mapM', but with
flipped arguments, so that the list comes first and then comes the action. This
is reminiscent of "foreach" loops in imperative languages.

> main18 = forM [1..10] print

There's also a 'forM_' version that discards the resulting values:

> main19 = forM_ [1..10] print

Often you will find yourself using 'forM' in combination with a lambda
expression, like here:

> main20 = do
>   ys <- forM [1..10] $ \x -> do
>     putStrLn $ "Input " ++ show x ++ "th number"
>     y <- getLine
>     return $ read y
>   putStrLn $ "The sum is " ++ show (sum ys)

Function that repeats an action a given number of times:

  replicateM :: Monad m => Int -> m a -> m [a]
  replicateM_ :: Monad m => Int -> m a -> m ()

Another interesting function is 'forever':

> main21 = forever $
>   putStrLn "Forever young"

Take a quiet moment to think about the type of this function.

== EXERCISE 3 =================================================================

3.1.
- Define a function that reads in a number, then reads in that many
  strings, and finally prints these string in reverse order.

3.2.
- Give recursive definitions for 'sequence' and 'sequence_'.

3.3.
- Give a recursive definitions for 'mapM' and 'mapM_'.

3.4.
- Define a function that prints out the Pythagorean triplets whose all sides
  are <=100. Every triplet should be in a separate line.

