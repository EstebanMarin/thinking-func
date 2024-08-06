# What haskellers do not want you to know. The `Yin` and `Yang` of Functional programming

I am for one a person that is very excited about Functional Programming and Haskell.
Normally you will see me yaping on social media about the beauty about FP, how its the best thing ever invented since
sliced bread.

For all the great thing that functional programming give us, it also comes, as anything in life, with a cost.
And funny enough for all the great things it claims like, inmutability, easy to reason about, and consiceness. Those 
same things is what makes FP painful. Ergo Yin-Yang

But as with everything in life there is `Yin` and `Yang`

## What is functional programming in a nutshell

- Functional programming is a method or program construction that emphasises function and their application, rather than commands and their execution
- Functional programming uses simple mathematical notation that allows problems to be to be described clearly and consicely
- Functional programming has a simple mathematical basis that supports equational reasoning about the properties of programs. 

(Thinking functionaly, Richard Bird) Chapter 1 page 1.

I will not go through the weeds of each bullet point, but in general, they are saying that FP is based on simple sound mathematical principles that make reasoning about programs easier. 

I am showing 2 cases where it might not be necessarily the case. 

## The YIN and YANG

### Conciseness

Lets take the following, program over 3 **Natural** numbers:

```javascript
[1,2,3].map(x => x+1)
// [2,3,4]
```

Many people might think that this code translate too:

```javascript
const arr = [1, 2, 3];
const result = [];
for (let i = 0; i < arr.length; i++) {
  result.push(arr[i] + 1);
}
result
// [2,3,4]
```

And they would be missing half the story, as this is only a partial implementation. 

Great we took 6 lines and converted into 1, so is this is what we solving? 


The conciseness of the `[1,2,3].map(x => x+1)` is not about less characters, rather it hides from us several mathematical principles that will send us to a deep rabit hole, (PLEASE BE AWARE I WILL BE USING scary words for a while)

What this code is really saying that we will be map over a data structure (natural numbers, that will need to be encoded in the language) that has properties of Functor and probably monad. Under the hood we would have something like this in haskell, hidden by the standard library. 


```haskell
-- We NEED to encode the definition of numbers into the lang
data Nat = Zero | Succ Nat
-- equality type-class we need this in order to compare numbers
instance Eq Nat where
  (==) :: Nat -> Nat -> Bool
-- we need this in order to print results
instance Show Nat where
  show :: Nat -> String
-- provide binary functions we need this to tell the computer how to operate with numbers
instance Num Nat where
  (+) :: Nat -> Nat -> Nat
  (*) :: Nat -> Nat -> Nat
  abs :: Nat -> Nat
  ...
-- and the finally after we can map over our natural numbers
```

Now haskell hides this from you by providing utility data types such as `Num` `Integer` and their respective 
instances of `Eq, Semigroup, Monoid, Monad...` and so on.
But there you go, its concise mathematically speaking, but at the end you are writing more lines of code. 

### Inmutability 

Probably the focal point of any OOP vs FP debate. Many FP elitis will look with disgust any code that reasigns variables values, arguing that state breakes equational reasoning of programns. Where OOP pragmatics argue about the inherent performance. Both side are right. I'll start by showing a bautiful canonical example of an FP function. Where lazyness, recursive scheme and conciseness shine.

```haskell
-- fibonnaci
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
```

At glance we have described easily how to create fib numbers, but its a terribly inneficient funcion, as it takes exponencial time to evaluate. With not getting into details we can improve the time performance of this function, by using  `Tupling` technique of paramenters (Chapter 7.6 Thinking Functionaly, Bird.).

```haskell
fib :: Int -> Int 
fib n = fst (fib2 n)
fib2 0 = (0,1)
fib2 n = (b, a+b) where (a, b) = fib2 (n-1)
```

Evaluating fib now takes linear time, but the space involved is not constant (even ignoring the fact that arbitrarily large integers cannot be stored in constant space)

In order to constraint the space of our solution we will introduce the ST monad and the strict operator ($!). (See chapter 10.4 Thinking functionaly. Bird) like so.

```haskell
fibST :: Int -> ST s Integer
fibST n = do { a <- newSTRef 0;
               b <- newSTRef 1;
               repeatFor n
                (do {x <- readSTRef a;
                     y <- readSTRef b;
                  writeSTRef a y;
                  writeSTRef b $! (x + y);
                readSTRef a})
              }

-- we need however to run the ST monad to get a proper value
runST :: (forall s . ST s a) -> a
-- Then we can finally have an efficient fibonacci function that looks like
fib :: Int -> Integer
fib n = runST (fibST n)
```

I do not know about you, but I found this significally harder to digest.

On ther other hand we can make a one to one translation of the haskell code above in `python` as

```python
def fib (n):
  a,b = 0,1
  for i in range(0, n):
    a, b = b, a+b
  return a
```

What this is telling us is that inmutability is not only making our code more efficient, but also easier to understand! 

## Summary 

As much as I love functional programming and using maths. Going full on math principles can make easy code really hard to reason about, specially when dealing with performance. 

