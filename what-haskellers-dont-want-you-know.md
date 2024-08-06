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

// todo describe a bit better abuot this

## The YIN

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

What this code is really saying that we will be applying over a seque

our haskell code will look something like this

```haskell
-- definition of natural numbers
data Nat = Zero | Succ Nat
-- equality type-class
instance Eq Nat where
  (==) :: Nat -> Nat -> Bool
-- how to print it
instance Show Nat where
  show :: Nat -> String
-- provide binary functions
instance Num Nat where
  (+) :: Nat -> Nat -> Nat
  (*) :: Nat -> Nat -> Nat
  abs :: Nat -> Nat
  ...
-- and the finally after 
```

