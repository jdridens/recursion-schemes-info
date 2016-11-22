{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
-- This guys is specifically so we can call show on fixed data types
{-# LANGUAGE UndecidableInstances #-}

module BetterRecursion where

-- In order to push this stuff further we need to introduce fix points.
--
-- Fix points have the form: f (x) = x, such that successive
-- applications of the function f do not change its input:
-- f (f (f (f ... (x) ...) = x
--
-- Basically a fix point occurs when a function stops changing
-- its input.
--
-- If we apply a function to it's argument continuously, but stop
-- the moment we reach a fixed point then we have produced what
-- is called the least fixed point.

-- We have two things floating around that can have fixed points:
-- data types, and functions. To represent arbitrarily recursive types
-- we have the following data type definition of fix:

newtype Fix f = Fix { unFix :: f (Fix f) }

-- This guy is why we need UndecidableInstance

instance Show (f (Fix f)) => Show (Fix f) where
  show x = "(" ++ show (unFix x) ++ ")"

-- To represent arbitrarily recursive functions we have the following
-- function combinator:

fix :: (t -> t) -> t
fix f = x where x = f x

-- As it turns out for all functors there exists some data type, a,
-- That represents a fixed point for the functor.
--
-- This means we can define two functions:
-- inF  :: Functor f => f a -> a
-- outF :: Functor f => a -> f a
--
-- These functions are responsible for moving between the functor
-- representation and the fixed representation of our data type.
-- We can express this idea in a class:

-- A few interesting things here, first is the use of MultiParameterTypeClass.
-- This allows us to specify a relation between two types. Doing this causes
-- us to introduce some ambiguity, which introduces us to the second
-- interesting thing going on: FunctionalDependencies, which allows us to
-- solve the new ambiguity (the issue being where type t comes from).

-- What this class says is that for some functor f, there is a relationship
-- between f and t such that f is uniquely determined from t. Cool? An example
-- is much more appropriate:

class Functor f => Fixpoint f t | t -> f where
  inF :: f t -> t
  outF :: t -> f t


instance Functor f => Fixpoint f (Fix f) where
  inF = Fix
  outF = unFix

-- Remember our old List data type?

-- data List a = Nil | Cons a (List a)

-- This data type is explicitly recursive, you can easily tell because the
-- type constructors explicitly references itself in its definition.
-- We can perform a nice little transformation on the data type by factoring
-- out the recursion, or parameterizing the data type.

-- Our new definition for List is implicitly recursive. Notice how the
-- recursion is parameterized by the type variable b. Meaning we could
-- be recursive, but we don't need to be recursive.

data ListF a b = NilF | ConsF a b deriving (Show , Functor)

-- An interesting difference between implicitly and explicitly recursive
-- data types is how well they represent the recursion.

-- With explicitly recursive functions the data type is finite (even if
-- the recursion could be infinite). Basically List a can represent
-- arbitrarily recursive lists, no matter how nested or long they may be.
-- For example:
-- Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil))) :: List Int

-- With implicitly recursive functions we lose this capability. The data
-- type must show exactly how far nested the recursion is. For example:

typeExmaple :: ListF Integer (ListF Integer (ListF Integer (ListF a b)))
typeExmaple = ConsF 1 (ConsF 2 (ConsF 3 NilF))

-- This isn't much of a problem with the Fix data type. We can recover the
-- representation of arbitrarily recursive data types by using it:

type List a = Fix (ListF a)

-- While this is cool we have to deal with calling Fix on everything, which
-- is error prone and annoying at best...

listExmaple :: List Int
listExmaple = Fix (ConsF 1 (Fix (ConsF 2 (Fix (ConsF 3 (Fix NilF))))))


-- Smart constructors to the rescue! Could also you pattern synonyms, or get
-- fancy with a type family. This is the easy answer for now, better do exist:

nil :: List a
nil = Fix NilF

cons :: a -> List a -> List a
cons x xs = Fix (ConsF x xs)

-- Lets make a couple of example data's

exmapleInt :: List Integer
exmapleInt = cons 1 (cons 2 (cons 3 (cons 4 (cons 5 nil))))

exmapleChar :: List Char
exmapleChar = cons 'a' (cons 'b' (cons 'c' (cons 'd' (cons 'e' nil))))

-- Finally we need to revise our definition for the various recursion schemes.
-- These are quite different than our original schemes. Abstraction made these
-- definitions simpler! Go figure ¯\_(ツ)_/¯

-- This lil guy just wraps a function h between g and f, really useful here:
(<-*-) :: Functor f => (f b -> b') -> (a' -> f a) -> (a -> b) -> a' -> b'
g <-*- f = \h -> g . fmap h . f

cataFix :: Fixpoint f a => (f b -> b) -> a -> b
cataFix y = fix (y <-*- outF)

anaFix :: Fixpoint f a => (b -> f b) -> b -> a
anaFix q = fix (inF <-*- q)

hyloFix :: Functor f => (f b -> b) -> (a -> f a) -> a -> b
hyloFix y q = fix (y <-*- q)

-- Catamorphisms use F-Algebras
-- F-Algebra :: F a -> a
lengthFix :: ListF a Integer -> Integer
lengthFix NilF = 0
lengthFix (ConsF _ b) = 1 + b

makeListFix :: Int -> List Int
makeListFix n = anaFix (listFix n) 0

-- Anamorphisms use F-Coalgebras
-- F-Coalgebre :: a -> f a
listFix :: (Ord a, Num a) => a -> a -> ListF a a
listFix p n
  | n >= p = NilF
  | otherwise = ConsF n (n + 1)

-- Hylomorphisms use both F-Algebras and F-Coalgebras
facFix :: Integer -> Integer
facFix = hyloFix alg coalg where
  alg (1,_) = 1
  alg (x,y) = x * y
  coalg n = (n, n-1)

-- So what did we gain in doing this?
--
--
--
-- We can run these forms of recursion schemes on ANY data type
-- that can be represented by Fix. These above schemes are really
-- the simplest schemes by the way. Don't forget that schemes like
-- Zygohistomorphic prepromorphisms exist!
--
-- Zygo = tears down a structure
-- Histo = tears down a structure considering previous answers
-- Prepro = tears down a structure after repeatedly transforming it
--
-- Add it altogether and you get: both semi-mutual recursion and history and
-- to repeatedly apply a natural transformation as you get deeper into
-- the functor.
--
-- Also fun to say...
--
-- Haven't found any uses for it though, not that I could use it, just saying
-- that apparently even google hasn't found specific usage for it. You can
-- get close with zygohistomorphic morphisms though.
