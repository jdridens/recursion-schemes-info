module SimpleRecursion where

-- To Simplify the topic we will first explore recursion
-- schemes in terms of this cons list.
-- As a side note, most of the simple recursion here already
-- exists in haskell with foldable, traversable, and Data.List

data List a = Nil | Cons a (List a)

instance Show a => Show (List a) where
    show Nil = "()"
    show ls  = "(" ++ showlist ls ++ ")"
        where showlist (Cons a Nil) = show a
              showlist (Cons a b)   = show a ++ " " ++ showlist b


conslist :: [a] -> List a
conslist = foldr Cons Nil










-- Generalized foldr, takes a seed value, and a right
-- associative binary operator. A catamorphism is normally
-- expressed in terms of banana brackets (| and |), but for now
-- we can use this data type:
data Catamorphism a b = Banana b ((a,b) -> b)

-- In the notation we would normally use: h = (| 0, const (+ 1) |)
bananaLength :: Catamorphism a Integer
bananaLength = Banana 0 (\(a,b) -> 1 + b)

-- h = (| Nil, helper |)
bananaFilter :: (a -> Bool) -> Catamorphism a (List a)
bananaFilter p = Banana Nil helper
    where helper (x,xs)
            | p x = Cons x xs
            | otherwise = xs

cata :: Catamorphism a b -> List a -> b
cata (Banana seed func) = alg where
    alg Nil = seed
    alg (Cons a as) = func (a, alg as)










-- Generalized unfold, takes a function to build up the list (via
-- induction) and a predicate (to terminate the list creation).
-- An anamorphism is normally expressed in terms of concave
-- lenses [( and )], but for now we can use this data type:
data Anamorphism a b = ConcaveLense (b -> (a, b)) (b -> Bool)

-- h = [( buildFunc, termPred )]
clenseZip = ConcaveLense buildFunc termPred
    where
        buildFunc :: (List a, List b) -> ((a,b), (List a, List b))
        buildFunc (Cons a as, Cons b bs) = ((a,b),(as,bs))

        termPred :: (List a, List b) -> Bool
        termPred (Nil, _) = True
        termPred (_, Nil) = True
        termPred _ = False

-- h = [( buildFunc, (== n) )]
clenseList n = ConcaveLense buildFunc (== n)
    where buildFunc a = (a, succ a)

ana :: Anamorphism a b -> b -> List a
ana (ConcaveLense buildFunc termPred) = alg where
    alg b
      | termPred b = Nil
      | otherwise = let (a,b') = buildFunc b
                    in Cons a (alg b')











-- Composition of foldr and unfoldr
-- In more "accurate scarry words" a hylomorphism is a recursive
-- function whose call-tree is isomorhpic to the data structure.
-- In other words it's a linear recursive function. In the notation
-- we express these by wrapping them in envelopes [| and |], but
-- for now we'll use this data type:
data Hylomorphism a b c = Envelope c ((b,c)->c) (a ->(b,a)) (a->Bool)

-- We build up the structure and then we reduce it down
-- to the value we want.
hylo :: Hylomorphism a b c -> a -> c
hylo (Envelope cataSeed cataFunc buildFunc termPred) = alg where
    alg n
      | termPred n = cataSeed
      | otherwise = let (b,n') = buildFunc n
                    in cataFunc (b, alg n')

envelopeFac :: Hylomorphism Integer Integer Integer
envelopeFac = Envelope 1 cataFunc buildFunc termPred
    where cataFunc (x,y) = x * y
          buildFunc n = (n, n - 1)
          termPred n = n == 0
