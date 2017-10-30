{-# LANGUAGE DeriveAnyClass #-}

module MafiaGraph where

import Data.Semigroup
import Data.List ((\\), delete)

data Effect = Save
            | Kill
            | Oil
            | Silence
            | Visit
            | Light
            -- an edge that transfers one part of the graph to another
            | Trans

data Rel a = R a Effect a

-- I needed to use a graph representation for this as we need both directions of
-- the graph and a Kleisli arrow based approach cannot give us that.
-- Frobenius monads in the category Rel seem promising for getting back nice
-- compositionality, but for now I just make a monoid instance which although
-- slow is sufficient.
newtype Segment a = S [Rel a]

-- action parts only compose if one componenet is Trans
compose :: Eq a => Rel a -> Rel a -> Maybe (Rel a)
compose (R p Trans q) (R q' e r)
  | q == q' = Just (R p e r)
  | otherwise = Nothing
compose (R p e q) (R q' Trans r)
  | q == q' = Just (R p e r)
  | otherwise = Nothing
compose _ _ = Nothing

instance Eq a => Semigroup (Segment a) where
  S xs <> S ys = S $ do
    x <- xs
    y <- ys
    -- abuse of fail warning
    Just z <- [compose x y]
    pure z

data Action a = A (Segment a) (Segment a) (Segment a)

instance Eq a => Semigroup (Action a) where
  A s1 s2 s3 <> A t1 t2 t3 = A (s1 <> t1) (s2 <> t2) (s3 <> t3)

data Event a = Visiting a a
             | Killing a a
             | UseStrength a a -- this is a huge problem, how do we make sure
                               -- that strongman kills pierce through roleblocks
                               -- and interact properly with body guard
             | Saving a a -- currently our graph does not allow computing which
                          -- players are saved and eliminating save/kill edges
                          -- It would be nice to allow this
             | Roleblocking a a
             | Distract a a -- Bard's action is also very hard to encode.
                            -- we need to know a specific kill edge in order to
                            -- evaluate it. an approach might be figuring out
                            -- if an attack should happen after accumulating a
                            -- graph of effects but this violates the fact that
                            -- the attacker is only supposed to visit if they
                            -- are not distracted by Bard
             | Swapping a a a
             | Oiling a a
             | Silencing a a
             | LightUp a -- how do we know what players are lit, or do we defer
                         -- this information to a state table later
             | PerformRitual a

identity :: [a] -> Segment a
identity xs = S [ R x Trans x | x <- xs ]

block :: Eq a => [a] -> a -> a -> Action a
block ps p q = A r a s where
  r = identity (delete q ps)
  a = S [R p Visit q] <> identity ps
  s = identity ps

swap :: Eq a => [a] -> a -> a -> a -> Action a
swap ps p q r = A (identity ps) a s where
  a = S [R p Visit q, R p Visit r] <> identity ps
  s = S [R q Trans r, R r Trans q] <> identity (ps \\ [q, r])

simple :: Eq a => [a] -> [Rel a] -> Action a
simple xs rs = A (identity xs) (S rs <> identity xs) (identity xs)

interpret :: Eq a => [a] -> [Event a] -> Action a
interpret xs (Visiting p q : ts) = simple xs [R p Visit q] <> interpret xs ts
interpret xs (Killing p q : ts) = simple xs [R p Visit q, R p Kill q] <> interpret xs ts
interpret xs (Silencing p q : ts) = simple xs [R p Visit q, R p Silence q] <> interpret xs ts
interpret xs (Saving p q : ts) = simple xs [R p Visit q, R p Save q] <> interpret xs ts
interpret xs (Roleblocking p q : ts) = block xs p q <> interpret xs ts
interpret xs (Swapping p q r : ts) = swap xs p q r <> interpret xs ts
interpret xs (Oiling p q : ts) = simple xs [R p Oil q, R p Visit q] <> interpret xs ts
interpret xs (LightUp p : ts) = simple xs (map (R p Light) xs) <> interpret xs ts
interpret xs (PerformRitual p : ts) = simple xs (map (R p Save) xs) <> interpret xs ts
