{-# LANGUAGE DeriveAnyClass #-}

module MafiaGraph where

import Data.Semigroup
import Data.List ((\\), delete, intersect)
import Data.Map (Map)
import qualified Data.Map as Map

data Effect = Save
            | Kill
            | Oil
            | Silence
            | Visit
            | Tracked
            | Watched
            | Light -- the semantics of this is to use up oil if it exists or
                    -- otherwise act like a save
            | Trans [Effect] -- an edge that transfers one part of the graph to
                             -- another
            deriving (Eq)

concreteEffects :: [Effect]
concreteEffects = [Save, Kill, Oil, Silence, Visit, Light, Tracked, Watched]

transAny :: Effect
transAny = Trans concreteEffects

isTrans :: Effect -> Bool
isTrans (Trans _) = True
isTrans _ = False

data Rel a = R a Effect a

-- I needed to use a graph representation for this as we need both directions of
-- the graph and a Kleisli arrow based approach cannot give us that.
-- Frobenius monads in the category Rel seem promising for getting back nice
-- compositionality, but for now I just make a monoid instance which although
-- slow is sufficient.
newtype Segment a = S [Rel a]

-- action parts only compose if one componenet is Trans
compose :: Eq a => Rel a -> Rel a -> Maybe (Rel a)
compose (R p (Trans es) q) (R q' (Trans es') r)
  | q == q' = Just (R p (Trans $ es `intersect` es') r)
  | otherwise = Nothing
compose (R p (Trans es) q) (R q' e r)
  | q == q' && e `elem` es = Just (R p e r)
  | otherwise = Nothing
compose (R p e q) (R q' (Trans es) r)
  | q == q' && e `elem` es = Just (R p e r)
  | otherwise = Nothing
compose _ _ = Nothing

instance Eq a => Semigroup (Segment a) where
  S xs <> S ys = S $ do
    x <- xs
    y <- ys
    -- abuse of fail warning
    Just z <- [compose x y]
    pure z

data Action a = A (Segment a) [Rel a] (Segment a)

instance Eq a => Semigroup (Action a) where
  A s1 s2 s3 <> A t1 t2 t3 = A (s1 <> t1) (s2 ++ t2) (s3 <> t3)

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
             | Track a a
             | Watch a a
             | Silencing a a
             | LightUp a -- how do we know what players are lit, or do we defer
                         -- this information to a state table later
             | PerformRitual a

identity :: [a] -> Segment a
identity xs = S [ R x transAny x | x <- xs ]

block :: Eq a => [a] -> a -> a -> Action a
block xs p q = A r [R p Visit q] s where
  r = identity (delete q xs)
  s = identity xs

distract :: Eq a => [a] -> a -> a -> Action a
distract xs p q = A r [R p Visit q] s where
  r = S $ R q (Trans $ delete Kill concreteEffects) q : part
  S part = identity (delete q xs)
  s = identity xs

-- how do swaps get roleblocked in this model? right now there is no way for
-- this to happen. Instead all swaps and roleblocks are guaranteed to happen.
-- This needs to be remedied since swaps at the very least should be
-- roleblockable in some kind of way. It might also be nice to make roleblocks
-- roleblockable so that we can have exactly the same semantics as the mafia we
-- play today.
swap :: Eq a => [a] -> a -> a -> a -> Action a
swap xs p q r = A (identity xs) [R p Visit q, R p Visit r] s where
  s = S $ R q transAny r : R r transAny q : part
  S part = identity (xs \\ [q, r])

simple :: Eq a => [a] -> [Rel a] -> Action a
simple xs rs = A (identity xs) rs (identity xs)

interpret :: Eq a => [a] -> [Event a] -> Action a
interpret xs (Visiting p q : ts) = simple xs [R p Visit q] <> interpret xs ts
interpret xs (Killing p q : ts) = simple xs [R p Visit q, R p Kill q] <> interpret xs ts
interpret xs (Silencing p q : ts) = simple xs [R p Visit q, R p Silence q] <> interpret xs ts
interpret xs (Saving p q : ts) = simple xs [R p Visit q, R p Save q] <> interpret xs ts
interpret xs (Roleblocking p q : ts) = block xs p q <> interpret xs ts
interpret xs (Swapping p q r : ts) = swap xs p q r <> interpret xs ts
interpret xs (Oiling p q : ts) = simple xs [R p Oil q, R p Visit q] <> interpret xs ts
interpret xs (LightUp p : ts) = simple xs ((\q -> [R p Kill q, R p Light q]) =<< xs) <> interpret xs ts
interpret xs (PerformRitual p : ts) = simple xs (map (R p Save) xs) <> interpret xs ts
interpret xs (Distract p q : ts) = distract xs p q <> interpret xs ts
interpret xs (Track p q : ts) = simple xs [R p Tracked q] <> interpret xs ts
interpret xs (Watch p q : ts) = simple xs [R p Watched q] <> interpret xs ts
interpret xs (UseStrength p q : ts) = undefined

-- get the effects attached to a specific player and the player that caused them
runForward :: Ord a => [a] -> Action a -> Map a [(a, Effect)]
runForward xs (A r a s) = Map.fromList l where
  l = map (\q -> (q, [ (p, e) | R p e q' <- rs, q == q', not $ isTrans e])) xs
  S rs = r <> S a <> s

-- get the effects that a specific player performed and the affected players
runReverse :: Ord a => [a] -> Action a -> Map a [(a, Effect)]
runReverse xs (A r a s) = Map.fromList l where
  l = map (\p -> (p, [ (q, e) | R p' e q <- rs, p == p', not $ isTrans e])) xs
  S rs = r <> S a <> s
