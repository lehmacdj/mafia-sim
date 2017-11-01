{-# LANGUAGE DeriveFunctor #-}

module Mafia.Trace where

import Mafia.Types

import Data.List ((\\), intersect)
import Data.Semigroup
import Data.Map (Map)
import qualified Data.Map as Map

data EAux e = Effect e
            | CanMutate -- is player allowed to provide subset of identity
            deriving (Eq, Show)

data EType = EAux (EAux CondEffect)
           | Compose [EAux Effect] -- allow horizontal compositions
           deriving (Eq, Show)

data CondEffect = When Effect Effect
                | Always Effect
  deriving (Eq, Show)

concreteEffects :: [EAux Effect]
concreteEffects =
  [ Effect Kill
  , Effect Oil
  , Effect Conceal
  , Effect Silence
  , Effect Check
  , Effect Visit
  , Effect Light
  , Effect Track
  , Effect Watch
  , Effect GetRole
  , Effect Oracle
  , CanMutate
  ]

data Edge a = Edge EType a a
  deriving (Eq, Show, Functor)

composeAny :: a -> a -> Edge a
composeAny = Edge $ Compose concreteEffects

comp :: EAux CondEffect -> EAux Effect
comp (Effect (When _ e)) = Effect e
comp CanMutate = CanMutate

composeEdge :: Eq a => Edge a -> Edge a -> Maybe (Edge a)
composeEdge (Edge e p q) (Edge e' q' r)
  | q == q' = case (e, e') of
    (Compose es, Compose es') -> Just $ Edge (Compose $ es `intersect` es') p r
    (Compose es, EAux e') | comp e' `elem` es -> Just $ Edge (EAux e') p r
    (EAux e, Compose es') | comp e `elem` es' -> Just $ Edge (EAux e) p r
    _ -> Nothing
  | otherwise = Nothing

newtype Action a = A [Edge a]
  deriving (Eq, Show, Functor)

-- horizontal composition of Actions is a semigroup
instance Eq a => Semigroup (Action a) where
  A xs <> A ys = A [ z | x <- xs, y <- ys, Just z <- [composeEdge x y] ]

-- there is also however a notion of vertical composition that just adds
-- stuff to the relation
infixr 8 <@>
(<@>) :: Eq a => Action a -> Action a -> Action a
A xs <@> A ys = A $ xs ++ ys

composeEdges :: [EAux Effect] -> [a] -> Action a
composeEdges ts xs = A [ Edge (Compose ts) x x | x <- xs ]

identity :: [a] -> Action a
identity = composeEdges concreteEffects

initial :: Eq a => [a] -> Action a
initial xs = identity xs <@> A [ Edge (EAux CanMutate) x x | x <- xs ]

simple :: Eq a => [a] -> [Edge a] -> Action a
simple xs es = identity xs <@> A es

edge :: Effect -> a -> a -> Edge a
edge = edge' Always

edge' :: (Effect -> CondEffect) -> Effect -> a -> a -> Edge a
edge' c e = Edge (EAux $ Effect $ c e)

hasEffect :: Eq a => EAux CondEffect -> a -> Action a -> Bool
hasEffect e x (A es) = any (\z -> case z of
  Edge (EAux e') _ x' -> e == e' && x == x'
  _ -> False) es

flattenEdges :: Action a -> [(a, CondEffect, a)]
flattenEdges (A rs) = [ (p, e, q) | Edge (EAux (Effect e)) p q ]

eliminateConditions :: Eq a => [(a, CondEffect, a)] -> [(a, Effect, a)]
eliminateConditions xs = go xs where
  go ((p, When e e', q):ys)
    | (p, e, q) `elem` xs = (p, e', q) : go ys
    | otherwise = go ys
  go ((p, Always e, q):ys) = (p, e, q) : go ys

reduceEdges = eliminateConditions . flattenEdges

getAffected :: Ord a => [a] -> Action a -> Map a [(a, Effect)]
getAffected xs (A rs) = Map.fromList l where
  l = map (\q -> (q, [(p, e) | (p, e, q') <- reduceEdges rs, q == q'])) xs

getEffects :: Ord a => [a] -> Action a -> Map a [(a, Effect)]
getEffects xs (A rs) = Map.fromList l where
  l = map (\p -> (p, [(q, e) | (p', e, q) <- reduceEdges rs, p == p'])) xs

denoteSA :: SimpleAbility -> [CondEffect]
denoteSA AKill = [Always Kill, When Kill Visit]
denoteSA AOil = [Always Oil, When Oil Visit]
denoteSA AOracle = [Always Oracle, When Oracle Visit]
denoteSA ATrack = [Always Track]
denoteSA AWatch = [Always Watch]
denoteSA ACheck = [Always Check, When Check Visit]
denoteSA AGetRole = [Always GetRole, When GetRole Visit]
denoteSA ASilence = [Always Silence, When Silence Visit]
denoteSA AVisit = [Always Visit]
denoteSA AGravedig = [Always Kill, When Kill GetRole, When Kill Conceal, When Kill Visit]

edgesFromSA :: SimpleAbility -> a -> a -> [Edge a]
edgesFromSA a x y = map (\e -> Edge (EAux $ Effect e) x y) (denoteSA a)

canMutate :: Eq a => a -> Action a -> Bool
canMutate = hasEffect CanMutate

visit :: Eq a => [a] -> a -> a -> Action a
visit xs x y = simple xs [edge Visit x y]

swapEdge :: a -> a -> Action a
swapEdge x y = A [Edge (Compose concreteEffects) x y]

noKill :: [EAux Effect]
noKill = concreteEffects \\ [Effect Kill]

-- a folding function for a trace
augment :: Eq a => [a] -> Event a -> Action a -> Action a
augment xs (E x (ASimple a y)) ac = ac <> simple xs (edgesFromSA a x y)
augment xs (E x (ASave y)) ac
  | canMutate x ac = ac <> save <@> A [edge Visit x y] where
    save = composeEdges noKill [y] <@> identity (xs \\ [y])
-- roleblocking doesn't care if it is allowed to mutate
augment xs (E x (ARoleblock y)) ac = roleblock <> ac <> visit xs x y where
    roleblock = identity (xs \\ [y])
augment xs (E x (ADistract y)) ac
  | canMutate x ac = distract <> ac <> visit xs x y where
    distract = identity (xs \\ [y]) <@> composeEdges noKill [y]
augment xs (E x ALight) ac = ac <> simple xs (f =<< xs) where
  f y = [edge' (When Kill) Light x y, edge Kill x y]
augment xs (E x ARitual) ac | canMutate x ac = ac <> composeEdges noKill xs
augment xs (E x (ASwap y z)) ac
  | canMutate x ac = ac <> swap <@> A [edge Visit x y, edge Visit x z] where
    swap = identity (xs \\ [y, z]) <@> swapEdge y z <@> swapEdge z y
augment xs (E x (AGuard y)) ac
  | canMutate x ac = ac <> guard' <@> A [edge Visit x y] where
    guard' = identity (xs \\ [y]) <@> swapEdge y x
augment xs (E x (AStrength y)) ac = undefined -- strong edge? how to strongman
augment _ _ ac = ac
