{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}

module Mafia.Relation where

import Mafia.Types

import Control.Arrow
import Data.List ((\\), intersect, sort, elemIndex, nub)
import Data.Semigroup
import Data.Map (Map)
import qualified Data.Map as Map

data EAux e = Effect e
            | CanMutate -- is player allowed to provide subset of identity
            | StrongKill
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
  , StrongKill
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
comp (Effect (Always e)) = Effect e
comp CanMutate = CanMutate
comp StrongKill = Effect Kill

composeEdge :: Eq a => Edge a -> Edge a -> [Edge a]
composeEdge (Edge e p q) (Edge e' q' r)
  | q == q' = case (e, e') of
    (Compose es, Compose es') -> pure $ Edge (Compose $ es `intersect` es') p r
    (Compose es, EAux StrongKill) | Effect Kill `elem` es ->
      Edge (EAux StrongKill) <$> [p,q] <*> [r]
    (EAux StrongKill, Compose es) | Effect Kill `elem` es ->
      Edge (EAux StrongKill) <$> [p] <*> [q,r]
    (Compose es, EAux e') | comp e' `elem` es -> pure $ Edge (EAux e') p r
    (EAux e, Compose es') | comp e `elem` es' -> pure $ Edge (EAux e) p r
    _ -> []
composeEdge (Edge e@(EAux StrongKill) p q) _ = pure $ Edge e p q
composeEdge _ (Edge e@(EAux StrongKill) p q) = pure $ Edge e p q
composeEdge _ _ = []

newtype Rel a = R [Edge a]
  deriving (Eq, Show, Functor)

-- horizontal composition of Rel is a semigroup
instance Eq a => Semigroup (Rel a) where
  R xs <> R ys = R [ z | x <- xs, y <- ys, z <- composeEdge x y ]

-- there is also however a notion of vertical composition that just adds
-- stuff to the relation
infixr 8 <@>
(<@>) :: Eq a => Rel a -> Rel a -> Rel a
R xs <@> R ys = R $ xs ++ ys

composeEdges :: [EAux Effect] -> [a] -> Rel a
composeEdges ts xs = R [ Edge (Compose ts) x x | x <- xs ]

identity :: [a] -> Rel a
identity = composeEdges concreteEffects

initial :: Eq a => [a] -> Rel a
initial xs = identity xs <@> R [ Edge (EAux CanMutate) x x | x <- xs ]

simple :: Eq a => [a] -> [Edge a] -> Rel a
simple xs es = identity xs <@> R es

edge :: Effect -> a -> a -> Edge a
edge = edge' Always

edge' :: (Effect -> CondEffect) -> Effect -> a -> a -> Edge a
edge' c e = Edge (EAux $ Effect $ c e)

hasEffect :: Eq a => EAux CondEffect -> a -> Rel a -> Bool
hasEffect e x (R es) = any (\z -> case z of
  Edge (EAux e') _ x' -> e == e' && x == x'
  _ -> False) es

flattenEdges :: Rel a -> [(a, EAux CondEffect, a)]
flattenEdges (R rs) = [ (p, e, q) | Edge (EAux e) p q <- rs ]

substituteStrongKill :: [(a, EAux CondEffect, a)] -> [(a, CondEffect, a)]
substituteStrongKill = (>>= (\(p, e, q) -> (p,,q) <$> case e of
  Effect e' -> [e']
  StrongKill -> Always <$> [Visit, Kill]
  _ -> []))

rcond :: [(a, CondEffect, a)] -> [(a, Effect, a)]
rcond xs = [ (p, e, q) | (p, Always e, q) <- xs ]

eliminateConditions :: Eq a => [(a, CondEffect, a)] -> [(a, Effect, a)]
eliminateConditions xs = go xs where
  go [] = []
  go ((p, When e e', q):ys)
    -- we know based on the way we constructed conditions that this works
    | (p, e, q) `elem` rcond xs = (p, e', q) : go ys
    | otherwise = go ys
  go ((p, Always e, q):ys) = (p, e, q) : go ys

reduceEdges :: Eq a => Rel a -> [(a, Effect, a)]
reduceEdges = eliminateConditions . substituteStrongKill . flattenEdges

getAffectedByMap :: Ord a => [a] -> Rel a -> Map a [(a, Effect)]
getAffectedByMap xs a = Map.fromList l where
  l = map (\q -> (q, [(p, e) | (p, e, q') <- reduceEdges a, q == q'])) xs

getEffectingMap :: Ord a => [a] -> Rel a -> Map a [(a, Effect)]
getEffectingMap xs a = Map.fromList l where
  l = map (\p -> (p, [(q, e) | (p', e, q) <- reduceEdges a, p == p'])) xs

getResult :: Ord a => Rel a -> Result a
getResult = map (\(p, e, q) -> Action p e q) . reduceEdges

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

canMutate :: Eq a => a -> Rel a -> Bool
canMutate = hasEffect CanMutate

visit :: Eq a => [a] -> a -> a -> Rel a
visit xs x y = simple xs [edge Visit x y]

swapEdge :: a -> a -> Rel a
swapEdge x y = R [Edge (Compose concreteEffects) x y]

noKill :: [EAux Effect]
noKill = concreteEffects \\ [Effect Kill]

-- a folding function for a trace
execute :: Eq a => [a] -> Event a -> Rel a -> Rel a
execute xs (E x (ASimple a y)) ac = ac <> simple xs (edgesFromSA a x y)
execute xs (E x (ASave y)) ac
  | canMutate x ac = ac <> save <@> R [edge Visit x y] where
    save = composeEdges noKill [y] <@> identity (xs \\ [y])
-- roleblocking doesn't care if it is allowed to mutate
execute xs (E x (ARoleblock y)) ac = roleblock <> ac <> visit xs x y where
  roleblock = identity (xs \\ [y])
execute xs (E x (ADistract y)) ac
  | canMutate x ac = distract <> ac <> visit xs x y where
    distract = identity (xs \\ [y]) <@> composeEdges noKill [y]
execute xs (E x ALight) ac = ac <> simple xs (f =<< xs) where
  f y = [edge' (When Kill) Light x y, edge Kill x y]
execute xs (E x ARitual) ac | canMutate x ac = ac <> composeEdges noKill xs
execute xs (E x (ASwap y z)) ac
  | canMutate x ac = ac <> swap <@> R [edge Visit x y, edge Visit x z] where
    swap = identity (xs \\ [y, z]) <@> swapEdge y z <@> swapEdge z y
execute xs (E x (AGuard y)) ac
  | canMutate x ac = ac <> guard' <@> R [edge Visit x y] where
    guard' = identity (xs \\ [y]) <@> swapEdge y x
execute xs (E x (AStrength y)) ac = (simple xs k <> ac) <@> R sk where
  sk = [Edge (EAux StrongKill) x y]
  k = [edge Kill x y, edge' (When Kill) Visit x y]
execute _ _ ac = ac

newtype TraceOrd a = TO { unTO :: Event a }
  deriving (Show)

-- whnf equality
instance Eq (TraceOrd a) where
  TO (E _ (ARoleblock _)) == TO (E _ (ARoleblock _)) = True
  TO (E _ (ADistract _)) == TO (E _ (ADistract _)) = True
  TO (E _ (ASave _)) == TO (E _ (ASave _)) = True
  TO (E _ (ASimple sa _)) == TO (E _ (ASimple sa' _)) = sa == sa'
  TO (E _ ALight) == TO (E _ ALight) = True
  TO (E _ ARitual) == TO (E _ ARitual) = True
  TO (E _ (AStrength _)) == TO (E _ (AStrength _)) = True
  TO (E _ (ASwap _ _)) == TO (E _ (ASwap _ _)) = True
  TO (E _ (AGuard _)) == TO (E _ (AGuard _)) = True
  _ == _ = False

-- we never evaluate these beyond WHNF so undefined is fine here
actionOrder :: [TraceOrd a]
actionOrder = map (TO . E undefined . ($ undefined)) abilities where
  abilities =
    [ ASimple AKill
    , ASimple AOil
    , ASimple ATrack
    , ASimple AOracle
    , ASimple AWatch
    , ASimple ACheck
    , ASimple AGetRole
    , ASimple ASilence
    , ASimple AVisit
    , ASimple AGravedig
    , const ALight
    , const ARitual
    -- saves are affected by swaps thus before them
    , ASave
    -- swaps happen after all of the concrete roles
    , ASwap undefined
    -- strength goes after swap so it is unaffected by it
    , AStrength
    -- guard does affect strength so it occurs next
    , AGuard
    -- strength is post save to go through saves
    -- roleblocks occur in inverse priority order composing on left
    , ADistract
    , ARoleblock
    ]

instance Ord (TraceOrd a) where
  compare x y = case (x `elemIndex` actionOrder, y `elemIndex` actionOrder) of
    (Just x', Just y')
      | x' > y' -> GT
      | x' == y' -> EQ
      | otherwise -> LT
    -- if both aren't Just, something went wrong
    _ -> undefined

runTrace :: Ord a => [a] -> Trace a -> Result a
runTrace xs = nub
  . getResult
  . foldl (flip $ execute xs) (initial xs)
  . map unTO . sort . map TO
