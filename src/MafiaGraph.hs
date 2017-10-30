{-# LANGUAGE DeriveFunctor #-}

module MafiaGraph where

import Control.Monad
import Control.Arrow
import Control.Monad.Free

data EffectF a = SaveF a
              | KillF a
              | VisitF a
              deriving (Functor)

type Effect = Free EffectF

save, kill, visit :: a -> Effect a
save = liftF . SaveF
kill = liftF . KillF
visit = liftF . VisitF

type Action a = a -> [Effect a]

data Event a = Visiting a a
             | Killing a a
             | Saving a a
             | Roleblocking a a
             | Swapping a a a

single :: Eq a => a -> Effect a -> Action a
single p e q
  | p == q = e:[pure p]
  | otherwise = [pure p]

block :: Eq a => a -> a -> Action a
block p q r
  | q == r = []
  | otherwise = [pure p]

interpret :: Eq a => [Event a] -> (Action a, [(a, Effect a)], Action a)

interpret (Visiting p q : ts) =
  let (r, as, s) = interpret ts
   in (r, (p, visit q) : as, s)

interpret (Killing p q : ts) =
  let (r, as, s) = interpret ts
   in (r, (p, kill q) : as, s)

interpret (Saving p q : ts) =
  let (r, as, s) = interpret ts
   in (r, (p, save q) : as, s)

-- interpret (Roleblocking p q : ts) =
--   let (r, as, s) = interpret ts
--    in (r >=> block p q, (p, visit q) : as, s)
