module Mafia.Types where

data SimpleAbility = AKill -- e.g. mafia
                   | AOil -- e.g. arsonist
                   | ATrack -- e.g. tracker
                   | AOracle -- e.g. oracle
                   | AWatch -- e.g. watcher
                   | ACheck -- e.g. sheriff
                   | AGetRole -- e.g. bootlegger
                   | ASilence -- e.g. hooker
                   | AVisit -- e.g. sleepwalker
                   | AGravedig -- e.g. gravedigger
                   deriving (Eq, Show)

-- the things that roles are allowed to do
data Ability a = ARoleblock a -- e.g. bootlegger
               | ADistract a -- e.g. bard
               | ASave a -- e.g. doctor
               | ASimple SimpleAbility a -- applies several effects
               | ALight -- e.g. arsonist
               | ARitual -- e.g. virgin
               -- the position of strength in list determines if it goes through
               -- swaps or not. bodyguard is not handled correctly
               | AStrength a -- e.g. strongman
               | ASwap a a -- e.g. illusionist
               | AGuard a -- e.g. bodyguard
               deriving (Eq, Show)

data Event a = E a (Ability a)
  deriving (Eq, Show)

type Trace a = [Event a]

data Effect = Kill
            | Oil
            | Conceal
            | Silence
            | Check
            | Visit
            | Light
            | Track
            | Watch
            | GetRole
            | Oracle
            deriving (Eq, Show)

data Attr = Anti Effect
          | Town
          | Mafia
          | HasOil
          | ThirdParty
          | Dead
          deriving (Eq, Show)
