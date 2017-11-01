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
               | AStrength a -- e.g. strongman
               | ASwap a a -- e.g. illusionist
               | AGuard a -- e.g. bodyguard
               deriving (Eq, Show)

data Event a = E a (Ability a)
  deriving (Eq, Show)

type Trace a = [Event a]

data Effect = Kill -- kill a player
            | Oil -- add oil to a player
            | Conceal -- prevent a kill from being announced
            | Silence -- prevent a player from talking
            | Check -- sheriff check someone
            | Visit -- visited for tracker/watcher
            | Light -- use up oil on a player or prevent kills by same player
            | Track -- track someone
            | Watch -- watch someone
            | GetRole -- learn the role of a player
            | Oracle -- reveal the role of player when cause of effect dies
            deriving (Eq, Show)

-- the type we should get after one night is a list of
data Action a = Action
  { src :: a
  , eff :: Effect
  , dest :: a
  }
  deriving (Show, Eq)

-- the result of executing a trace
type Result a = [Action a]

-- attributes for global state, for multiple games
data Attr = Anti Effect
          | Town
          | Mafia
          | HasOil
          | ThirdParty
          | Dead
          deriving (Eq, Show)
