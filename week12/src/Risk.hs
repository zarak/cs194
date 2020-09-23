{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Data.List

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
    deriving Show

battle :: Battlefield -> Rand StdGen Battlefield
battle (Battlefield {attackers=a, defenders=d}) = do
    attackerRolls <- generateRolls a
    defenderRolls <- generateRolls d
    let diffs = getDiffs attackerRolls defenderRolls
        newAttackers = attackerWins diffs
        newDefenders = defenderWins diffs
    pure $ Battlefield {attackers=newAttackers, defenders=newDefenders}

generateRolls :: Int -> Rand StdGen [DieValue]
generateRolls numRolls = replicateM numRolls die >>= pure . reverse . sort

attackerWins :: [DieValue] -> Int
attackerWins = length . filter (>0)

defenderWins :: [DieValue] -> Int
defenderWins = length . filter (<=0)

getDiffs :: [DieValue] -> [DieValue] -> [DieValue]
getDiffs attackerRolls defenderRolls =
    zipWith (-) attackerRolls defenderRolls


