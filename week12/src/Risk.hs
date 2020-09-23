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
battle (Battlefield a d) = do
    attackerRolls <- generateRolls $ maxAttacks a
    defenderRolls <- generateRolls $ maxDefends d
    let diffs = getDiffs attackerRolls defenderRolls
        newAttackers = attackerWins diffs
        newDefenders = defenderWins diffs
    pure $ Battlefield newAttackers newDefenders

maxAttacks :: Army -> Army
maxAttacks a = min 3 (a - 1)

maxDefends :: Army -> Army
maxDefends d = min 2 d

generateRolls :: Int -> Rand StdGen [DieValue]
generateRolls numRolls = replicateM numRolls die >>= pure . reverse . sort

attackerWins :: [DieValue] -> Int
attackerWins = length . filter (>0)

defenderWins :: [DieValue] -> Int
defenderWins = length . filter (<=0)

getDiffs :: [DieValue] -> [DieValue] -> [DieValue]
getDiffs attackerRolls defenderRolls =
    zipWith (-) attackerRolls defenderRolls
