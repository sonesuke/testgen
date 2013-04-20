{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts #-}

import Control.Monad
import Text.Peggy
import qualified Data.ByteString as B
import qualified Data.ListLike as LL

add :: [[Int]] -> [[Int]] -> [[Int]]
add x y   |  length x > length y = left_melt x y
           | otherwise           = right_melt x y 

every :: [[Int]] -> [[Int]] -> [[Int]]
every [] y = []
every (x:xs) y = (map (\i -> x++i) y) ++ every xs y

melt :: [[Int]] -> [[Int]] -> [[Int]]
melt = left_melt

left_melt :: [[Int]] -> [[Int]] -> [[Int]]
left_melt x y = left_melt_internal x y y

left_melt_internal :: [[Int]] -> [[Int]] -> [[Int]] -> [[Int]]
left_melt_internal [] _ _ = []
left_melt_internal x [] m  = left_melt_internal x m m 
left_melt_internal (x:xs) (y:ys) m = [x ++ y] ++ left_melt_internal xs ys m

right_melt :: [[Int]] -> [[Int]] -> [[Int]]
right_melt x y = right_melt_internal x y x

right_melt_internal :: [[Int]] -> [[Int]] -> [[Int]] -> [[Int]]
right_melt_internal _ [] _ = []
right_melt_internal [] y m  = right_melt_internal m y m 
right_melt_internal (x:xs) (y:ys) m = [x ++ y] ++ right_melt_internal xs ys m

data TestDef
  = TestFactor (String, [String])
  | TestFactors [TestDef]
  | TestCase String
  deriving (Show)

data TestExpression
  = TestExpression String
  | TestTerm String
  | TestMultiply (TestExpression, TestExpression)
  | TestAdd (TestExpression, TestExpression)
  | TestMelto (TestExpression, TestExpression)
  deriving (Show)

[peggy|
top :: (TestDef, TestExpression) = factors expr!.


expr :: TestExpression
  = expr "+" fact {TestAdd ($1, $2)}
  / expr "~" fact {TestMelto ($1,$2)}
  / fact          

fact :: TestExpression
  = fact "*" term { TestMultiply ($1, $2) }
  / term

term :: TestExpression
  = "(" expr ")" 
  / factor_name  {TestTerm $1}

factors :: TestDef
  = (factor, "\n") {TestFactors $1}

factor :: TestDef
   = factor_name ":" levels  {TestFactor ($1, $2)}

levels :: [String]
  = (level, ",")

factor_name ::: String
  = factor_char*

level ::: String
  = [^:\n,]*

factor_char :: Char
  = [^ :=\n+*~]
|]

main :: IO ()
main = print . parseString top "<stdin>" =<< getContents

-- main =
  -- forever $ do
  --   line <- B.getLine
  --   print . parseString top "<stdin>" $ LL.CS line
