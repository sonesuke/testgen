{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts #-}

import Control.Monad
import Text.Peggy
import qualified Data.ByteString as B
import qualified Data.ListLike as LL


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
