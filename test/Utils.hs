module Utils(isRight) where
import Prelude

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _         = False