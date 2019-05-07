module Utils(isRight) where

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _         = False