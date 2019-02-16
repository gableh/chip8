module State where

newtype State = State String deriving (Show, Eq)

mkState :: String -> State
mkState filename = State filename