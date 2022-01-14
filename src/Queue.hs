module Queue where

data Queue = Queue deriving (Show)

newtype QueueId = QueueId Int deriving (Show)

used :: Queue -> Bool
used = undefined

unused :: Queue -> Bool
unused = not . used

qLength :: Queue -> Int
qLength = undefined
