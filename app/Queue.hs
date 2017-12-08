module Queue where

data Queue a = Queue [a] [a]

empty :: Queue a
empty = undefined
