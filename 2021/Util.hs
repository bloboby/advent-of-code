module Util (print) where

import Prelude hiding (print)

print :: (Show a) => a -> String
print x = (show x) ++ "\n"
