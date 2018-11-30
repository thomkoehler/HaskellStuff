
module Fixpoint where

import Prelude

fix :: (a -> a) -> a
fix f = f $ fix f

sumR r [] = 0
sumR r (x:xs) = x + r xs

sum = fix sumR

facR r 0 = 1
facR r 1 = 1
facR r n = n * r (n - 1)

fac = fix facR
