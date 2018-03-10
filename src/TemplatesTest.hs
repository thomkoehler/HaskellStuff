
module TemplatesTest where

import Templates

main = do
  print $ $(sel 2 3) ('a','b','c')
  return ()
