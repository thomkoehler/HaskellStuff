{-# LANGUAGE ForeignFunctionInterface #-}

module Fun
(
    test
) where

import Foreign.C.Types

test :: IO ()
test = print $ addOne 1


foreign import ccall "fun.h addOne" c_addOne :: CInt -> CInt
addOne :: Int -> Int
addOne = fromIntegral . c_addOne . fromIntegral