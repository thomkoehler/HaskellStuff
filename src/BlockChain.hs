module BlockChain (test) where

import qualified Data.ByteString.Lazy as B
import Data.Digest.Pure.SHA

type Hash = Digest SHA256State

data BlockChainHeader = BlockChainHeader
    { 
        prevHash :: Hash,
        hash :: Hash
    }

data BlockChain = BlockChain
    { 
        header :: BlockChainHeader,
        next :: Maybe BlockChain, 
        payload :: B.ByteString
    }

test :: IO ()
test = putStrLn "Hello World"
