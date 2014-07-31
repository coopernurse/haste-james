{-# LANGUAGE OverloadedStrings #-}
module Main where

import Haste
import Haste.Foreign
import Haste.Concurrent

addInt :: Int -> Int -> IO Int
addInt x y = return $ x + y

addIntAsync :: Int -> Int -> Opaque (Int -> IO ()) -> IO ()
addIntAsync x y callback = setTimeout 0 add
	where add = addInt x y >>= fromOpaque callback

main :: IO Bool
main = do
	export "addInt" addInt
	export "addIntAsync" addIntAsync
	return True