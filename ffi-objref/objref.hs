-- need this ghc option turned on for "JSAny deriving (Pack, Unapack)" to work
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Haste
import Haste.Foreign

newtype Toggle = Toggle JSAny
newtype ToggleObj = ToggleObj JSAny deriving (Pack, Unpack)

foreign import ccall newToggle :: Bool -> IO (Toggle)
foreign import ccall toggle :: Toggle -> IO (Bool)

foreign import ccall newToggleObj :: Bool -> IO (ToggleObj)

-- Is there any way to do this using ccall?
toggleObj :: ToggleObj -> IO (Bool)
toggleObj = ffi $ toJSString "(function(t) { return t.toggle();})"

nowMillis :: IO (Float)
nowMillis = ffi $ toJSString "(function() { return new Date().getTime(); })"

-- this doesn't compile:
--foreign import ccall "(function(t) { return t.toggle();})" toggleObj2 :: ToggleObj -> IO (Bool)

-- TODO: make this strict
benchIO :: (Int -> IO ()) -> IO (Float)
benchIO fx = do
	start <- nowMillis
	_ <- fx 1
	end <- nowMillis
	return $ end - start

main :: IO Bool
main = withElems ["t1", "t2", "bench"] objref

objref [t1, t2, bench] = do
	tog1 <- newToggle    False
	onEvent t1 OnClick $ \_ _ -> do
		b <- toggle tog1
		setProp t1 "innerHTML" $ toString "toggle 1: " ++ (show b)

	tog2 <- newToggleObj False
	onEvent t2 OnClick $ \_ _ -> do
		b <- toggleObj tog2
		setProp t2 "innerHTML" $ toString "toggle 2: " ++ (show b)

	onEvent bench OnClick $ \_ _ -> do
		let n = 1000000
		setProp bench "innerHTML" $ toString "benchmarking ccall"
		el1 <- benchIO (\ _ -> mapM_ (\ _ -> toggle tog1) [1..n])
		setProp bench "innerHTML" $ toString "benchmarking ffi"
		el2 <- benchIO (\ _ -> mapM_ (\ _ -> toggleObj tog2) [1..n])
		setProp bench "innerHTML" $ toString "iterations: " ++ (show n) 
			++ "<br>ccall: " ++ (show el1) 
			++ "<br>ffi: " ++ (show el2) 
			++ "<br>click to run again"

		-- TODO: uncomment once benchIO is made strict
		-- right now el1 and el2 are being evaluated more than once
		
		--setProp bench "innerHTML" $ toString "iterations: " ++ (show n) 
		--	++ "<br>ccall: " ++ (show el1) 
		--	++ " ms (" ++ (show ((el1/n)*1000)) ++ " us/call)"
		--	++ "<br>ffi: " ++ (show el2) 
		--	++ " ms (" ++ (show ((el2/n)*1000)) ++ " us/call)"
		--	++ "<br>click to run again"