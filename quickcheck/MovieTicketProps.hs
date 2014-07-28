{-# LANGUAGE TemplateHaskell, FlexibleInstances, UndecidableInstances,
 ScopedTypeVariables, OverlappingInstances #-}
module Main where

--
-- to run:  runhaskell MovieTicketProps.hs
--

import System.Random
import Control.Applicative
import Test.QuickCheck
import Test.QuickCheck.All
import MovieTicket

class (Bounded a, Enum a) => BoundedEnum a
instance (Bounded a, Enum a) => BoundedEnum a
instance BoundedEnum a => Random a where
   random gen = randomR (minBound :: a, maxBound :: a) gen
   randomR (f, t) gen =
     (toEnum r :: a, nextGen)
     where
       (rnd, nextGen) = next gen
       r = fromEnum f + (rnd `mod` length [f..t])

instance Arbitrary TicketPurchaseReq where
	arbitrary = TicketPurchaseReq <$> arbitraryBoundedRandom <*> arbitraryBoundedRandom <*> arbitrary <*> arbitrary <*> arbitrary

prop_RevUnit x =
	reverse [x] == [x]

prop_RevApp xs ys =
	reverse (xs++ys) == reverse ys++reverse xs

prop_EmptyAgeListCostsZero req =
	calcPrice (req { ages = [] }) == 0

main = $(quickCheckAll)