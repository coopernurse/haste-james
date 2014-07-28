module MovieTicket where

data Day = Mon | Tues | Wed | Thurs | Fri | Sat | Sun deriving (Show, Eq, Enum, Ord, Bounded)

data SeatingArea = Loge | Parquet deriving (Show, Eq, Enum, Ord, Bounded)

data TicketPurchaseReq =
	TicketPurchaseReq {
		day :: Day,
		seatingArea :: SeatingArea,
		is3D :: Bool,
		minutes :: Int,
		ages :: [Int]
	} deriving (Show, Eq)

calcPrice :: TicketPurchaseReq ->
             Float             -- Total price

calcPrice req =
	0

