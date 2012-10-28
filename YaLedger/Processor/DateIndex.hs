
module YaLedger.Processor.DateIndex where

import Data.List
import Data.Function (on)
import Data.Maybe
import qualified Data.Map as M
import Data.Dates
import Text.Printf

import YaLedger.Types.Common
import YaLedger.Types.Transactions
import YaLedger.Output.Pretty

lookupDate :: DateTime -> DateIndex a -> Maybe (DateZipper a)
lookupDate (DateTime y m d _ _ _) ix = do
  byMonth <- M.lookup y ix
  byDay   <- M.lookup m byMonth
  M.lookup d byDay

lookupDateApprox :: DateTime -> Integer -> DateIndex a -> [a]
lookupDateApprox base 0 ix = maybe [] dzThisDate $ lookupDate base ix
lookupDateApprox base dx ix =
  case lookupDate base ix of
    Nothing -> let dates = [base `addInterval` Days i | i <- [-dx .. dx]]
                   zippers = mapMaybe (\d -> lookupDate d ix) dates
               in  concatMap dzThisDate zippers
    Just zipper -> prevDays (base `addInterval` Days (- dx)) zipper ++
                   dzThisDate zipper ++
                   nextDays (base `addInterval` Days dx) zipper

lookupDatePrev :: DateTime -> Integer -> DateIndex a -> [a]
lookupDatePrev base 0 ix = maybe [] dzThisDate $ lookupDate base ix
lookupDatePrev base dx ix =
  case lookupDate base ix of
    Nothing -> let dates = [base `addInterval` Days i | i <- [-dx .. 0]]
                   zippers = mapMaybe (\d -> lookupDate d ix) dates
               in  concatMap dzThisDate zippers
    Just zipper -> prevDays (base `addInterval` Days (- dx)) zipper ++ dzThisDate zipper

prevDays :: DateTime -> DateZipper a -> [a]
prevDays start z = go z
  where
    go x = case dzPrevDate x of
             Nothing -> []
             Just pz -> if dzDate pz >= start
                          then dzThisDate pz ++ go pz
                          else []

nextDays :: DateTime -> DateZipper a -> [a]
nextDays end z = go z
  where
    go x = case dzNextDate x of
             Nothing -> []
             Just pz -> if dzDate pz <= end
                          then dzThisDate pz ++ go pz
                          else []

buildZipper :: [(DateTime, [Ext a])] -> DateZipper (Ext a)
buildZipper list = go Nothing list
  where
    go _ [] = EmptyZipper
    go back ((date, recs): xs) =
      let this = DateZipper date recs back
                            (Just $ go (Just this) xs)
      in  this

groupRecords :: [Ext a] -> [(DateTime, [Ext a])]
groupRecords recs =
  let dateEq dt1 dt2 = (year dt1 == year dt2) && (month dt1 == month dt2) && (day dt1 == day dt2)
      groups = groupBy (dateEq `on` getDate) recs
      convert rs = (getDate $ head rs, rs)
  in  map convert groups

buildIndex :: [Ext a] -> DateIndex (Ext a)
buildIndex recs =
  let rootZipper = buildZipper $ groupRecords recs

      allZippers = getZippers rootZipper

      getZippers z
        | EmptyZipper <- z          = []
        | Nothing   <- dzNextDate z = [z]
        | Just next <- dzNextDate z = z: getZippers next
      
      byYear = M.fromList $
        [(year $ dzDate $ head g, byMonth g) | g <- groupBy ((==) `on` (year . dzDate)) allZippers]

      byMonth zs = M.fromList $
        [(month $ dzDate $ head g, byDay g) | g <- groupBy ((==) `on` (month . dzDate)) zs]

      byDay zs = M.fromList $
        [(day $ dzDate z, z) | [z] <- groupBy ((==) `on` (day . dzDate)) zs]

  in  byYear

noext y m d x = Ext (DateTime y m d 0 0 0) i (newPos "<nowhere>" 0 0) M.empty x
  where i = fromIntegral (y*366 + m*31 + d)

testList ::  [Ext String]
testList = [noext y m d (s y m d) | y <- [2012,2013], m <- [1,5,10,11], d <- [3,6,7,10,12,29]]
  where
    s :: Int -> Int -> Int -> String
    s y m d = printf "S <%02d/%02d/%02d>" y m d
