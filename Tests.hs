module Tests where

-- import Control.Monad.State
import qualified Data.Map as M

import Dates hiding (today)
import Currencies

rur = "р."
euro = "€"
perc = "%"

n # curr = F (n :# curr)

posting1 s descr from to sum = Posting s descr [p1,p2]
  where
    p1 = to :<+ sum
    p2 = Auto from

posting2 s descr from to sum1 sum2 = Posting s descr [p1,p2]
  where
    p1 = from :<+ sum1
    p2 = to :<+ sum2

simpleRecord n s descr from to sum = At (today n) $ PR $ posting1 s descr from to sum

myRates = M.fromList [((rur,euro), 1.0/30.0),
                      ((euro,rur), 30.0)]

income = Account "income" rur Nothing Nothing []
expense = Account "expense" rur Nothing Nothing []
cash = Account "cash" rur Nothing (Just corr) []
bank = Account "bank" euro Nothing Nothing []
corr = Account "correct" rur Nothing Nothing []
start = Account "start" rur Nothing Nothing []

accs = M.fromList $ map (\acc -> (accName acc, acc)) [income,expense,bank,cash,corr,start]

today n = DateTime 2010 02 28 n 0 0

s0 :: LedgerState
s0 = LS (today 0) accs [] myRates M.empty []

r0 = simpleRecord 0 'S' "start" start "cash" (200#rur)

p1 = posting2 '!' "1" "cash" "expense" (10#rur) ((-10)#rur)
r1 = At (today 1) $ PR p1

r2 = simpleRecord 2 '!' "2" income "cash" (50#rur)

r3 = simpleRecord 3 '!' "3" income "bank" (2#euro)

pp41 = "expense" :<+ 15#rur
pp42 = "cash" :<+ 10#rur
pp43 = Auto bank
p4 = Posting '!' "4" [pp41,pp42,pp43]
r4 = At (today 4) $ PR p4

r5 = At (today 5) $ RR $ euro := 28:#rur

r6 = simpleRecord 6 '!' "6" income "bank" (2#euro)

r7 = At (today 7) $ VR "cash" (45:#rur)

p8 = posting1 'R' "8" cash "expense" (100#rur)
r8 = At (today 8) $ RegR (RegularPosting (date 2010 02 27) (Months 1) p8)

p9 = posting1 'T' "template" cash "expense" (P 100 0 (10:#rur))
tpl1 = Template "Test" 1 p9
r9 = At (today 9) $ TR tpl1

r10 = At (today 10) $ CTR "Test" []
r11 = At (today 11) $ CTR "Test" [15:#rur]

p12 = posting1 'R' "ruled" cash "expense" (1#rur)
r12 = At (today 0) $ RuledP Before ("bank" :> (1:#rur)) p12

r13 = simpleRecord 13 'T' "test" cash "bank" (20#rur)
p14 = posting1 'B' "percents" income "bank" (5#perc)
r14 = At (today 14) $ PR p14

p15 = posting1 'P' "percents" income "bank" (10#perc)
r15 = At (today 15) $ RegR (RegularPosting (date 2010 03 08) (Months 1) p15)

p16 = posting1 'C' "tax" bank "expense" $ P 5 0 (0:#rur)
r16 = At (today 16) $ RuledP Before ("bank" :< (0:#rur)) p16

r17 = simpleRecord 17 '!' "test" bank "cash" (1#euro)

posts = doRecords (date 2010 08 01)
    [r0, r3,r14,r15,r16,r17]
--     [r0, r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,r13,r14] 

onePost = doRecords (date 2010 08 01) [r1]
