module Tests where

-- import Control.Monad.State
import qualified Data.Map as M

import Dates hiding (today)
import Currencies

rur = "р."
euro = "€"

n # curr = F (n :# curr)

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
s0 = LS (today 0) accs [] myRates M.empty

pp01 = "cash" :<+ 200#rur
pp02 = Auto start
p0 = Posting 'S' "start" [pp01,pp02]
r0 = At (today 0) $ PR p0

pp11 = "expense" :<+ 10#rur
pp12 = "cash" :<+ (-10)#rur
p1 = Posting '!' "1" [pp11,pp12]
r1 = At (today 1) $ PR p1

pp21 = "cash" :<+ 50#rur
pp22 = Auto income
p2 = Posting '!' "2" [pp21,pp22]
r2 = At (today 2) $ PR p2

pp31 = "bank" :<+ 2#euro
pp32 = Auto income
p3 = Posting '!' "3" [pp31,pp32]
r3 = At (today 3) $ PR p3

pp41 = "expense" :<+ 15#rur
pp42 = "cash" :<+ 10#rur
pp43 = Auto bank
p4 = Posting '!' "4" [pp41,pp42,pp43]
r4 = At (today 4) $ PR p4

r5 = At (today 5) $ RR $ euro := 28:#rur

-- pp61 = "bank" :<+ 2 :# euro
-- pp62 = Auto income
-- p6 = Posting [pp61,pp62]
-- r6 = At today $ PR p6
r6 = At (today 6) $ PR p3

r7 = At (today 7) $ VR "cash" (45:#rur)

pp81 = "expense" :<+ 100#rur
pp82 = Auto cash
p8 = Posting 'R' "8" [pp81,pp82]
r8 = At (today 8) $ RegR (RegularPosting (date 2010 02 27) (Months 1) p8)

pp91 = "expense" :<+ (0 :? 10:#rur)
pp92 = Auto cash
p9 = Posting 'T' "template" [pp91,pp92]
tpl1 = Template "Test" 1 p9
r9 = At (today 9) $ TR tpl1

r10 = At (today 10) $ CTR "Test" []
r11 = At (today 11) $ CTR "Test" [15:#rur]

posts = doRecords (date 2010 08 01) [r0, r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11] 
