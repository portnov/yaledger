module Tests where

import Control.Monad.State
import qualified Data.Map as M

import Dates hiding (today)
import Currencies

rur = "р."
euro = "€"

myRates = M.fromList [((rur,euro), 1.0/30.0),
                      ((euro,rur), 30.0)]

income = Account "income" rur Nothing Nothing []
expense = Account "expense" rur Nothing Nothing []
cash = Account "cash" rur Nothing (Just corr) []
bank = Account "bank" euro Nothing Nothing []
corr = Account "correct" rur Nothing Nothing []

accs = M.fromList $ map (\acc -> (accName acc, acc)) [income,expense,bank,cash,corr]

today = date 28 02 2010
s0 = LS today accs [] myRates

pp11 = "expense" :<+ 10 :# rur
pp12 = "cash" :<+ (-10) :# rur
p1 = Posting '!' "1" [pp11,pp12]
r1 = At today $ PR p1

pp21 = "cash" :<+ 50 :# rur
pp22 = Auto income
p2 = Posting '!' "2" [pp21,pp22]
r2 = At today $ PR p2

pp31 = "bank" :<+ 2 :# euro
pp32 = Auto income
p3 = Posting '!' "3" [pp31,pp32]
r3 = At today $ PR p3

pp41 = "expense" :<+ 15 :# rur
pp42 = "cash" :<+ 10 :# rur
pp43 = Auto bank
p4 = Posting '!' "4" [pp41,pp42,pp43]
r4 = At today $ PR p4

r5 = At today $ RR $ euro := 28 :# rur

-- pp61 = "bank" :<+ 2 :# euro
-- pp62 = Auto income
-- p6 = Posting [pp61,pp62]
-- r6 = At today $ PR p6
r6 = r3

r7 = At today $ VR "cash" (45 :# rur)

pp81 = "expense" :<+ 100 :# rur
pp82 = Auto cash
p8 = Posting 'R' "8" [pp81,pp82]
r8 = At today $ RegR (RegularPosting (date 2010 02 27) (Months 1) p8)

posts = doRecords (date 2010 08 01) [r1,r2,r3,r4,r5,r6,r7,r8] 
