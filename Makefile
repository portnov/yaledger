all: yaledger

yaledger: *.hs
	ghc --make yaledger.hs

clean:
	find . -name \*.hi -delete
	find . -name \*.o -delete