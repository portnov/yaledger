all: yaledger

yaledger: *.hs
	ghc --make -idist/build/autogen yaledger.hs

clean:
	find . -name \*.hi -delete
	find . -name \*.o -delete
	find . -name \*.p_o -delete
	find . -name \*.oprof -delete
