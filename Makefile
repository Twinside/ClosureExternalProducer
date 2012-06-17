all:
	runhaskell Setup.hs build

conf:
	runhaskell Setup.hs configure

test:
	runhaskell -Wall clos_test.hs

doc:
	runhaskell Setup.hs haddock

