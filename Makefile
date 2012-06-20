CEP_VERSION:=0.1
PACK_FOLDER:=ClosureExternalProducer-$(CEP_VERSION)

test:
	runhaskell -Wall clos_test.hs

build:
	runhaskell Setup.hs build

install:
	runhaskell Setup.hs install

conf:
	runhaskell Setup.hs configure

doc:
	runhaskell Setup.hs haddock

pack:
	mkdir $(PACK_FOLDER)
	cp -r Text $(PACK_FOLDER)
	cp ClosureExternalProducer.cabal $(PACK_FOLDER)
	cp Setup.hs $(PACK_FOLDER)
	cp README.md $(PACK_FOLDER)
	cp LICENSE $(PACK_FOLDER)
	tar cvf $(PACK_FOLDER).tar $(PACK_FOLDER)
	gzip $(PACK_FOLDER).tar
	rm -Rf $(PACK_FOLDER)

