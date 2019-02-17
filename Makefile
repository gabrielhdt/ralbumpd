CC = ghc
RUNH = runhaskell
OUTDOC = doc
GUIXENVMANIFEST = guix-env.scm

all: cabal doc

ralbum: Main.hs Ralbum.hs
	$(CC) -o $@ $<

.PHONY: cabal

cabal: Setup.hs
	$(RUNH) Setup.hs configure
	$(RUNH) Setup.hs build

.PHONY: guix_cabal
cabal_guix: Setup.hs
	guix environment --manifest=$(GUIXENVMANIFEST) -- fish configure.fish
	guix environment --manifest=$(GUIXENVMANIFEST) -- $(RUNH) Setup.hs build

.PHONY: doc
doc:
	$(RUNH) Setup.hs haddock

.PHONY: clean
clean:
	$(RUNH) Setup.hs clean
