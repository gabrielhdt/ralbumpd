HC = ghc
HCFLAGS = --make -o ralbum
TARGET = Main
LIB = Ralbum

$(TARGET): $(TARGET).hs $(LIB).hs
	$(HC) $(HCFLAGS) $<
