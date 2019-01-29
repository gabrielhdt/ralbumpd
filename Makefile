HC = ghc --make
TARGET = ralbum

$(TARGET): $(TARGET).hs
	$(HC) $<
