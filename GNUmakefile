################################################################################
SOURCE = slides.md

################################################################################
DEVALOT_PANDOC_DIR = vendor/devalot-pandoc
DEVALOT_PANDOC_BIN = $(DEVALOT_PANDOC_DIR)/.cabal-sandbox/bin
DEVALOT_PANDOC     = $(DEVALOT_PANDOC_BIN)/devalot-pandoc
PANDOC             = $(DEVALOT_PANDOC_BIN)/pandoc

################################################################################
.PHONEY: all clean

################################################################################
all: $(DEVALOT_PANDOC) README.md

################################################################################
clean::
	rm -f README.md

################################################################################
README.md: $(SOURCE)
	$(PANDOC) -f markdown -t json $< | \
          $(DEVALOT_PANDOC) | \
          $(PANDOC) -f json -t markdown --atx-headers -o $@

################################################################################
$(DEVALOT_PANDOC): $(DEVALOT_PANDOC_DIR)
	cd $(DEVALOT_PANDOC_DIR) && \
          cabal sandbox init && \
          cabal install

################################################################################
$(DEVALOT_PANDOC_DIR):
	git submodule update --init
