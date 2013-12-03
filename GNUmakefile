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
all: $(DEVALOT_PANDOC) README.md README.html slides.html

################################################################################
clean::
	rm -f README.html slides.html

################################################################################
README.md: $(SOURCE)
	$(PANDOC) -f markdown -t json $< | \
          $(DEVALOT_PANDOC) | \
          $(PANDOC) -f json -t markdown --atx-headers -o $@.tmp
	sed -r 's|^</?div.*$$||' < $@.tmp > $@
	rm $@.tmp

################################################################################
README.html: README.md
	$(PANDOC) -f markdown -t html5 -s -o $@ $<

################################################################################
slides.html: $(SOURCE)
	$(PANDOC) -f markdown -t json $< | \
          $(DEVALOT_PANDOC) | \
          $(PANDOC) -f json -t dzslides \
            -s --self-contained -o $@

################################################################################
$(DEVALOT_PANDOC): $(DEVALOT_PANDOC_DIR)
	cd $(DEVALOT_PANDOC_DIR) && \
          cabal sandbox init && \
          cabal install

################################################################################
$(DEVALOT_PANDOC_DIR):
	git submodule update --init
