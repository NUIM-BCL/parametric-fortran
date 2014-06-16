all: ../bin/pfc

GHC = ghc
GHCFLAGS = -fglasgow-exts -XOverlappingInstances -XUndecidableInstances -O

generated_hs = Scanner.hs FParser.hs

%.hs: %.x; alex $<
%.hs: %.y; happy $<

# ../bin/pfc isn't actually PHONY, but we want to run its rule
# regardless rather than depending on $(wildcard *.hs) or something
# non-robust like that, and this does the job.

.PHONY: ../bin/pfc
../bin/pfc: $(generated_hs)
	$(GHC) --make $(GHCFLAGS) Main -o $@

clean:
	-rm -f $(generated_hs) *.o *.hi ../bin/pfc
