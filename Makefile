EMACS = emacs

check: compile
	$(EMACS) -q -batch -l salsa20.el -l salsa20-test.el \
		-f ert-run-tests-batch-and-exit
	$(EMACS) -q -batch -l salsa20.elc -l salsa20-test.el \
		-f ert-run-tests-batch-and-exit

compile:
	$(EMACS) -q -batch -f batch-byte-compile salsa20.el

clean:
	rm -f *.elc
