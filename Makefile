EMACS = emacs

check:
	$(EMACS) -q -batch -eval "(byte-compile-file \"salsa20.el\")"; \
	$(EMACS) -q -batch -l salsa20.el -l salsa20-test.el \
		-eval "(ert-run-tests-batch-and-exit '(tag salsa20))";

clean:
	rm -f *.elc
