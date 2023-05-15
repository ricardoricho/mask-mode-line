# mask-mode-line Makefile

tests:
	emacs -Q -batch -l ert -l mask-mode-line-tests.el -f ert-run-tests-batch-and-exit
