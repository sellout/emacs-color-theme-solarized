EMACS=emacs

EMACS_CLEAN=-Q
EMACS_BATCH=$(EMACS_CLEAN) --batch
TESTS=

CURL=curl --silent
WORK_DIR=$(shell pwd)
PACKAGE_NAME=$(shell basename $(WORK_DIR))
AUTOLOADS_FILE=$(PACKAGE_NAME)-autoloads.el
TRAVIS_FILE=.travis.yml
TEST_DIR=ert-tests
TEST_DEP_1=ert
TEST_DEP_1_STABLE_URL=http://git.savannah.gnu.org/cgit/emacs.git/plain/lisp/emacs-lisp/ert.el?h=emacs-24.3
TEST_DEP_1_LATEST_URL=http://git.savannah.gnu.org/cgit/emacs.git/plain/lisp/emacs-lisp/ert.el?h=master

.PHONY : build downloads downloads-latest autoloads test-autoloads test-travis \
         test test-interactive clean edit test-dep-1 test-dep-2 test-dep-3     \
         test-dep-4 test-dep-5 test-dep-6 test-dep-7 test-dep-8 test-dep-9

build :
	$(EMACS) $(EMACS_BATCH) --eval                      \
	    "(if (< emacs-major-version 24)                 \
	         (progn                                     \
	           (add-to-list 'load-path \"./\")          \
	           (require 'color-theme-solarized)         \
	           (color-theme-solarized))                 \
	       (add-to-list 'custom-theme-load-path \"./\") \
	       (load-theme 'solarized t))"

test-dep-1 :
	@cd $(TEST_DIR)                                      && \
	$(EMACS) $(EMACS_BATCH)  -L . -L .. -l $(TEST_DEP_1) || \
	(echo "Can't load test dependency $(TEST_DEP_1).el, run 'make downloads' to fetch it" ; exit 1)

downloads :
	$(CURL) '$(TEST_DEP_1_STABLE_URL)' > $(TEST_DIR)/$(TEST_DEP_1).el

downloads-latest :
	$(CURL) '$(TEST_DEP_1_LATEST_URL)' > $(TEST_DIR)/$(TEST_DEP_1).el

autoloads :
	$(EMACS) $(EMACS_BATCH) --eval                       \
	    "(progn                                          \
	      (setq generated-autoload-file \"$(WORK_DIR)/$(AUTOLOADS_FILE)\") \
	      (update-directory-autoloads \"$(WORK_DIR)\"))"

test-autoloads : autoloads
	@$(EMACS) $(EMACS_BATCH) -L . -l "./$(AUTOLOADS_FILE)"      || \
	 ( echo "failed to load autoloads: $(AUTOLOADS_FILE)" && false )

test-travis :
	@if test -z "$$TRAVIS" && test -e $(TRAVIS_FILE); then travis-lint $(TRAVIS_FILE); fi

test : build test-dep-1 test-autoloads
	@cd $(TEST_DIR)                                   && \
	(for test_lib in *-test.el; do                       \
	    $(EMACS) $(EMACS_BATCH) -L . -L .. -l cl -l $(TEST_DEP_1) -l $$test_lib --eval \
	    "(progn                                          \
	      (fset 'ert--print-backtrace 'ignore)           \
	      (ert-run-tests-batch-and-exit '(and \"$(TESTS)\" (not (tag :interactive)))))" || exit 1; \
	done)

clean :
	@rm -f $(AUTOLOADS_FILE) *.elc *~ */*.elc */*~ $(TEST_DIR)/$(TEST_DEP_1).el            \
        $(TEST_DIR)/$(TEST_DEP_2).el $(TEST_DIR)/$(TEST_DEP_3).el $(TEST_DIR)/$(TEST_DEP_4).el \
        $(TEST_DIR)/$(TEST_DEP_5).el $(TEST_DIR)/$(TEST_DEP_6).el $(TEST_DIR)/$(TEST_DEP_7).el \
        $(TEST_DIR)/$(TEST_DEP_8).el $(TEST_DIR)/$(TEST_DEP_9).el
