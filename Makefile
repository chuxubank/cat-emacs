EMACS ?= emacs
INIT_DIR ?= $(CURDIR)
EMACS_BATCH = $(EMACS) --batch --debug-init --init-directory "$(INIT_DIR)"

.PHONY: sync-packages compile-org

sync-packages:
	yes | $(EMACS_BATCH) \
		-l "$(INIT_DIR)/early-init.el" \
		-l "$(INIT_DIR)/init.el" \
		--eval "(require 'package-vc)" \
		--eval "(package-initialize)" \
		--eval "(package-refresh-contents)" \
		--eval "(package-install-selected-packages t)" \
		--eval "(package-vc-install-selected-packages)" \
		--eval "(setq package-selected-packages (delete-dups (append (mapcar #'car package-vc-selected-packages) package-selected-packages)))" \
		--eval "(package-autoremove)"

compile-org:
	$(MAKE) -C "$(INIT_DIR)/elpa/org-mode" compile autoloads
