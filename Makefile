EMACS ?= emacs
INIT_DIR ?= $(CURDIR)
EMACS_BATCH = $(EMACS) --batch --debug-init --init-directory "$(INIT_DIR)"
PACKAGE_BOOTSTRAP = -l "$(INIT_DIR)/early-init.el" -l "$(INIT_DIR)/init.el" --eval "(package-initialize)" --eval "(package-refresh-contents)"
PACKAGE_SYNC = --eval "(package-install-selected-packages t)" --eval "(package-vc-install-selected-packages)"
PACKAGE_UPGRADE = --eval "(package-upgrade-all nil)" --eval "(package-vc-upgrade-all)"
PACKAGE_SYNC_UPGRADE = $(PACKAGE_SYNC) $(PACKAGE_UPGRADE)
PACKAGE_ACTION ?= $(PACKAGE_SYNC)
PACKAGE_CLEANUP = --eval "(setq package-selected-packages (delete-dups (append (mapcar 'car package-vc-selected-packages) package-selected-packages)))" --eval "(package-autoremove)"

.PHONY: packages sync-packages upgrade-packages sync-upgrade-packages compile-org

packages:
	yes | $(EMACS_BATCH) $(PACKAGE_BOOTSTRAP) \
		$(PACKAGE_ACTION) \
		$(PACKAGE_CLEANUP)

sync-packages:
	$(MAKE) packages PACKAGE_ACTION='$(PACKAGE_SYNC)'

upgrade-packages:
	$(MAKE) packages PACKAGE_ACTION='$(PACKAGE_UPGRADE)'

sync-upgrade-packages:
	$(MAKE) packages PACKAGE_ACTION='$(PACKAGE_SYNC_UPGRADE)'

compile-org:
	$(MAKE) -C "$(INIT_DIR)/elpa/org-mode" compile autoloads
