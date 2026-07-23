EMACS ?= emacs
INIT_DIR ?= $(CURDIR)
EMACS_BATCH = $(EMACS) --batch --debug-init --init-directory "$(INIT_DIR)"
PACKAGE_BOOTSTRAP = -l "$(INIT_DIR)/early-init.el" --eval "(package-initialize)" -l "$(INIT_DIR)/init.el" --eval "(package-refresh-contents)"
PACKAGE_SYNC = --funcall cat-package-sync
PACKAGE_UPGRADE = --funcall cat-package-upgrade
PACKAGE_SYNC_UPGRADE = $(PACKAGE_SYNC) $(PACKAGE_UPGRADE)
PACKAGE_ACTION ?= $(PACKAGE_SYNC)

.PHONY: packages sync-packages upgrade-packages sync-upgrade-packages compile-org

packages:
	yes | $(EMACS_BATCH) $(PACKAGE_BOOTSTRAP) \
		$(PACKAGE_ACTION)

sync-packages:
	$(MAKE) packages PACKAGE_ACTION='$(PACKAGE_SYNC)'

upgrade-packages:
	$(MAKE) packages PACKAGE_ACTION='$(PACKAGE_UPGRADE)'

sync-upgrade-packages:
	$(MAKE) packages PACKAGE_ACTION='$(PACKAGE_SYNC_UPGRADE)'

compile-org:
	$(MAKE) -C "$(INIT_DIR)/elpa/org-mode" compile autoloads
