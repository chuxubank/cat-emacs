EMACS ?= emacs
INIT_DIR ?= $(CURDIR)
PACKAGE_MANIFEST ?= $(INIT_DIR)/.local/package-manifest.eld
EMACS_BATCH = $(EMACS) --batch --debug-init --init-directory "$(INIT_DIR)"
PACKAGE_INITIALIZE = -l "$(INIT_DIR)/early-init.el" \
	--eval "(package-initialize)"
PACKAGE_ARCHIVES = -l "$(INIT_DIR)/core/config.el" \
	-l "$(INIT_DIR)/core/package/archives.el"
PACKAGE_BOOTSTRAP = $(PACKAGE_INITIALIZE) \
	$(PACKAGE_ARCHIVES) \
	--eval "(package-refresh-contents)" \
	-l "$(INIT_DIR)/init.el"
PACKAGE_MANIFEST_BOOTSTRAP = $(PACKAGE_INITIALIZE) \
	$(PACKAGE_ARCHIVES) \
	--funcall cat-package-refresh-contents-if-needed \
	-l "$(INIT_DIR)/init.el"
PACKAGE_GENERATED_BOOTSTRAP = $(PACKAGE_INITIALIZE) \
	-l "$(INIT_DIR)/core/package/archives.el" \
	--eval "(package-refresh-contents)" \
	-l "$(INIT_DIR)/core/package/manifest.el" \
	--eval "(cat-package-load-manifest \"$(PACKAGE_MANIFEST)\")"
PACKAGE_WRITE_MANIFEST = --eval "(cat-package-write-manifest \"$(PACKAGE_MANIFEST)\")"
PACKAGE_SYNC = --funcall cat-package-sync
PACKAGE_UPGRADE = --funcall cat-package-upgrade
PACKAGE_SYNC_UPGRADE = $(PACKAGE_SYNC) $(PACKAGE_UPGRADE)
PACKAGE_ACTION ?= $(PACKAGE_SYNC)

.PHONY: packages package-manifest sync-package-manifest sync-packages \
	upgrade-packages sync-upgrade-packages compile-org

packages:
	yes | $(EMACS_BATCH) $(PACKAGE_BOOTSTRAP) \
		$(PACKAGE_ACTION)

package-manifest:
	yes | $(EMACS_BATCH) $(PACKAGE_MANIFEST_BOOTSTRAP) \
		$(PACKAGE_WRITE_MANIFEST)

sync-package-manifest:
	yes | $(EMACS_BATCH) $(PACKAGE_GENERATED_BOOTSTRAP) \
		$(PACKAGE_SYNC)

sync-packages:
	$(MAKE) packages PACKAGE_ACTION='$(PACKAGE_SYNC)'

upgrade-packages:
	$(MAKE) packages PACKAGE_ACTION='$(PACKAGE_UPGRADE)'

sync-upgrade-packages:
	$(MAKE) packages PACKAGE_ACTION='$(PACKAGE_SYNC_UPGRADE)'

compile-org:
	$(MAKE) -C "$(INIT_DIR)/elpa/org-mode" compile autoloads
