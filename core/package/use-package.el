;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(defvar use-package-keywords)

(defvar cat-font-rule-alist nil
  "Module font rules in declaration order.")

(defun cat-register-font-rule (owner rule)
  "Register OWNER's font RULE, replacing its previous declaration."
  (when (fboundp 'cat-font-validate-rule)
    (cat-font-validate-rule owner rule))
  (if-let* ((entry (assq owner cat-font-rule-alist)))
      (setcdr entry rule)
    (setq cat-font-rule-alist
          (append cat-font-rule-alist
                  (list (cons owner rule))))))

(defun use-package-handler/:cat (name _keyword args rest state)
  "Handler for the `:cat' keyword in `use-package'.
It skips the package declaration unless ARGS evaluates to non-nil."
  (when (eval args lexical-binding)
    (use-package-process-keywords name rest state)))

(defun use-package-normalize/:cat (name _keyword args)
  "Normalize the arguments for `:cat'.
With no argument, use NAME as the Cat feature.  With one argument, t and nil are
direct boolean values, a symbol names a Cat feature, and a list is treated as an
expression."
  (if (null args)
      `(catp! ,name)
    (use-package-only-one ":cat" args
      (lambda (_label arg)
        (cond
         ((memq arg '(t nil)) arg)
         ((symbolp arg) `(catp! ,arg))
         (t arg))))))

(defun cat-use-package--default-font-mode (name)
  "Return the conventional major mode associated with package NAME."
  (let ((name (symbol-name name)))
    (intern (if (string-suffix-p "-mode" name)
                name
              (concat name "-mode")))))

(defun use-package-normalize/:cat-font (name _keyword args)
  "Normalize a `:cat-font' declaration for package NAME."
  (use-package-only-one ":cat-font" args
    (lambda (_label arg)
      (let* ((spec (cond
                    ((symbolp arg) (list :font arg))
                    ((and (listp arg) (keywordp (car arg))) arg)
                    ((and (consp arg) (symbolp (car arg)))
                     (cons :font arg))
                    (t (use-package-error
                        ":cat-font expects ROLE or (ROLE :KEY VALUE...)"))))
             (allowed '(:modes :font :faces :rescale))
             (tail spec))
        (unless (zerop (% (length spec) 2))
          (use-package-error ":cat-font expects a property list"))
        (while tail
          (unless (memq (pop tail) allowed)
            (use-package-error ":cat-font contains an unknown property"))
          (pop tail))
        (unless (plist-member spec :modes)
          (setq spec (plist-put spec :modes
                                (cat-use-package--default-font-mode name))))
        (let ((font-present-p (plist-member spec :font))
              (role (plist-get spec :font))
              (faces-present-p (plist-member spec :faces))
              (faces (plist-get spec :faces)))
          (when (and font-present-p
                     (not (and role (symbolp role))))
            (use-package-error ":cat-font :font must name a role"))
          (when (and faces-present-p (not (listp faces)))
            (use-package-error ":cat-font :faces must be a list"))
          (dolist (face-rule faces)
            (unless (and (consp face-rule)
                         (symbolp (car face-rule))
                         (cadr face-rule)
                         (symbolp (cadr face-rule)))
              (use-package-error
               ":cat-font face rules must have the form (FACE ROLE ...)")))
          (unless (or (and font-present-p role)
                      (and faces-present-p faces))
            (use-package-error ":cat-font requires :font or :faces")))
        spec))))

(defun use-package-handler/:cat-font (name _keyword rule rest state)
  "Register normalized font RULE for package NAME."
  (use-package-concat
   `((cat-register-font-rule ',name ',rule))
   (use-package-process-keywords name rest state)))

(defun cat-package--position-cat-keyword ()
  "Ensure `:cat' is processed before `:ensure' in `use-package'.

`:ensure' unconditionally pushes package-install code regardless of the
body returned by inner keywords, so `:cat' can only prevent installation
when it runs as an outer (earlier) keyword than `:ensure'."
  (setq use-package-keywords (delq :cat use-package-keywords))
  (let ((position (or (cl-position :ensure use-package-keywords) 0)))
    (setq use-package-keywords
          (append (seq-take use-package-keywords position)
                  (list :cat)
                  (seq-drop use-package-keywords position)))))

(defun cat-package--position-font-keyword ()
  "Process `:cat-font' eagerly after conditional keywords."
  (setq use-package-keywords (delq :cat-font use-package-keywords))
  (let ((position (or (cl-position :catch use-package-keywords)
                      (length use-package-keywords))))
    (setq use-package-keywords
          (append (seq-take use-package-keywords position)
                  (list :cat-font)
                  (seq-drop use-package-keywords position)))))

(eval-after-load 'use-package-core
  '(progn
     (put 'use-package-handler/:cat
          'function-documentation
          "Skip package unless its Cat expression is non-nil")
     (put 'use-package-handler/:cat-font
          'function-documentation
          "Register semantic font roles for a package's major modes")
     (cat-package--position-cat-keyword)
     (cat-package--position-font-keyword)))

;; `use-package-ensure' prepends `:ensure' when it loads, so restore the order.
(eval-after-load 'use-package-ensure
  '(progn
     (cat-package--position-cat-keyword)
     (cat-package--position-font-keyword)))

(provide 'cat-package-use-package)
