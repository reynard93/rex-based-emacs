;;; -*- lexical-binding: t -*-
;;
;; Add support for code completion and snippets.
(use-package emacs :elpaca nil
  :config
  (setq tab-always-indent 'complete))

;; Corfu displays a popup with current candidates from the active
;; completion-at-point-function
(use-package corfu
  :elpaca (:files (:defaults "extensions/*"))
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  (corfu-echo-mode)

  :custom
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-min-width 40)
  (corfu-max-width 80)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.2)
  (corfu-echo-documentation 0.25)
  (corfu-preselect 'prompt)
  (corfu-preselect-first nil)

  :config
  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      (setq-local corfu-echo-delay nil
                  corfu-popupinfo-delay nil)
      (corfu-mode 1)))

  :general
  (:keymaps 'corfu-map
            "RET" nil
            "C-j" 'corfu-next
            [tab] 'corfu-next
            "C-k" 'corfu-previous
            [backtab] 'corfu-previous)

  :hook
  (minibuffer-setup . corfu-enable-in-minibuffer)
  (eshell . (lambda ()
                    (setq-local corfu-auto nil)
                    (corfu-mode))))

(use-package kind-icon
  :if (display-graphic-p)
  :ensure t
  :after corfu
  :custom
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default)
  (kind-icon-blend-background nil)
  (kind-icon-blend-frac 0.08)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; Cape provides capfs for better in-buffer completion, as well as ways to
;; combine / transform such functions.
(use-package cape
  :init
  (setq rex/capfs '(cape-dabbrev cape-file))
  (defun rex/add-capfs ()
    (dolist (fkt rex/capfs)
    (add-to-list 'completion-at-point-functions fkt)))
  (rex/add-capfs)
  :general
  (rex-leader
    "cd" 'cape-dabbrev
    "cf" 'cape-file))

;; Tempel provides a framework for defining / using snippets in plain elisp.
(use-package tempel
  :init;;https://github.com/sochotnicky/tempel/blob/b3e85ee85d8c32d0f9a82960c0ae7f584439ace1/README.org
  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)
  ;; make the Tempel templates available to Abbrev,
  ;; either locally or globally. `expand-abbrev' is bound to C-x '.
  (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  (global-tempel-abbrev-mode)
  :general
  (rex-leader
    "ct" 'tempel-insert)
  (:keymaps 'prog-mode-map
            "M-RET" 'tempel-expand)
  (:keymaps 'org-mode-map
            "C-c e" 'tempel-expand)
  (:keymaps 'tempel-map
            "TAB" 'tempel-next
            "S-TAB" 'tempel-previous))

;; Some predefined snippets
(use-package tempel-collection
  :elpaca (:host github :repo "Crandel/tempel-collection")
  :init (require 'tempel-collection)
  :after tempel)
