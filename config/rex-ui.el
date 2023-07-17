;;; -*- lexical-binding: t -*-
;;
(use-package info :elpaca nil
  :custom-face
  (info-menu-star
   ((t ( :foreground unspecified))))
  (Info-quoted
   ((t ( :inherit nil
         :inherit font-lock-function-name-face)))))

;; Huge theme pack
(use-package doom-themes
  :custom-face
  (line-number
   ((t ( :slant normal))))
  (line-number-current-line
   ((t ( :inherit nil
         :inherit 'default
         :slant normal
         :weight semi-bold))))
  :config (load-theme 'doom-nord-aurora t))

;; Flashy modeline
(use-package moody
  :config
  (setq x-underline-at-descent-line t)
  (setq moody-mode-line-height 38)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)
  (moody-replace-eldoc-minibuffer-message-function))

(use-package all-the-icons)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode)
  :custom-face
  (all-the-icons-dired-dir-face
   ((t ( :foreground unspecified
         :inherit font-lock-type-face)))))

(use-package all-the-icons-completion
  :ensure t
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

;; Show the results of C-x C-e directly in the buffer
(use-package eros
  :config
  (setq eros-eval-result-prefix "-> ")
  (eros-mode 1)
  :custom-face
  (eros-result-overlay-face
   ((t ( :box nil
         :inverse-video t)))))

;; Highlight hex color strings (and some other kinds) in the buffer
(use-package rainbow-mode
  :commands 'rainbow-mode)

;; better pdf support
(use-package pdf-tools
  :diminish pdf-view-midnight-minor-mode
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (setq-default pdf-view-display-size 'fit-width)
  (setq pdf-view-resize-factor 1.1)
  (setq pdf-view-use-scaling t)
  (setq pdf-view-use-imagemagick nil)

  (defun rex/ignore-errors (&rest r)
    (ignore-errors (apply (car r) (cdr r))))

  (advice-add 'pdf-view-goto-page :around #'rex/ignore-errors)

  :general
  (:keymaps 'pdf-view-mode-map
            "M-m" 'pdf-view-themed-minor-mode)
  :hook
  (pdf-view-mode . (lambda () (auto-composition-mode -1)))
  (pdf-view-mode . pdf-view-themed-minor-mode))

(use-package saveplace-pdf-view
  :after pdf-tools)

;; Show diffs in the fringe.
(use-package diff-hl
  :custom-face
  (diff-hl-insert
   ((t ( :background unspecified))))
  (diff-hl-change
   ((t ( :background unspecified))))
  (diff-hl-delete
   ((t ( :background unspecified))))
  :config
  (defun rex/diff-hl-settings-apply ()
    (interactive)
    (let* ((height (frame-char-height))
           (width 4)
           (ones (1- (expt 2 width)))
           (bits (make-vector height ones)))
      (define-fringe-bitmap 'rex/diff-hl-bitmap bits height width))
    (setq diff-hl-fringe-bmp-function (lambda (type pos) 'rex/diff-hl-bitmap)))
  :hook
  (prog-mode . diff-hl-mode)
  (diff-hl-mode . rex/diff-hl-settings-apply))

(use-package highlight-indent-guides
  :ensure t
  :custom
  ((highlight-indent-guides-responsive 'top)
   (highlight-indent-guides-method 'character)))
