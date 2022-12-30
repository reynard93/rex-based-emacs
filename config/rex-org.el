;;; -*- lexical-binding: t -*-
;;
;; General settings
(use-package org
  :config
  (setq-default org-agenda-window-setup 'current-window)
  (setq org-fontify-whole-heading-line t)
  (setq org-startup-indented t)
  (setq org-startup-folded nil)
  (setq org-hide-leading-stars t)
  :hook
  (org-mode . auto-fill-mode))

;; Font settings
(use-package org
  :config
  (setq org-ellipsis " ⯆")
  (setq rex/org-levels
        '(org-level-1 org-level-2
          org-level-3 org-level-4
          org-level-5 org-level-6
          org-level-7 org-level-8))
  (dolist (face rex/org-levels)
    (set-face-attribute face nil :inherit nil :weight 'bold))
  (dolist (face '(org-block org-block-begin-line org-block-end-line))
    (set-face-attribute face nil :background nil)))

(use-package org-superstar
  :hook (org-mode . (lambda () (org-superstar-mode 1)))
  :config
  (setq org-superstar-headline-bullets-list '("●")))
