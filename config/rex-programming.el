;;; -*- lexical-binding: t -*-
;;
;; General programming utilities / settings
;; ****************************************
;; TODO: set up the integrated version
;; (use-package tree-sitter
;;   :diminish tree-sitter-mode)
;; (use-package tree-sitter-langs)
(use-package emacs
  :elpaca nil
  :init
  (setq treesit-font-lock-level 4)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
  )
(use-package treesit-auto
  :elpaca (:host github :repo "renzmann/treesit-auto")
  :config (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode))

(use-package flycheck
  :hook ((prog-mode . flycheck-mode))
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enabled newline))
  (setq flycheck-display-errors-delay 0.1))

(use-package flymake :elpaca nil
  :init
  (setq-default flymake-no-changes-timeout 1)
  :config
  (setq flymake-mode-line-format
        '(" " flymake-mode-line-exception flymake-mode-line-counters))
  :general
  (rex-leader
    "tf" 'flymake-mode
    "fe" 'flycheck-list-errors
    ))

(use-package emacs :elpaca nil
  :hook (after-save . executable-make-buffer-file-executable-if-script-p))

(use-package fancy-compilation
  :commands 'fancy-compilation-mode
  :config
  (with-eval-after-load 'compile
    (fancy-compilation-mode)))

(use-package compile :elpaca nil
  :config
  (require 'ansi-color)
  (defun rex/ansi-colorize-buffer ()
    (let ((buffer-read-only nil))
      (ansi-color-apply-on-region (point-min) (point-max))))
  (add-hook 'compilation-filter-hook 'rex/ansi-colorize-buffer)
  :custom-face
  (compilation-warning
   ((t ( :slant normal))))
  (compilation-error
   ((t ( :weight normal)))))

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode))

(use-package highlight-defined
  :hook (emacs-lisp-mode . highlight-defined-mode))

(use-package highlight-quoted
  :hook (emacs-lisp-mode . highlight-quoted-mode))

(use-package imenu-list
  :config
  (setq imenu-list-mode-line-format nil)
  (setq imenu-list-position 'left)
  (setq imenu-list-size 0.2)
  :general
  (rex-leader
    "sl" 'imenu-list))

(use-package dumb-jump
  :init
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
  :config
  (add-hook 'xref-backend-functions  #'dumb-jump-xref-activate))

(use-package string-inflection)

;; Language server support
;; ****************************************
(use-package eglot :elpaca nil
  :defer t
  :commands (eglot)
  :custom-face
  (eglot-inlay-hint-face
   ((t ( :foreground unspecified
         :inherit font-lock-comment-face))))
  :general (rex-leader
    "cf" 'eglot-format-buffer
    "cd" 'eglot-find-declaration
    "cD" 'eglot-find-implementation
    "cr" 'eglot-rename
    "ca" 'eglot-code-actions)
  :config (add-to-list 'eglot-server-programs
                       '(php-mode . ("phpactor" "language-server")))
  :hook (eglot-managed-mode . (lambda ()
                                (setq eldoc-documentation-function
                                      'eldoc-documentation-compose-eagerly))))

(use-package eldoc-box
  :defer t
  :config
  (defun rex/eldoc-box-scroll-up ()
    "Scroll up in `eldoc-box--frame'"
    (interactive)
    (with-current-buffer eldoc-box--buffer
      (with-selected-frame eldoc-box--frame
        (scroll-down 3))))
  (defun rex/eldoc-box-scroll-down ()
    "Scroll down in `eldoc-box--frame'"
    (interactive)
    (with-current-buffer eldoc-box--buffer
      (with-selected-frame eldoc-box--frame
        (scroll-up 3))))
  :general
  (:keymaps 'eglot-mode-map
            "C-k" 'rex/eldoc-box-scroll-up
            "C-j" 'rex/eldoc-box-scroll-down
            "M-h" 'eldoc-box-help-at-point))

;; Language config EGLOT
;; ****************************************
;; (use-package java-mode :elpaca nil
;;   :ensure nil
;;   :hook
;;   (java-mode . eglot-ensure)
;;   (java-mode . (lambda () (cl-defmethod eglot-execute-command
;;                             (_server (_cmd (eql java.apply.workspaceEdit)) arguments)
;;                             "Eclipse JDT breaks spec and replies with edits as arguments."
;;                             (mapc #'eglot--apply-workspace-edit arguments)))))

(use-package php-mode
  :hook
  (php-mode . eglot-ensure)
  :general
  (:keymaps '(normal php-mode)
            "gr" 'xref-find-references))

(use-package lua-mode
  :mode ("\\.lua$" . lua-mode))

(use-package yaml-mode
  :mode
  ("\\.yml.dist$" . yaml-mode)
  ("\\.yml$" . yaml-mode))

(use-package ess
  :hook (ess-r-mode . eglot-ensure)
  :mode ("\\.R$" . R-mode))

(use-package markdown-mode
  :custom-face
  (markdown-code-face
   ((t ( :background unspecified))))
  :mode ("\\.md$" . markdown-mode)
  :hook (markdown-mode . olivetti-mode))

(use-package web-mode
  :mode
  ("\\.html$" . web-mode)
  ("\\.twig$" . web-mode)
  ("\\.vue$" . web-mode)
  :config
  (setq web-mode-code-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-markup-indent-offset 2
        web-mode-sql-indent-offset 2
        web-mode-script-padding 0       ; start script in col 0
        web-mode-enable-current-column-highlight t
        )
  :hook
  (web-mode . (lambda () (electric-indent-local-mode -1)))
)

(use-package nxml-mode :elpaca nil
  :ensure nil
  :mode ("\\.fxml$" . nxml-mode)
  :config
  (setq nxml-child-indent 4))

(use-package htmlize)

(use-package impatient-mode)

(use-package js
  :elpaca nil
  :ensure nil
  :mode ("\\.js$" . js-mode))

(use-package typescript-mode
  :mode(
  "\\.ts$" . typescript-mode))

;; https://ianyepan.github.io/posts/emacs-ide/
(use-package lsp-mode
  :init
  (defun ii/lsp-mode-setup-completion()
  (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
        '(orderless)))
  :hook (
         ((c-mode          ; clangd
          c++-mode        ; clangd
          c-or-c++-mode   ; clangd
          java-mode       ; eclipse-jdtls
          js-mode         ; ts-ls (tsserver wrapper)
          js-jsx-mode     ; ts-ls (tsserver wrapper)
          typescript-mode ; ts-ls (tsserver wrapper)
          python-mode     ; pyright
          web-mode        ; ts-ls/HTML/CSS
          haskell-mode    ; haskell-language-server
          ) . lsp-deferred)
         (lsp-completion-mode . ii/lsp-mode-setup-completion)
         )
  :commands lsp
  :config
  (setq lsp-auto-guess-root t)
  (setq lsp-log-io nil)
  (setq lsp-restart 'auto-restart)
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-signature-auto-activate nil)
  (setq lsp-signature-render-documentation nil)
  (setq lsp-eldoc-hook nil)
  (setq lsp-modeline-code-actions-enable nil)
  (setq lsp-modeline-diagnostics-enable nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-semantic-tokens-enable nil)
  (setq lsp-enable-folding nil)
  (setq lsp-enable-imenu nil)
  (setq lsp-enable-snippet nil)
  (setq read-process-output-max (* 1024 1024)) ;; 1MB
  (setq lsp-prefer-capf t) ; prefer lsp's company-capf over company-lsp
 ;; (add-to-list 'lsp-language-id-configuration '(js-jsx)
  (setq lsp-idle-delay 0.5)
  (setq lsp-completion-provider :none)
  :general (rex-leader
    "cf" 'lsp-format-buffer
    "cd" 'lsp-find-declaration
    "cD" 'lsp-find-implementation
    "cr" 'lsp-rename
    "ca" 'lsp-code-actions)
  )

(use-package lsp-ui
  :config
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-peek-enable t)
  (setq lsp-ui-doc-header t)
  (setq lsp-ui-doc-include-signature t)
  (setq lsp-ui-doc-border (face-foreground 'default))
  (setq lsp-ui-sideline-show-code-actions t)
  (setq lsp-ui-sideline-delay 0.05))
