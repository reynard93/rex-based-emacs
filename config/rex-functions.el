;;; -*- lexical-binding: t -*-
;;
;; This file defines any custom functions unrelated to specific
;; packages.
(use-package emacs :elpaca nil
  :config

  ;; find way to do swap window keybind

  ;; override window split behavior
  (defun rex/split-and-follow-horizontally ()
    "Split window below and place point in the new window."
    (interactive)
    (split-window-below)
    (other-window 1))
  (defun rex/split-and-follow-vertically ()
    "Split window right and place point in the new window."
    (interactive)
    (split-window-right)
    (other-window 1))

  ;; simple way to run pieces of a buffer as shell commands
  (defun rex/async-shell-command-on-region-or-line ()
    "Run the command at point or in the selected region in the shell."
    (interactive)
    (async-shell-command (if (use-region-p)
                             (buffer-substring (region-beginning) (region-end))
                           (thing-at-point 'line t))))

  (defun rex/shell-command-on-region-or-line ()
    "Run the command at point or in the selected region in the shell."
    (interactive)
    (shell-command (if (use-region-p)
                       (buffer-substring (region-beginning) (region-end))
                     (thing-at-point 'line t))))

  (defun rex/large-file-read-only ()
    "If a file is over a given size, make the buffer read only (and don't waste memory trying to use undo)"
    (when (> (buffer-size) (* 1024 1024))
      (setq buffer-read-only t)
      (buffer-disable-undo)))

  (defun rex/kill-relative-path ()
    "Kill the path to the currect file relative to the project root."
    (interactive)
    (kill-new (file-relative-name buffer-file-name (project-root (project-current t)))))

  (defun rex/docker-up ()
    "Start docker"
    (interactive)
    (let ((default-directory "/sudo::"))
      (shell-command "sv up docker")))

  (defun rex/docker-down ()
    "Stop docker"
    (interactive)
    (let ((default-directory "/sudo::"))
      (shell-command "sv down docker")))

  (defun rex/docker-compose-up ()
    "Run docker-compose up -d in the docker-local directory of the current project."
    (interactive)
    (async-shell-command (concat "docker-compose -f "
                    (project-root (project-current t))
                    "docker-local/docker-compose.yml up -d")))

  (defun rex/docker-compose-down ()
    "Run docker-compose down in the docker-local directory of the current project."
    (interactive)
    (async-shell-command (concat "docker-compose -f "
                    (project-root (project-current t))
                    "docker-local/docker-compose.yml down")))

  (defun rex/ansi-color-apply-on-region (begin end)
    (interactive "r")
    (ansi-color-apply-on-region begin end t))

  (defun rex/dark-theme ()
    "Switch to a dark theme."
    (interactive)
    (load-theme 'doom-nord-aurora t))

  (defun rex/light-theme ()
    "Switch to a light theme."
    (interactive)
    (load-theme 'doom-opera-light-alt t))

  :hook (find-file . rex/large-file-read-only))
