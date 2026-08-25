(require 'tramp)
(require 'eca)

;; Other
(defun copy-file-path ()
  (interactive)
  (kill-new (buffer-file-name)))

(defun copy-project-relative-file-path ()
  (interactive)
  (kill-new (file-relative-name (buffer-file-name) (project-root (project-current t)))))

(defun copy-git-relative-file-path ()
  (interactive)
  (kill-new (file-relative-name (buffer-file-name) (vc-root-dir))))

;; Set to nil in .dir-locals.el to disable auto-formatting for a specific project.
(defvar enable-format-on-save t)

(defun format-buffer-with-eglot ()
  (interactive)
  (when (and (bound-and-true-p eglot--managed-mode)
             enable-format-on-save)
    (save-excursion
      (condition-case err
          (call-interactively #'eglot-code-action-organize-imports)
        (error
         (message (error-message-string err))))
      (call-interactively #'eglot-format-buffer))))

;; Dired
(setq dired-kill-when-opening-new-dired-buffer t)
(setq dired-listing-switches "-lah --group-directories-first")

;; TRAMP
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)

;; envrc
(add-hook 'after-init-hook 'envrc-global-mode)

;; eca
(setq eca-chat-hide-markdown-markup nil
      eca-chat-diff-tool 'ediff
      eca-chat-use-side-window nil)
(add-hook 'emacs-startup-hook (lambda ()
  (add-hook 'text-mode-hook #'eca-completion-mode)
  (add-hook 'prog-mode-hook #'eca-completion-mode)))

(provide 'emacs-config-tools)
