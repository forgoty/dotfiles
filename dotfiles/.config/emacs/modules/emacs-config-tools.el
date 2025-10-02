(require 'copilot)
(require 'tramp)
(require 'direnv)

(defun custom/copilot-tab ()
  "Tab command that will complet with copilot if a completion is
available. Otherwise will try normal
tab-indent."
  (interactive)
  (if (copilot--overlay-visible)
      (copilot-accept-completion))
  (copilot-complete))

(define-key copilot-mode-map (kbd "<backtab>") 'custom/copilot-tab)
(define-key copilot-mode-map (kbd "M-k") #'copilot-next-completion)
(define-key copilot-mode-map (kbd "M-j") #'copilot-previous-completion)
(setq copilot-max-char-warning-disable t)
(setq copilot-indent-offset-warning-disable t)

(defun copilot-mode-custom-hook ()
  (unless (member major-mode '(org-mode))
    (copilot-mode 1)))

(add-hook 'emacs-startup-hook
          (lambda ()
            (add-hook 'text-mode-hook #'copilot-mode-custom-hook)
            (add-hook 'prog-mode-hook #'copilot-mode-custom-hook)))

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

(defun format-buffer-with-eglot ()
  (interactive)
  (when (bound-and-true-p eglot--managed-mode)
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

;; direnv
(direnv-mode)

(provide 'emacs-config-tools)
