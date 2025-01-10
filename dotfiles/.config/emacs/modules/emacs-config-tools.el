;;;; Copilot
(require 'copilot)
(require 'tramp)

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

;;;; Common flycheck
(defun custom/toggle-flycheck-error-list ()
  "Toggle flycheck's error list window.
If the error list is visible, hide it.  Otherwise, show it."
  (interactive)
  (if-let ((window (flycheck-get-error-list-window)))
      (save-selected-window (quit-window nil window))
    (flycheck-list-errors)))

(defun custom/goto-flycheck-error-list ()
  "Open and go to the error list buffer."
  (interactive)
  (if (flycheck-get-error-list-window)
      (switch-to-buffer flycheck-error-list-buffer)
    (flycheck-list-errors)
    (switch-to-buffer-other-window flycheck-error-list-buffer)))

;; Dired
(setq dired-kill-when-opening-new-dired-buffer t)
(setq dired-listing-switches "-lah --group-directories-first")

;; TRAMP
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)

(provide 'emacs-config-tools)
