;;; Code:
(defun c//hooks ()
  "Call this when c-ts-mode is enabled."
  (add-hook 'before-save-hook (lambda ()
                                (call-interactively #'eglot-format-buffer)
                                (call-interactively #'eglot-code-action-organize-imports))))


;; Remap c-mode to c-ts-mode
(add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
(add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
(add-to-list 'major-mode-remap-alist
              '(c-or-c++-mode . c-or-c++-ts-mode))


(add-hook 'c-ts-mode-hook #'eglot-ensure)
(add-hook 'c-ts-mode-hook 'c//hooks)

;; Enable folding
(add-hook 'c-ts-mode-hook 'hs-minor-mode)

;; Default language server is ccls for c-ts-mode
(with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '((c-ts-mode c++-ts-mode) "ccls"
 					  "-init={\"compilationDatabaseDirectory\":\"build\"}")))
(provide 'ide-mode-c)
