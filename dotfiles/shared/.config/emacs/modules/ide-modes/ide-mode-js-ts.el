(defun js-ts//hooks ()
  ;; Add flymake diagnostics to mode bar
  (add-to-list 'mode-line-misc-info
    `(flymake-mode (" " flymake-mode-line-counters " "))))

;; Associated go files with typescript-ts-mode
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))

(add-hook 'typescript-ts-mode-hook #'eglot-ensure)
(add-hook 'typescript-ts-mode-hook #'hs-minor-mode)
(add-hook 'typescript-ts-mode-hook 'js-ts//hooks)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))
(add-hook 'js-ts-mode-hook #'eglot-ensure)
(add-hook 'js-ts-mode-hook #'hs-minor-mode)
(add-hook 'js-ts-mode-hook 'js-ts//hooks)

(provide 'ide-mode-js-ts)
