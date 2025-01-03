;; Associated go files with typescript-ts-mode
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))

(add-hook 'typescript-ts-mode-hook #'eglot-ensure)
(add-hook 'typescript-ts-mode-hook 'flycheck-mode)
(add-hook 'typescript-ts-mode-hook #'hs-minor-mode)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))
(add-hook 'js-ts-mode-hook #'eglot-ensure)
(add-hook 'js-ts-mode-hook 'flycheck-mode)
(add-hook 'js-ts-mode-hook #'hs-minor-mode)

(provide 'ide-mode-js-ts)
