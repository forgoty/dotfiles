(add-hook 'tex-mode-hook 'flyspell-mode)
(add-hook 'tex-mode-hook #'eglot-ensure)
(provide 'emacs-config-writing)
