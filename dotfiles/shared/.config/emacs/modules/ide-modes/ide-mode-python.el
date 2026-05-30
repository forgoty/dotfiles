(defconst python-indent-level 4)

(defun python//hooks ()
  "Call this when python-ts-mode is enabled."
  (setq-local tab-width python-indent-level)
  (setq-local python-indent-offset python-indent-level)
  (setq-local evil-shift-width python-indent-level)
  (setq-local indent-tabs-mode nil)
  (setq-local eglot-workspace-configuration
              '(:pylsp (:plugins (:ruff (:enabled t
                                         :formatEnabled t
                                         :lineLength 130)))))
  (add-hook 'before-save-hook #'format-buffer-with-eglot nil t))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((python-ts-mode python-mode) . ("pylsp"))))

(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

(add-hook 'python-ts-mode-hook #'python//hooks)
(add-hook 'python-ts-mode-hook #'eglot-ensure)
(add-hook 'python-ts-mode-hook #'hs-minor-mode)

(provide 'ide-mode-python)
