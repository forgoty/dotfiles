;; -*- lexical-binding: t; -*-

;;; Consult
;; Since Consult doesn't need to be required, we assume the user wants these
;; setting if it is installed (regardless of the installation method).
(when (locate-library "consult")
  ;; Set some consult bindings
  (keymap-global-set "C-s" 'consult-line)
  (keymap-set minibuffer-local-map "C-r" 'consult-history)

  (setq completion-in-region-function #'consult-completion-in-region))

(setq consult-ripgrep-args
  "rg --null --line-buffered --color=never --max-columns=1000 --path-separator /\
   --smart-case --no-heading --with-filename --line-number --hidden --glob \"!{.git,node_modules,vendor,.venv,site-lisp}/*\"")

;;; Orderless
(when (require 'orderless nil :noerror)
  ;; Set up Orderless for better fuzzy matching
  (customize-set-variable 'completion-styles '(orderless basic))
  (customize-set-variable 'completion-category-overrides
                          '((file (styles . (partial-completion))))))

;;; Completions
;; update live
(setq completion-eager-display t)
(setq completion-eager-update t)

;; minibuffer ui
(setq completions-format 'one-column)
(setq completions-max-height 12)
(setq completions-header-format nil)
(setq completion-show-help nil)
(setq completion-show-inline-help nil)
(setq completions-detailed t)
(setq completions-sort 'historical)

;; ignore case and matching
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)
(setq completion-pcm-leading-wildcard t) 
(setq minibuffer-visible-completions t)
(setq completion-auto-help nil)
(setq completion-auto-select nil)
(setq completion-auto-wrap t)

;;; Skip directories in a project from search
(add-to-list 'vc-directory-exclusion-list "vendor")
(add-to-list 'vc-directory-exclusion-list ".venv")
(add-to-list 'vc-directory-exclusion-list "node_modules")
(add-to-list 'vc-directory-exclusion-list "site-lisp")
(add-to-list 'vc-directory-exclusion-list "zig-cache")
(add-to-list 'vc-directory-exclusion-list "zig-out")

;;; Custom advice
(advice-add #'project-find-regexp :override 'consult-ripgrep)

(provide 'emacs-config-search-and-completion)
