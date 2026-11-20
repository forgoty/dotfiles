;; -*- lexical-binding: t; -*-

;; Set some variables that must be configured before loading the package
(customize-set-variable 'evil-want-integration t)
(customize-set-variable 'evil-want-keybinding nil)
(customize-set-variable 'evil-want-C-i-jump nil)
(customize-set-variable 'evil-respect-visual-line-mode t)
;; C-h is backspace in insert state
(customize-set-variable 'evil-want-C-h-delete t)
;; Set undo-system
(customize-set-variable 'evil-undo-system 'undo-redo)

;; Load Evil and enable it globally
(require 'evil)
(evil-mode 1)

;; Load evil-anzu to improve search
(with-eval-after-load 'evil
  (require 'evil-anzu))

;; Turn on Evil Nerd Commenter
(evilnc-default-hotkeys)

;; Make sure some modes start in Emacs state
(dolist (mode '(custom-mode
                eshell-mode
                term-mode))
  (add-to-list 'evil-emacs-state-modes mode))

;; Evil Collection
(setq evil-collection-key-blacklist '("SPC"))
(evil-collection-init)

(provide 'emacs-config-evil)
