;; SPC map
(defconst my-leader-key "SPC" "The leader prefix key for Evil users.")

;; Which key
(which-key-mode t)
(setq which-key-idle-delay 0.2)

(defun custom/evil-shift-left-visual ()
  (interactive)
  (evil-shift-left (region-beginning) (region-end))
  (evil-normal-state)
  (evil-visual-restore))

(defun custom/evil-shift-right-visual ()
  (interactive)
  (evil-shift-right (region-beginning) (region-end))
  (evil-normal-state)
  (evil-visual-restore))

; Overload shifts so that they don't lose the selection
(evil-define-key 'visual global-map "<" 'custom/evil-shift-left-visual)
(evil-define-key 'visual global-map ">" 'custom/evil-shift-right-visual)

;; vertico keybindings
(keymap-set vertico-map "C-j" 'vertico-next)
(keymap-set vertico-map "C-k" 'vertico-previous)
(keymap-set vertico-map "M-h" 'vertico-directory-up)

; Custom evil-textobj-tree-sitter
;; bind `function.outer`(entire function block) to `f` for use in things like `vaf`, `yaf`
(define-key evil-outer-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.outer"))
(define-key evil-outer-text-objects-map "a" (evil-textobj-tree-sitter-get-textobj "class.outer"))

; gc to comment lines in visual mode
(define-key evil-visual-state-map "gc" 'evilnc-comment-operator)

; go to definition
(evil-define-key 'normal global-map "gd" 'xref-find-definitions)
; go to definition other window
(evil-define-key 'normal global-map "gD" 'xref-find-definitions-other-window)
; find implementation
(evil-define-key 'normal global-map "gi" 'eglot-find-implementation)
; find references
(evil-define-key 'normal global-map "gr" 'xref-find-references)

;; Evil-surround
(global-evil-surround-mode 1)
;; Spacemacs-like keybindings:
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.org#the-vim-surround-case
(evil-define-key 'visual evil-surround-mode-map "s" 'evil-surround-region)
(evil-define-key 'visual evil-surround-mode-map "S" 'evil-substitute)

;; Use visual line motions even outside of visual-line-mode buffers
(evil-global-set-key 'motion "j" 'evil-next-visual-line)
(evil-global-set-key 'motion "k" 'evil-previous-visual-line)

;; Evil-visualstar
(global-evil-visualstar-mode)

;; Evil-org
(evil-set-initial-state 'org-agenda-mode 'normal)
(evil-define-key 'normal org-agenda-mode-map
  ;; open
  (kbd "RET") 'org-agenda-switch-to
  (kbd "M-RET") 'org-agenda-recenter

  ;; motion
  "j" 'org-agenda-next-item
  "k" 'org-agenda-previous-item
  "J" 'org-agenda-todo-prev
  "K" 'org-agenda-todo-next
  "gj" 'org-agenda-next-line
  "gk" 'org-agenda-previous-line

  ;; priority
  "p" 'org-agenda-priority-up

  ;; operations
  "a" 'org-agenda-add-note
  "u" 'org-agenda-undo
  "q" 'org-agenda-exit

  ;; actions
  "D" 'org-agenda-kill
  "t" 'org-agenda-set-tags
  "e" 'org-agenda-set-effort
  "T" 'org-timer-set-timer
  "A" 'org-agenda-append-agenda
  "C" 'org-agenda-capture

  ;; mark
  "m" 'org-agenda-bulk-toggle
  "~" 'org-agenda-bulk-toggle-all
  "*" 'org-agenda-bulk-mark-all
  "%" 'org-agenda-bulk-mark-regexp
  "M" 'org-agenda-bulk-unmark-all
  "x" 'org-agenda-bulk-action

  ;; refresh
  "r" 'org-agenda-redo
  "R" 'org-agenda-redo-all

  ;; filter
  "sc" 'org-agenda-filter-by-category
  "sr" 'org-agenda-filter-by-regexp
  "se" 'org-agenda-filter-by-effort
  "st" 'org-agenda-filter-by-tag
  "s^" 'org-agenda-filter-by-top-headline
  "ss" 'org-agenda-limit-interactively
  "S" 'org-agenda-filter-remove-all

  ;; go and show
  "gC" 'org-agenda-convert-date
  "gd" 'org-agenda-goto-date
  "gt" 'org-agenda-show-tags

  ;; clock
  "I" 'org-agenda-clock-in ; Original binding
  "O" 'org-agenda-clock-out) ; Original binding

(add-hook 'org-agenda-after-show-hook
          (lambda ()
            (org-agenda-redo)))  ;; Refresh the agenda view

;; general package config
(general-evil-setup t)
(general-create-definer my-leader-def
  :keymaps '(normal visual emacs)
  :prefix my-leader-key)

;; Iedit
(setq iedit-current-symbol-default t
      iedit-only-at-symbol-boundaries t
      iedit-toggle-key-default nil)

(my-leader-def
  "TAB" '(evil-switch-to-windows-last-buffer :which-key "switch to previous window")
  "SPC" '(execute-extended-command :which-key "M-x")
  "1" '(winum-select-window-1 :which-key "select window 1")
  "2" '(winum-select-window-2 :which-key "select window 2")
  "3" '(winum-select-window-3 :which-key "select window 3")
  "4" '(winum-select-window-4 :which-key "select window 4")
  "5" '(winum-select-window-5 :which-key "select window 5")
  "6" '(winum-select-window-6 :which-key "select window 6")
  "7" '(winum-select-window-7 :which-key "select window 7")
  "8" '(winum-select-window-8 :which-key "select window 8")
  "9" '(winum-select-window-9 :which-key "select window 9"))

;; Kill all other buffers.
(defun custom/kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

;; Saves all buffers silently
(defun save-all ()
  "Saves all buffers silently."
  (interactive)
  (save-some-buffers t))

;; same as previous but this keybindings shouldn't be overriden by any modes
;; for example as dired-mode
(general-define-key
:states 'normal
:keymaps 'override
:prefix my-leader-key
  "TAB" '(evil-switch-to-windows-last-buffer :which-key "switch to previous window")
  "SPC" '(execute-extended-command :which-key "M-x")
  "1" '(winum-select-window-1 :which-key "select window 1")
  "2" '(winum-select-window-2 :which-key "select window 2")
  "3" '(winum-select-window-3 :which-key "select window 3")
  "4" '(winum-select-window-4 :which-key "select window 4")
  "5" '(winum-select-window-5 :which-key "select window 5")
  "6" '(winum-select-window-6 :which-key "select window 6")
  "7" '(winum-select-window-7 :which-key "select window 7")
  "8" '(winum-select-window-8 :which-key "select window 8")
  "9" '(winum-select-window-9 :which-key "select window 9"))

(my-leader-def
  "q"  '(:ignore t :which-key "quit/restart")
  "qq" '(evil-quit :which-key "quit")
  "qr" '(restart-emacs :which-key "restart"))
(my-leader-def
  "y"  '(:ignore t :which-key "toggles")
  "yt" '(load-theme t :which-key "choose theme"))
(my-leader-def
  "f"  '(:ignore t :which-key "files")
  "ft" '(dired-other-window :which-key "dired")
  "ff" '(find-file :which-key "find-file")
  "fs" '(save-buffer :which-key "save file")
  "fd" '(delete-file :which-key "delete file")
  "fr" '(rename-file :which-key "rename file")
  "fc" '(copy-file :which-key "copy file")
  "fS" '(save-all :which-key "save all"))
(my-leader-def
  "p"  '(:keymap project-prefix-map :package project))
(my-leader-def
  "t"       '(:keymap tabspaces-command-map :which-key "workspaces")
  "t1"      '(tab-bar-select-tab 1 :which-key "select workspace 1")
  "t2"      '(tab-bar-select-tab 2 :which-key "select workspace 2")
  "t3"      '(tab-bar-select-tab 3 :which-key "select workspace 3")
  "t4"      '(tab-bar-select-tab 4 :which-key "select workspace 4")
  "t5"      '(tab-bar-select-tab 5 :which-key "select workspace 5")
  "t6"      '(tab-bar-select-tab 6 :which-key "select workspace 6")
  "t7"      '(tab-bar-select-tab 7 :which-key "select workspace 7")
  "t8"      '(tab-bar-select-tab 8 :which-key "select workspace 8")
  "t9"      '(tab-bar-select-tab 9 :which-key "select workspace 9")
  "tn"      '(tab-bar-switch-to-next-tab :which-key "next workspace")
  "tr"      '(tab-bar-rename-tab :which-key "rename workspace")
  "t <tab>" '(tab-bar-switch-to-recent-tab :which-key "recent workspace")
  "tp"      '(tab-bar-switch-to-prev-tab :which-key "prev workspace"))
(my-leader-def
  "w"  '(evil-window-map t :which-key "windows")
  "wd" '(evil-window-delete t :which-key "window delete"))
(my-leader-def
  "s"  '(:ignore t :which-key "search/symbol")
  "sc" '(evil-ex-nohighlight :which-key "clear highlight")
  "se" '(iedit-mode :which-key "iedit"))
(my-leader-def
  "b"  '(:ignore t :which-key "buffers")
  "bs" '(save-buffer :which-key "save file")
  "bD" '(custom/kill-other-buffers :which-key "kill other buffers")
  "bb" '(consult-buffer :which-key "list buffers")
  "bn" '(evil-next-buffer :which-key "next buffer")
  "bp" '(evil-prev-buffer :which-key "previous buffer")
  "bh" '((lambda () (interactive) (switch-to-buffer "*Home*")) :which-key "home buffer")
  "bm" '((lambda () (interactive) (switch-to-buffer "*Message*")) :which-key "message buffer")
  "ba" '(org-agenda :which-key "agenda")
  "bd" '(kill-current-buffer t :which-key "delete buffer"))
(my-leader-def
  "g"  '(:ignore t :which-key "git")
  "gb" '(magit-blame-addition :which-key "blame"))
(my-leader-def
  "e"  '(:ignore t :which-key "errors")
  "eb" '(flymake-show-buffer-diagnostics :which-key "buffer errors")
  "el" '(flymake-show-project-diagnostics :which-key "list project errors")
  "en" '(flymake-goto-next-error :which-key "next error")
  "ep" '(flymake-goto-prev-error :which-key "prev errors"))
(my-leader-def
  "z"  '(:ignore t :which-key "zoom/narrow")
  "zi" '(text-scale-increase :which-key "zoom in")
  "zo" '(text-scale-decrease :which-key "zoom out")
  "zn"  '(:ignore t :which-key "narrow")
  "zni" '(narrow-to-region :which-key "narrow in")
  "zno" '(narrow-to-page :which-key "narrow out"))

;; Mode dependent leader keybindings
(general-create-definer mode-dependent-leader-def
  :states 'normal
  :keymaps 'override
  :prefix "SPC m")

(my-leader-def
  "m"  '(:ignore t :which-key "mode dependent leader"))
(mode-dependent-leader-def eglot-mode-map
  "r" '(eglot-rename :which-key "rename")
  "=" '(format-buffer-with-eglot :which-key "format buffer"))
(mode-dependent-leader-def go-ts-mode-map
  "i" '(go-import-add :which-key "add import"))
(mode-dependent-leader-def org-mode-map
  "l" '(org-node-insert-into-related :which-key "insert link into RELEATED drawer")
  "o" '(org-open-at-point :which-key "org open at point")
  "s" '(org-search-view :which-key "search")
  "t"  '(:ignore t :which-key "org toggles")
  "tc" '(org-toggle-checkbox :which-key "org toggle checkbox")
  "f" '(org-node-find :which-key "find node"))
(mode-dependent-leader-def org-agenda-mode-map
  "l" '(org-node-agenda-insert-into-related :which-key "insert link into RELEATED drawer")
  "f" '(org-node-find :which-key "find node"))

(provide 'emacs-config-input)
