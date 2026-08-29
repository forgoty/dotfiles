(require 'transient)

(defun evil-shift-left-visual ()
  (interactive)
  (evil-shift-left (region-beginning) (region-end))
  (evil-normal-state)
  (evil-visual-restore))

(defun evil-shift-right-visual ()
  (interactive)
  (evil-shift-right (region-beginning) (region-end))
  (evil-normal-state)
  (evil-visual-restore))

; Overload shifts so that they don't lose the selection
(evil-define-key 'visual global-map "<" 'evil-shift-left-visual)
(evil-define-key 'visual global-map ">" 'evil-shift-right-visual)

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

;; Iedit
(setq iedit-current-symbol-default t
      iedit-only-at-symbol-boundaries t
      iedit-toggle-key-default nil)

;; Kill all other buffers.
(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

;; Saves all buffers silently
(defun save-all ()
  "Saves all buffers silently."
  (interactive)
  (save-some-buffers t))

;; eca
(define-key eca-completion-map (kbd "<backtab>") 'eca-completion-accept)

;; transient key menu
(transient-define-prefix leader-quit-menu ()
  "Quit/restart."
  [["Quit"
    ("q" "quit" evil-quit)
    ("r" "restart" restart-emacs)]])

(transient-define-prefix leader-toggles-menu ()
  "Toggles."
  [["Toggles"
    ("t" "choose theme" load-theme)]])

(transient-define-prefix leader-files-menu ()
  "Files."
  [["Find"
    ("f" "find-file" find-file)
    ("t" "dired" dired-other-window)]
   ["Save"
    ("s" "save file" save-buffer)
    ("S" "save all" save-all)]
   ["Manage"
    ("d" "delete file" delete-file)
    ("r" "rename file" rename-file)
    ("c" "copy file" copy-file)]])

(transient-define-prefix leader-search-menu ()
  "Search/symbol."
  [["Search"
    ("c" "clear highlight" evil-ex-nohighlight)
    ("e" "iedit" iedit-mode)]])

(transient-define-prefix leader-buffers-menu ()
  "Buffers."
  [["Switch"
    ("b" "list buffers" consult-buffer)
    ("n" "next buffer" evil-next-buffer)
    ("p" "previous buffer" evil-prev-buffer)
    ("h" "home buffer" (lambda () (interactive) (switch-to-buffer "*Home*")))
    ("m" "message buffer" (lambda () (interactive) (switch-to-buffer "*Messages*")))
    ("a" "agenda" org-agenda)]
   ["Manage"
    ("s" "save buffer" save-buffer)
    ("r" "revert buffer" revert-buffer)
    ("d" "delete buffer" kill-current-buffer)
    ("y" "copy file path" copy-file-path)
    ("D" "kill other buffers" kill-other-buffers)]])

(transient-define-prefix leader-git-menu ()
  "Git."
  [["Git"
    ("b" "blame" magit-blame-addition)]])

(transient-define-prefix leader-errors-menu ()
  "Errors."
  [["Errors"
    ("b" "buffer errors" flymake-show-buffer-diagnostics)
    ("l" "list project errors" toggle-flymake-diagnostics-buffer)
    ("n" "next error" flymake-goto-next-error)
    ("p" "prev errors" flymake-goto-prev-error)]])

(transient-define-prefix leader-zoom-menu ()
  "Zoom/narrow."
  [["Zoom"
    ("i" "zoom in" text-scale-increase)
    ("o" "zoom out" text-scale-decrease)]
   ["Narrow"
    ("n i" "narrow in" narrow-to-region)
    ("n o" "narrow out" narrow-to-page)]])

(transient-define-prefix leader-project-menu ()
  "Project."
  [[("&" "async shell command" project-async-shell-command)
    ("C-b" "list buffers" project-list-buffers)
    ("D" "dired" project-dired)
    ("F" "or external find file" project-or-external-find-file)
    ("G" "or external find regexp" project-or-external-find-regexp)
    ("b" "switch to buffer" project-switch-to-buffer)]
   [("c" "compile" project-compile)
    ("d" "find dir" project-find-dir)
    ("e" "eshell" project-eshell)
    ("f" "find file" project-find-file)
    ("g" "find regexp" project-find-regexp)
    ("k" "kill buffers" project-kill-buffers)]
   [("p" "switch project" project-switch-project)
    ("r" "query replace regexp" project-query-replace-regexp)
    ("s" "shell" project-shell)
    ("v" "vc dir" project-vc-dir)
    ("x" "execute extended command" project-execute-extended-command)]
   [("y" "copy git relative path" copy-git-relative-file-path)]])

(transient-define-prefix leader-windows-menu ()
  "Windows."
  [[
    ("H" "move far left" evil-window-move-far-left)
    ("J" "move very bottom" evil-window-move-very-bottom)
    ("K" "move very top" evil-window-move-very-top)
    ("L" "move far right" evil-window-move-far-right)
    ("R" "rotate upwards" evil-window-rotate-upwards)
    ("S" "split" evil-window-split)]
   [("W" "prev" evil-window-prev)
    ("h" "left" evil-window-left)
    ("j" "down" evil-window-down)
    ("k" "up" evil-window-up)
    ("l" "right" evil-window-right)
    ("n" "new" evil-window-new)
    ("o" "delete other windows" delete-other-windows)]
   [("r" "rotate downwards" evil-window-rotate-downwards)
    ("s" "split" evil-window-split)
    ("t" "top left" evil-window-top-left)
    ("v" "vsplit" evil-window-vsplit)
    ("w" "next" evil-window-next)]
   [("d" "window delete" evil-window-delete)]])

(transient-define-prefix leader-workspaces-menu ()
  "Workspaces."
  [["Select"
    ("1" "workspace 1" (lambda () (interactive) (tab-bar-select-tab 1)))
    ("2" "workspace 2" (lambda () (interactive) (tab-bar-select-tab 2)))
    ("3" "workspace 3" (lambda () (interactive) (tab-bar-select-tab 3)))
    ("4" "workspace 4" (lambda () (interactive) (tab-bar-select-tab 4)))
    ("5" "workspace 5" (lambda () (interactive) (tab-bar-select-tab 5)))
    ("6" "workspace 6" (lambda () (interactive) (tab-bar-select-tab 6)))
    ("7" "workspace 7" (lambda () (interactive) (tab-bar-select-tab 7)))
    ("8" "workspace 8" (lambda () (interactive) (tab-bar-select-tab 8)))
    ("9" "workspace 9" (lambda () (interactive) (tab-bar-select-tab 9)))]
   ["Switch"
    ("n" "next workspace" tab-bar-switch-to-next-tab)
    ("p" "prev workspace" tab-bar-switch-to-prev-tab)
    ("TAB" "recent workspace" tab-bar-switch-to-recent-tab)
    ("r" "rename workspace" tab-bar-rename-tab)]
   ["Tabspaces"
    ("s" "switch or create" tabspaces-switch-or-create-workspace)
    ("o" "open project" tabspaces-open-or-create-project-and-workspace)
    ("b" "switch to buffer" tabspaces-switch-to-buffer)
    ("t" "buffer and tab" tabspaces-switch-buffer-and-tab)
    ("d" "close workspace" tabspaces-close-workspace)
    ("k" "kill buffers, close" tabspaces-kill-buffers-close-workspace)
    ("R" "remove selected buffer" tabspaces-remove-selected-buffer)
    ("C" "clear buffers" tabspaces-clear-buffers)]])

(transient-define-prefix leader-mode-menu ()
  "Mode dependent leader."
  [["LSP"
    :if (lambda () (bound-and-true-p eglot--managed-mode))
    ("r" "rename" eglot-rename)
    ("=" "format buffer" format-buffer-with-eglot)]
   ["Go"
    :if (lambda () (derived-mode-p 'go-ts-mode))
    ("i" "add import" go-import-add)]
   ["Org"
    :if (lambda () (derived-mode-p 'org-mode))
    ("l" "org insert link" org-super-links-quick-insert-drawer-link)
    ("o" "org open at point" org-open-at-point)
    ("s" "search" org-search-view)
    ("t c" "org toggle checkbox" org-toggle-checkbox)]
   ["Agenda"
    :if (lambda () (derived-mode-p 'org-agenda-mode))
    ("l" "org insert link" org-super-links-agenda-insert-link)]])

(transient-define-prefix leader-menu ()
  "Leader."
  [["Windows"
    ("1" "select window 1" winum-select-window-1)
    ("2" "select window 2" winum-select-window-2)
    ("3" "select window 3" winum-select-window-3)
    ("4" "select window 4" winum-select-window-4)
    ("5" "select window 5" winum-select-window-5)
    ("6" "select window 6" winum-select-window-6)
    ("7" "select window 7" winum-select-window-7)
    ("8" "select window 8" winum-select-window-8)
    ("9" "select window 9" winum-select-window-9)
    ("<tab>" "switch to previous window" evil-switch-to-windows-last-buffer)]
   ["Menus"
    ("a" "agent" eca-transient-menu)
    ("b" "buffers" leader-buffers-menu)
    ("e" "errors" leader-errors-menu)
    ("f" "files" leader-files-menu)
    ("g" "git" leader-git-menu)
    ("m" "mode dependent leader" leader-mode-menu)
    ("p" "project" leader-project-menu)
    ("q" "quit/restart" leader-quit-menu)
    ("s" "search/symbol" leader-search-menu)
    ("t" "workspaces" leader-workspaces-menu)
    ("w" "windows" leader-windows-menu)
    ("y" "toggles" leader-toggles-menu)
    ("z" "zoom/narrow" leader-zoom-menu)]
   ["Actions"
    ("SPC" "M-x" execute-extended-command)]])

(setq transient-show-menu 0.2)
(keymap-set transient-base-map "<escape>" 'transient-quit-all)
(evil-define-key '(normal visual motion emacs) 'global (kbd "SPC") 'leader-menu)

(provide 'emacs-config-input)
