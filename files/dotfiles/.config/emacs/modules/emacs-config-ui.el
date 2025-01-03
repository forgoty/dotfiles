(when (require 'helpful nil :noerror)
  (keymap-set helpful-mode-map "<remap> <revert-buffer>" #'helpful-update)
  (keymap-global-set "<remap> <describe-command>"        #'helpful-command)
  (keymap-global-set "<remap> <describe-function>"       #'helpful-callable)
  (keymap-global-set "<remap> <describe-key>"            #'helpful-key)
  (keymap-global-set "<remap> <describe-symbol>"         #'helpful-symbol)
  (keymap-global-set "<remap> <describe-variable>"       #'helpful-variable)
  (keymap-global-set "C-h F"                             #'helpful-function))

;; Bind extra `describe-*' commands
(keymap-global-set "C-h K" #'describe-keymap)

;; Make gruvbox theme default
(load-theme 'doom-gruvbox t)

;; Modeline
(doom-modeline-mode t)
(setq doom-modeline-highlight-modified-buffer-name t
      doom-modeline-height 15
      doom-modeline-bar-width 6
      doom-modeline-lsp t
      doom-modeline-github nil
      doom-modeline-mu4e nil
      doom-modeline-irc nil
      doom-modeline-persp-name nil
      doom-modeline-persp-icon nil
      doom-modeline-env-version nil
      doom-modeline-gnus nil
      doom-modeline-buffer-encoding t
      doom-modeline-buffer-file-name-style 'relative-from-project
      doom-modeline-major-mode-icon nil)

;; Enable window numbers in modeline
(winum-mode)

;; Golden Ratio
(golden-ratio-mode t)
(add-hook 'window-selection-change-functions 'golden-ratio)
(setq golden-ratio-exclude-modes '(ediff-mode))

;; Workspaces
(with-eval-after-load 'tabspaces
  (customize-set-variable 'tabspaces-use-filtered-buffers-as-default t)
  (customize-set-variable 'tabspaces-remove-to-default t)
  (customize-set-variable 'tabspaces-include-buffers '("*scratch*")))
;;; Activate workspaces
(customize-set-variable 'tabspaces-mode t)

;; Inside workspace make consult see buffers only from the workspace
(with-eval-after-load 'consult
  ;; hide full buffer list (still available with "b" prefix)
  (consult-customize consult--source-buffer :hidden t :default nil)
  ;; set consult-workspace buffer list
  (defvar consult--source-workspace
    (list :name     "Workspace Buffers"
          :narrow   ?w
          :history  'buffer-name-history
          :category 'buffer
          :state    #'consult--buffer-state
          :default  t
          :items    (lambda () (consult--buffer-query
                          :predicate #'tabspaces--local-buffer-p
                          :sort 'visibility
                          :as #'buffer-name)))

    "Set workspace buffer list for consult-buffer.")
  (add-to-list 'consult-buffer-sources 'consult--source-workspace))

(defun custom/consult-tabspaces ()
  "Deactivate isolated buffers when not using tabspaces."
  (require 'consult)
  (cond (tabspaces-mode
         ;; hide full buffer list (still available with "b")
         (consult-customize consult--source-buffer :hidden t :default nil)
         (add-to-list 'consult-buffer-sources 'consult--source-workspace))
        (t
         ;; reset consult-buffer to show all buffers
         (consult-customize consult--source-buffer :hidden nil :default t)
         (setq consult-buffer-sources (remove #'consult--source-workspace consult-buffer-sources)))))

(add-hook 'tabspaces-mode-hook #'custom/consult-tabspaces)

;; Make sure project is initialized
(project--ensure-read-project-list)

(provide 'emacs-config-ui)
