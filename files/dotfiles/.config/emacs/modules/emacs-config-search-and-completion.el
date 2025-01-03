;;; Vertico
(when (require 'vertico nil :noerror)
  (require 'vertico-directory)
  ;; Cycle back to top/bottom result when the edge is reached
  (customize-set-variable 'vertico-cycle t)

  ;; Start Vertico
  (vertico-mode 1)

  (with-eval-after-load 'emacs-config-default
    (fido-mode -1)
    (fido-vertical-mode -1)
    (icomplete-mode -1)
    (icomplete-vertical-mode -1)))


;;; Marginalia
(when (require 'marginalia nil :noerror)
  ;; Configure Marginalia
  (customize-set-variable 'marginalia-annotators
                          '(marginalia-annotators-heavy
                            marginalia-annotators-light
                            nil))
  (marginalia-mode 1))


;;; Consult
;; Since Consult doesn't need to be required, we assume the user wants these
;; setting if it is installed (regardless of the installation method).
(when (locate-library "consult")
  ;; Set some consult bindings
  (keymap-global-set "C-s" 'consult-line)
  (keymap-set minibuffer-local-map "C-r" 'consult-history)

  (setq completion-in-region-function #'consult-completion-in-region))


;;; Orderless
(when (require 'orderless nil :noerror)
  ;; Set up Orderless for better fuzzy matching
  (customize-set-variable 'completion-styles '(orderless basic))
  (customize-set-variable 'completion-category-overrides
                          '((file (styles . (partial-completion))))))


;;; Embark
(when (require 'embark nil :noerror)

  (keymap-global-set "<remap> <describe-bindings>" #'embark-bindings)
  (keymap-global-set "C-." 'embark-act)

  ;; Use Embark to show bindings in a key prefix with `C-h`
  (setq prefix-help-command #'embark-prefix-help-command)

  (when (require 'embark-consult nil :noerror)
    (with-eval-after-load 'embark-consult
      (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode))))


;;; Corfu
(when (require 'corfu nil :noerror)

  (unless (display-graphic-p)
    (when (require 'corfu-terminal nil :noerror)
      (corfu-terminal-mode +1)))

  ;; Setup corfu for popup like completion
  (customize-set-variable 'corfu-cycle t)        ; Allows cycling through candidates
  (customize-set-variable 'corfu-auto t)         ; Enable auto completion
  (customize-set-variable 'corfu-auto-prefix 3)  ; Complete with less prefix keys
  (customize-set-variable 'corfu-auto-delay 0.5) ; No delay for completion
  (customize-set-variable 'corfu-echo-documentation 0.75) ; Echo docs for current completion option

  (global-corfu-mode 1)
  (when (require 'corfu-popupinfo nil :noerror)

    (corfu-popupinfo-mode 1)
    (eldoc-add-command #'corfu-insert)
    (keymap-set corfu-map "M-p" #'corfu-popupinfo-scroll-down)
    (keymap-set corfu-map "M-n" #'corfu-popupinfo-scroll-up)
    (keymap-set corfu-map "M-d" #'corfu-popupinfo-toggle)))


;;; Cape

(when (require 'cape nil :noerror)
  ;; Setup Cape for better completion-at-point support and more

  ;; Add useful defaults completion sources from cape
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)

  ;; Silence the pcomplete capf, no errors or messages!
  ;; Important for corfu
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)

  ;; Ensure that pcomplete does not write to the buffer
  ;; and behaves as a pure `completion-at-point-function'.
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)

  ;; No auto-completion or completion-on-quit in eshell
  (defun custom/completion-corfu-eshell ()
    "Special settings for when using corfu with eshell."
    (setq-local corfu-quit-at-boundary t
                corfu-quit-no-match t
                corfu-auto nil)
    (corfu-mode))
  (add-hook 'eshell-mode-hook #'custom/completion-corfu-eshell))


(setq consult-ripgrep-args
  "/bin/rg --null --line-buffered --color=never --max-columns=1000 --path-separator /\
   --smart-case --no-heading --with-filename --line-number --hidden --glob \"!{.git,node_modules,vendor,.venv,site-lisp}/*\"")

;; Skip directories in a project from search
(add-to-list 'vc-directory-exclusion-list "vendor")
(add-to-list 'vc-directory-exclusion-list ".venv")
(add-to-list 'vc-directory-exclusion-list "node_modules")
(add-to-list 'vc-directory-exclusion-list "site-lisp")
(add-to-list 'vc-directory-exclusion-list "zig-cache")
(add-to-list 'vc-directory-exclusion-list "zig-out")

;; Custom advice
(advice-add #'project-find-regexp :override 'consult-ripgrep)

(provide 'emacs-config-search-and-completion)
