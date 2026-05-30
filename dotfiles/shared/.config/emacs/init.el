;;; Initial phase

;; Load custom.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (and custom-file
           (file-exists-p custom-file))
  (load custom-file nil 'nomessage))

;; update the `load-path' to include the Emacs modules path
(add-to-list 'load-path (expand-file-name "./modules/" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "./modules/ide-modes/" user-emacs-directory))

;; Packages phase
;; I decided to turn off the automatic download of packages. It's easy to do at startup.
(require 'emacs-config-packages)

;; Configuration phase
(require 'emacs-config-configuration)

;;;; Optional

;; Set default coding system (especially for Windows)
(set-default-coding-systems 'utf-8)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs loaded in %s."
                     (emacs-init-time))))
