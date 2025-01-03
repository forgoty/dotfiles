;;; Initial phase

;; Load custom.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (and custom-file
           (file-exists-p custom-file))
  (load custom-file nil 'nomessage))

;; update the `load-path' to include the Emacs modules path
(add-to-list 'load-path (expand-file-name "./modules/" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "./modules/ide-modes/" user-emacs-directory))

(defun custom/save-customized ()
  "Save and reload the customizations made during Emacs initialization.

Due to the way Emacs Customization works - or seems to - and this
bug: https://debbugs.gnu.org/cgi/bugreport.cgi?bug=21355, we need
to save all customizations made during Emacs startup and then
reload the custom-file.  This sets (or should set) all customized
values to the \"SET and saved.\" state and (hopefully) avoid the
bug above.  If the user never set a value for `custom-file' then
we can't reload the file."
  (customize-save-customized)
  ;; only load the `custom-file' if it is not `nil'.
  (when custom-file
    (load custom-file :noerror)))

;; Save all customizations to `custom-file', unless the user opted out.
;; (add-hook 'after-init-hook #'custom/save-customized)
(add-hook 'after-init-hook #'package--save-selected-packages)

;;;; Packages phase
;; Collect list of packages to install. Do not just blindly copy this
(require 'emacs-config-packages)

;; Install the packages listed in the `package-selected-packages' list.
(package-install-selected-packages :noconfirm)

;;;; Configuration phase
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
