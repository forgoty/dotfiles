(require 'emacs-config-default)
(require 'emacs-config-startup)
(require 'emacs-config-tools)
(require 'emacs-config-search-and-completion)
(require 'emacs-config-evil)
(require 'emacs-config-ui)
(require 'emacs-config-ide)
(require 'emacs-config-org)
(require 'emacs-config-writing)
(require 'emacs-config-input)

;; Darwin related
(when (equal system-type 'darwin)
  (require 'emacs-config-darwin))

(provide 'emacs-config-configuration)
