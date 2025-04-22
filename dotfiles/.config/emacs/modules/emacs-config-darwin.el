;; MacOS workarounds
;; fix shell PATH
;; See: https://github.com/d12frosted/homebrew-emacs-plus/issues/720
;; See: https://emacs.stackexchange.com/questions/53904/why-cant-i-list-the-contents-of-desktop-on-macos-using-dired
(exec-path-from-shell-initialize)

;; use gnu ls
(setq insert-directory-program "/opt/homebrew/opt/coreutils/libexec/gnubin/ls")
(provide 'emacs-config-darwin)
