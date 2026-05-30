;; MacOS workarounds
;; fix shell PATH
;; See: https://github.com/d12frosted/homebrew-emacs-plus/issues/720
;; See: https://emacs.stackexchange.com/questions/53904/why-cant-i-list-the-contents-of-desktop-on-macos-using-dired
(exec-path-from-shell-initialize)

;; use gnu ls
(setq insert-directory-program "/opt/homebrew/opt/coreutils/libexec/gnubin/ls")

;; Tree-Sitter confiugration
;; I install grammars manually for macos because macos can't run GNU Guix which I use on other systems to install grammars.
;; Perhaps when brew supports tree-sitter grammars installation we can switch to that.
(setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (c "https://github.com/tree-sitter/tree-sitter-c")
     (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (latex "https://github.com/latex-lsp/tree-sitter-latex" "master" "src")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
     (plantuml "https://github.com/lyndsysimon/tree-sitter-plantuml")
     (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
     (proto "https://github.com/mitchellh/tree-sitter-proto")
     (zig "https://github.com/maxxnino/tree-sitter-zig")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(defun custom/treesit-install-all-languages ()
    "Install all languages specified by `treesit-language-source-alist'."
    (interactive)
    (let ((languages (mapcar 'car treesit-language-source-alist)))
      (dolist (lang languages)
              (treesit-install-language-grammar lang)
              (message "`%s' parser was installed." lang)
              (sit-for 0.75))))

(provide 'emacs-config-darwin)
