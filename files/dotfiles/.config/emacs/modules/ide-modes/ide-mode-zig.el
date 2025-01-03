;; Associated zig files with zig-ts-mode
(require 'treesit)

(defgroup zig-ts-mode nil
  "Tree-sitter powered support for Zig code."
  :link '(url-link "https://ziglang.org/")
  :group 'languages)

(defcustom zig-indent-offset 4
  "Indent Zig code by this number of spaces."
  :type 'integer
  :safe #'integerp)

(defun zig-in-str-or-cmnt () (nth 8 (syntax-ppss)))

(defun zig-start-of-current-str-or-comment () (nth 8 (syntax-ppss)))

(defun zig-skip-backwards-past-whitespace-and-comments ()
  (while (or
          ;; If inside a comment, jump to start of comment.
          (let ((start (zig-start-of-current-str-or-comment)))
            (and start
                 (not (zig-currently-in-str))
                 (goto-char start)))
          ;; Skip backwards past whitespace and comment end delimiters.
          (/= 0 (skip-syntax-backward " >")))))

(defconst zig-top-item-beg-re
  (concat "^ *"
          (regexp-opt
           '("pub" "extern" "export" ""))
          "[[:space:]]*"
          (regexp-opt
           '("fn" "test"))
          "[[:space:]]+")
  "Start of a Zig item.")

(defconst zig-electric-indent-chars
  '(?\; ?\, ?\) ?\] ?\}))

(defun zig-paren-nesting-level () (nth 0 (syntax-ppss)))

(defun zig-beginning-of-defun (&optional arg)
  "Move backward to the beginning of the current defun.

With ARG, move backward multiple defuns.  Negative ARG means
move forward.

This is written mainly to be used as `beginning-of-defun-function' for Zig."
  (interactive "p")
  (let* ((arg (or arg 1))
         (magnitude (abs arg))
         (sign (if (< arg 0) -1 1)))
    ;; If moving forward, don't find the defun we might currently be
    ;; on.
    (when (< sign 0)
      (end-of-line))
    (catch 'done
      (dotimes (_ magnitude)
        ;; Search until we find a match that is not in a string or comment.
        (while (if (re-search-backward (concat "^[[:space:]]*\\(" zig-top-item-beg-re "\\)")
                                       nil 'move sign)
                   (zig-in-str-or-cmnt)
                 ;; Did not find it.
                 (throw 'done nil)))))
    t))

(defun zig-ts-mode-indent-line ()
  (interactive)
  ;; First, calculate the column that this line should be indented to.
  (let ((indent-col
         (save-excursion
           (back-to-indentation)
           (let* (;; paren-level: How many sets of parens (or other delimiters)
                  ;;   we're within, except that if this line closes the
                  ;;   innermost set(s) (e.g. the line is just "}"), then we
                  ;;   don't count those set(s).
                  (paren-level
                   (save-excursion
                     (while (looking-at "[]})]") (forward-char))
                     (zig-paren-nesting-level)))
                  ;; prev-block-indent-col: If we're within delimiters, this is
                  ;; the column to which the start of that block is indented
                  ;; (if we're not, this is just zero).
                  (prev-block-indent-col
                   (if (<= paren-level 0) 0
                     (save-excursion
                       (while (>= (zig-paren-nesting-level) paren-level)
                         (backward-up-list)
                         (back-to-indentation))
                       (current-column))))
                  ;; base-indent-col: The column to which a complete expression
                  ;;   on this line should be indented.
                  (base-indent-col
                   (if (<= paren-level 0)
                       prev-block-indent-col
                     (or (save-excursion
                           (backward-up-list)
                           (forward-char)
                           (and (not (looking-at " *\\(//[^\n]*\\)?\n"))
                                (current-column)))
                         (+ prev-block-indent-col zig-indent-offset))))
                  ;; is-expr-continutation: True if this line continues an
                  ;; expression from the previous line, false otherwise.
                  (is-expr-continutation
                   (and
                    (not (looking-at "[]});]\\|else"))
                    (save-excursion
                      (zig-skip-backwards-past-whitespace-and-comments)
                      (when (> (point) 1)
                        (backward-char)
                        (not (looking-at "[,;([{}]")))))))
             ;; Now we can calculate indent-col:
             (if is-expr-continutation
                 (+ base-indent-col zig-indent-offset)
               base-indent-col)))))
    ;; If point is within the indentation whitespace, move it to the end of the
    ;; new indentation whitespace (which is what the indent-line-to function
    ;; always does).  Otherwise, we don't want point to move, so we use a
    ;; save-excursion.
    (if (<= (current-column) (current-indentation))
        (indent-line-to indent-col)
      (save-excursion (indent-line-to indent-col)))))

(defun zig-syntax-propertize-to-newline-if-in-multiline-str (end)
  ;; First, we need to check if we're in a multiline string literal; if we're
  ;; not, do nothing.
  (when (zig-currently-in-str)
    (let ((start (zig-start-of-current-str-or-comment)))
      (when (save-excursion
              (goto-char start)
              (looking-at "\\\\\\\\"))
        ;; At this point, we've determined that we're within a multiline string
        ;; literal.  Let `stop' be the position of the closing newline, or
        ;; `end', whichever comes first.
        (let ((stop (if (save-excursion
                          (goto-char start)
                          (re-search-forward "\n" end t))
                        (prog1 (match-end 0)
                          ;; We found the closing newline, so mark it as the
                          ;; end of this string literal.
                          (put-text-property (match-beginning 0)
                                             (match-end 0)
                                             'syntax-table
                                             (string-to-syntax "|")))
                      end)))
          ;; Zig multiline string literals don't support escapes, so mark all
          ;; backslashes (up to `stop') as punctation instead of escapes.
          (save-excursion
            (goto-char (1+ start))
            (while (re-search-forward "\\\\" stop t)
              (put-text-property (match-beginning 0) (match-end 0)
                                 'syntax-table (string-to-syntax "."))
              (goto-char (match-end 0))))
          ;; Move to the end of the string (or `end'), so that
          ;; zig-syntax-propertize can pick up from there.
          (goto-char stop))))))

(defun zig-syntax-propertize (start end)
  (goto-char start)
  (zig-syntax-propertize-to-newline-if-in-multiline-str end)
  (funcall
   (syntax-propertize-rules
    ;; Multiline strings
    ;; Do not match backslashes that are preceded by single or
    ;; double-quotes.
    ("[^\\'\"]c?\\(\\\\\\)\\\\"
     (1 (prog1 "|"
          (goto-char (match-end 0))
          (zig-syntax-propertize-to-newline-if-in-multiline-str end)))))
   (point) end))

(defvar zig--treesit-font-lock-setting
  (treesit-font-lock-rules
   :feature 'comment
   :language 'zig
   '([
      (container_doc_comment)
      (doc_comment)
      (line_comment)
      ] @font-lock-comment-face)

   :feature 'brackets
   :language 'zig
   '([
      "[" "]" "(" ")" "{" "}"
      (Payload "|")
      (PtrPayload "|")
      (PtrIndexPayload "|")
      ] @font-lock-bracket-face)

   :feature 'misc-punctuation
   :language 'zig
   '([ ".." "..." ] @font-lock-misc-punctuation-face)

   :feature 'delimiters
   :language 'zig
   '([ ";" "." "," ":" ] @font-lock-delimiter-face)

   :feature 'operators
   :language 'zig
   '([
      (CompareOp)
      (BitwiseOp)
      (BitShiftOp)
      (AdditionOp)
      (AssignOp)
      (MultiplyOp)
      (PrefixOp)
      "*"
      "**"
      "=>"
      ".?"
      ".*"
      "?"] @font-lock-operator-face)

   :feature 'builtin-constants
   :language 'zig
   '(["null" "unreachable" "undefined"] @font-lock-builtin-face)

   :feature 'attributes
   :language 'zig
   '([
      "comptime"
      "export"
      "extern"
      "inline"
      "noinline"
      "packed"
      "pub"
      "threadlocal"] @font-lock-keyword-face)

   :feature 'storageclass
   :language 'zig
   '([
      "addrspace"
      "align"
      "callconv"
      "linksection"
      ] @font-lock-type-face)

  :feature 'type-qualifier
  :language 'zig
  '([
     "const"
     "var"
     "volatile"
     "allowzero"
     "noalias"
     ] @font-lock-keyword-face)

  :feature 'builtin-types
  :language 'zig
  '([
     "anytype"
     (BuildinTypeExpr)
     ] @font-lock-type-face)

  :feature 'try-catch
  :language 'zig
  :override t
  '(["try" "catch"] @font-lock-keyword-face)

  :feature 'include
  :language 'zig
  '(["usingnamespace"] @font-lock-keyword-face)

  :feature 'keywords
  :language 'zig
  '(["asm" "defer" "errdefer" "test" "struct" "union" "enum" "opaque" "error"]
    @font-lock-keyword-face)

  :feature 'logic
  :language 'zig
  '(["and" "or" "orelse"] @font-lock-keyword-face)

  :feature 'async
  :language 'zig
  '(["async" "await" "suspend" "nosuspend" "resume"] @font-lock-keyword-face)

  :feature 'fn
  :language 'zig
  '(["fn" "return"] @font-lock-keyword-face)

  :feature 'conditional
  :language 'zig
  '(["if" "else" "switch"] @font-lock-keyword-face)

  :feature 'repeat
  :language 'zig
  '(["for" "while" "break" "continue"] @font-lock-keyword-face)

  :feature 'labels
  :language 'zig
  '(((BreakLabel (IDENTIFIER) @font-lock-keyword-face))
    ((BlockLabel (IDENTIFIER) @font-lock-keyword-face)))

  :feature 'string-special
  :language 'zig
  '(((CHAR_LITERAL) @font-lock-escape-face)
    ((EscapeSequence) @font-lock-string-face)
    ((FormatSequence) @font-lock-string-face))

  :feature 'string
  :language 'zig
  '([
     (LINESTRING)
     (STRINGLITERALSINGLE)
     ] @font-lock-string-face)

  :feature 'boolean
  :language 'zig
  '([ "true" "false" ] @font-lock-builtin-face)

  :feature 'numbers
  :language 'zig
  '(((INTEGER) @font-lock-number-face)
    ((FLOAT) @font-lock-number-face))

  :feature 'builtin-calls
  :language 'zig
  '((BUILTINIDENTIFIER) @font-lock-builtin-face)

  :feature 'enum-constants
  :language 'zig
  '(
    ("." field_constant: (IDENTIFIER) @font-lock-constant-face)
    ((ErrorSetDecl field_constant: (IDENTIFIER) @font-lock-constant-face)))

  :feature 'misc-builtins
  :language 'zig
  '((
     ((IDENTIFIER) @font-lock-builtin-face)
     (:equal @font-lock-builtin-face "_")
     )
    ((FnProto exception: "!" @font-lock-builtin-face))
    ((PtrTypeStart "c" @font-lock-builtin-face)))

  :feature 'function
  :language 'zig
  '(((FnProto function: (IDENTIFIER) @font-lock-function-name-face))
    ((SuffixExpr variable_type_function: (IDENTIFIER) @font-lock-function-call-face (FnCallArguments)))
    ((FieldOrFnCall function_call: (IDENTIFIER) @font-lock-function-call-face)))

  :feature 'variables
  :language 'zig
  '(((SuffixExpr variable_type_function: (IDENTIFIER) @font-lock-variable-use-face))
    ((VarDecl variable_type_function: (IDENTIFIER) @font-lock-variable-name-face))
    ((ParamDecl parameter: (IDENTIFIER) @font-lock-variable-name-face))
    ((FieldOrFnCall field_access: (IDENTIFIER) @font-lock-variable-use-face))
    ((ContainerField field_member: (IDENTIFIER) @font-lock-variable-name-face)))

  :feature 'types
  :override t
  :language 'zig
  '(([
    (VarDecl variable_type_function: (IDENTIFIER) @font-lock-type-face)
    (SuffixExpr variable_type_function: (IDENTIFIER) @font-lock-type-face)
    (ParamDecl parameter: (IDENTIFIER) @font-lock-type-face)
    (FieldOrFnCall field_access: (IDENTIFIER) @font-lock-type-face)
    ]
    (:match "^[A-Z]\\([a-z]+[A-Za-z_0-9]*\\)*$" @font-lock-type-face)))

  :feature 'constants
  :override t
  :language 'zig
  '(([
    (VarDecl variable_type_function: (IDENTIFIER) @font-lock-constant-face)
    (SuffixExpr variable_type_function: (IDENTIFIER) @font-lock-constant-face)
    (FieldOrFnCall field_access: (IDENTIFIER) @font-lock-constant-face)
    ]
    (:match "^[A-Z][A-Z_0-9]+$" @font-lock-constant-face)))

  )
  "Tree-sitter font-lock settings.")

;;;###autoload
(define-derived-mode zig-ts-mode prog-mode "zig-ts"
  "A tree-sitter-powered major mode for the Zig programming language.

\\{zig-ts-mode-map}"
  :group 'zig-ts-mode
  (setq-local comment-start "// ")
  (setq-local comment-start-skip "//+ *")
  (setq-local comment-end "")
  (setq-local electric-indent-chars
              (append zig-electric-indent-chars
                      (and (boundp 'electric-indent-chars)
                           electric-indent-chars)))
  (setq-local beginning-of-defun-function 'zig-beginning-of-defun)
  (setq-local end-of-defun-function 'zig-end-of-defun)
  (setq-local indent-line-function 'zig-ts-mode-indent-line)
  (setq-local indent-tabs-mode nil)  ; Zig forbids tab characters.
  (setq-local syntax-propertize-function 'zig-syntax-propertize)

  (when (treesit-ready-p 'zig)
    (treesit-parser-create 'zig)
    (setq-local treesit-font-lock-feature-list
                '((comment string numbers)
                  (builtin-constants attributes keywords builtin-types type-qualifier function try-catch)
                  (operators repeat conditional fn async logic include storageclass enum-constants builtin-calls boolean types)
                  (brackets delimiters misc-punctuation labels misc-builtins string-special constants variables)))
    (setq-local treesit-font-lock-settings zig--treesit-font-lock-setting)
    (treesit-major-mode-setup)))

(add-to-list 'auto-mode-alist '("\\.\\(zig\\|zon\\)\\'" . zig-ts-mode))


(defun zig//hooks ()
  "Call this when zig-ts-mode is enabled."
  (add-hook 'before-save-hook
            (lambda ()
              (when (eq major-mode 'zig-mode)
                (call-interactively #'eglot-format-buffer)
                (call-interactively #'eglot-code-action-organize-imports)))))


(add-hook 'zig-ts-mode-hook #'eglot-ensure)
(add-hook 'zig-ts-mode-hook 'flycheck-mode)
(add-hook 'zig-ts-mode-hook #'hs-minor-mode)
(add-hook 'zig-ts-mode-hook 'zig//hooks)

(with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '((zig-ts-mode) "zls")))

(provide 'ide-mode-zig)
