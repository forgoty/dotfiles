(define-module (forgoty packages emacs)
  #:use-module (guix packages)
  #:use-module (gnu packages node)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix build-system emacs)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (gnu packages emacs-build)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (guix git-download))

(define-public emacs-evil-iedit-state
  (package
    (name "emacs-evil-iedit-state")
    (version "a44bc05acb49708aba124129d0e941084e8e14b6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/smile13241324/evil-iedit-state.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1zmz8hal50xppv5433qhrzkbv5n0pfm7vbhf1bzmxz63kfh2rv02"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-evil emacs-iedit))
    (home-page "https://github.com/smile13241324/evil-iedit-state")
    (synopsis "Evil states to interface iedit mode")
    (description
     "Adds two new Evil states `iedit and `iedit insert with expand-region integration.")
    (license #f)))

(define-public emacs-evil-textobj-tree-sitter
  (package
    (name "emacs-evil-textobj-tree-sitter")
    (version "20251118.341")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/meain/evil-textobj-tree-sitter.git")
             (commit "d0d088c781b54534b49880819a40575b203dc6c8")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1g6zdpf5djalckn7kdhmvvzv3jlrh8pnf91w15aj0gzx6lgppykv"))))
    (build-system emacs-build-system)
    (arguments
     '(#:include '("^[^/]+.el$" "^[^/]+.el.in$"
                   "^dir$"
                   "^[^/]+.info$"
                   "^[^/]+.texi$"
                   "^[^/]+.texinfo$"
                   "^doc/dir$"
                   "^doc/[^/]+.info$"
                   "^doc/[^/]+.texi$"
                   "^doc/[^/]+.texinfo$"
                   "^queries$"
                   "^treesit-queries$")
       #:tests? #f
       #:exclude '("^.dir-locals.el$" "^test.el$" "^tests.el$"
                   "^[^/]+-test.el$" "^[^/]+-tests.el$")))
    (home-page "https://github.com/meain/evil-textobj-tree-sitter")
    (synopsis "Provides evil textobjects using tree-sitter")
    (description
     "This package is a port of nvim-treesitter/nvim-treesitter-textobjects.  This
package will let you create evil textobjects using the power of tree-sitter
grammars.  You can easily create function,class,comment etc textobjects in
multiple languages.  You can do a sample map like below to create a function
textobj. (define-key evil-outer-text-objects-map \"f\"
(evil-textobj-tree-sitter-get-textobj \"function.outer\"))
`evil-textobj-tree-sitter-get-textobj will return you a function that you can
use in a define-key map.  You can pass in any of the supported queries as an arg
of that function.  You can also pass in multiple queries as a list and we will
match on all of them, ranked on which ones comes up first in the file.  You can
find more info in the README.md file at
https://github.com/meain/evil-textobj-tree-sitter This package also provides
with thing-at-point functions for common textobjects like functions, loops,
conditionals etc.  You need to either have elisp-tree-sitter installed or have
Emacs version >=29 for this package to work.")
    (license #f)))

(define-public emacs-protobuf-ts-mode
  (package
    (name "emacs-protobuf-ts-mode")
    (version "20230728.1747")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/emacsattic/protobuf-ts-mode.git")
             (commit "65152f5341ea4b3417390b3e60b195975161b8bc")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0hrxfnmaxlb9s0rqq8pb8lq0kl4w4vk6s62q55mbbhr0ad4c30p1"))))
    (build-system emacs-build-system)
    (home-page "https://git.ookami.one/cgit/protobuf-ts-mode")
    (synopsis "Tree sitter support for Protocol Buffers (proto3 only)")
    (description
     "Use tree-sitter for font-lock, imenu, indentation, and navigation of protocol
buffers files. (proto3 only) You can use
https://github.com/casouri/tree-sitter-module to build and install tree-sitter
modules.")
    (license #f)))

(define-public emacs-tabspaces
  (package
    (name "emacs-tabspaces")
    (version "20241123.1957")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mclear-tools/tabspaces.git")
             (commit "4fd52c33f4a215360e2b2e1b237115a217ae9bbe")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0k17vaflbqm5n7jcllpaz4idmvmy79njsaq9i4r5py367g85qa7z"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-project))
    (home-page "https://github.com/mclear-tools/tabspaces")
    (synopsis "Leverage tab-bar and project for buffer-isolated workspaces")
    (description
     "This package provides several functions to facilitate a frame-based tab workflow
with one workspace per tab, integration with project.el (for project-based
workspaces) and buffer isolation per tab (i.e.  a \"tabspace\" workspace).  The
package assumes project.el and tab-bar.el are both present (they are built-in to
Emacs 27.1+).  This file is not part of GNU Emacs. ; Acknowledgements Much of
the package code is inspired by: - https://github.com/kaz-yos/emacs -
https://github.com/wamei/elscreen-separate-buffer-list/issues/8 -
https://www.rousette.org.uk/archives/using-the-tab-bar-in-emacs/ -
https://github.com/minad/consult#multiple-sources -
https://github.com/florommel/bufferlo.")
    (license #f)))

(define-public emacs-auctex-latexmk
  (package
    (name "emacs-auctex-latexmk")
    (version "20221025.1219")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/emacsmirror/auctex-latexmk.git")
             (commit "b00a95e6b34c94987fda5a57c20cfe2f064b1c7a")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0bbvb4aw9frg4fc0z9qkc5xd2s9x65k6vdscy5svsy0h17iacsbb"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-auctex))
    (home-page "https://github.com/tom-tan/auctex-latexmk/")
    (synopsis "Add LatexMk support to AUCTeX")
    (description
     "This library adds @code{LatexMk} support to AUC@code{TeX}.  Requirements: *
AUC@code{TeX} * @code{LatexMk} * @code{TeXLive} (2011 or later if you write
@code{TeX} source in Japanese) To use this package, add the following line to
your .emacs file: (require auctex-latexmk) (auctex-latexmk-setup) And add the
following line to your .latexmkrc file: # .latexmkrc starts $pdf_mode = 1; #
.latexmkrc ends After that, by using M-x @code{TeX-command-master} (or C-c C-c),
you can use @code{LatexMk} command to compile @code{TeX} source.  For Japanese
users: @code{LatexMk} command automatically stores the encoding of a source file
and passes it to latexmk via an environment variable named \"LATEXENC\".  Here is
the example of .latexmkrc to use \"LATEXENC\": # .latexmkrc starts $kanji =
\"-kanji=$ENV{\\\"LATEXENC\\\"}\" if defined $ENV{\"LATEXENC\"}; $latex = \"platex
$kanji\"; $bibtex = \"pbibtex $kanji\"; $dvipdf = dvipdfmx -o %D %S'; $pdf_mode =
3; # .latexmkrc ends.")
    (license #f)))

(define-public emacs-flymake-golangci
  (package
    (name "emacs-flymake-golangci")
    (version "2cf8f3a55c64b52d6eab4aa13cb95b37442d33d5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/storvik/flymake-golangci")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1xi7v2kxdxvvchjdigbhh5wkh7a7ij3qr4q7jq2zxsglbsa06wl2"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-auctex))
    (home-page "https://github.com/storvik/flymake-golangci/")
    (synopsis "Flymake backend for golangci linter.")
    (description "Flymake backend for golangci linter.")
    (license #f)))

(define-public emacs-editor-code-assistant
  (let ((commit "7c55b3d45f43bc68f3347d1a5606b3d9e01b3328")
        (revision "0"))
    (package
      (name "emacs-editor-code-assistant")
      (version (git-version "0.10.0" revision commit))
      (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/editor-code-assistant/eca-emacs")
              (commit commit)))
        (file-name (git-file-name name version))
        (sha256
          (base32 "0ssb86h65risdz9sfx9jlqsi4g3sba4drr4bml5gg4rxgycyhk65"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/editor-code-assistant/eca-emacs")
      (propagated-inputs (list emacs-compat emacs-dash emacs-f
                              emacs-markdown-mode))
      (synopsis "Editor Code Assistant for Emacs")
      (description "Editor Code Assistant (ECA) integration for Emacs.")
      (license license:asl2.0))))
