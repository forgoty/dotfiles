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

(define-public emacs-embark-consult
  (package
    (name "emacs-embark-consult")
    (version "1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://elpa.gnu.org/packages/embark-consult-"
                           version ".tar"))
       (sha256
        (base32 "06yh6w4zgvvkfllmcr0szsgjrfhh9rpjwgmcrf6h2gai2ps9xdqr"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-compat emacs-embark emacs-consult))
    (home-page "https://github.com/oantolin/embark")
    (synopsis "Consult integration for Embark")
    (description
     "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ EMBARK: EMACS MINI-BUFFER
ACTIONS ROOTED IN KEYMAPS ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ 1
Overview ══════════ Embark makes it easy to choose a command to run based on
what is near point, both during a minibuffer completion session (in a way
familiar to Helm or Counsel users) and in normal buffers.  Bind the command
`embark-act to a key and it acts like prefix-key for a keymap of /actions/
(commands) relevant to the /target/ around point.  With point on an URL in a
buffer you can open the URL in a browser or eww or download the file it points
to.  If while switching buffers you spot an old one, you can kill it right there
and continue to select another.  Embark comes preconfigured with over a hundred
actions for common types of targets such as files, buffers, identifiers,
s-expressions, sentences; and it is easy to add more actions and more target
types.  Embark can also collect all the candidates in a minibuffer to an
occur-like buffer or export them to a buffer in a major-mode specific to the
type of candidates, such as dired for a set of files, ibuffer for a set of
buffers, or customize for a set of variables.  1.1 Acting on targets
───────────────────── You can think of `embark-act as a keyboard-based version
of a right-click contextual menu.  The `embark-act command (which you should
bind to a convenient key), acts as a prefix for a keymap offering you relevant
/actions/ to use on a /target/ determined by the context: • In the minibuffer,
the target is the current top completion candidate. • In the `*Completions*
buffer the target is the completion at point. • In a regular buffer, the target
is the region if active, or else the file, symbol, URL, s-expression or defun at
point.  Multiple targets can be present at the same location and you can cycle
between them by repeating the `embark-act key binding.  The type of actions
offered depend on the type of the target.  Here is a sample of a few of the
actions offered in the default configuration: • For files you get offered
actions like deleting, copying, renaming, visiting in another window, running a
shell command on the file, etc. • For buffers the actions include switching to
or killing the buffer. • For package names the actions include installing,
removing or visiting the homepage. • For Emacs Lisp symbols the actions include
finding the definition, looking up documentation, evaluating (which for a
variable immediately shows the value, but for a function lets you pass it some
arguments first).  There are some actions specific to variables, such as setting
the value directly or though the customize system, and some actions specific to
commands, such as binding it to a key.  By default when you use `embark-act if
you don't immediately select an action, after a short delay Embark will pop up a
buffer showing a list of actions and their corresponding key bindings.  If you
are using `embark-act outside the minibuffer, Embark will also highlight the
current target.  These behaviors are configurable via the variable
`embark-indicators'.  Instead of selecting an action via its key binding, you
can select it by name with completion by typing `C-h after `embark-act'.
Everything is easily configurable: determining the current target, classifying
it, and deciding which actions are offered for each type in the classification.
The above introduction just mentions part of the default configuration.
Configuring which actions are offered for a type is particularly easy and
requires no programming: the variable `embark-keymap-alist associates target
types with variables containing keymaps, and those keymaps containing bindings
for the actions. (To examine the available categories and their associated
keymaps, you can use `C-h v embark-keymap-alist or customize that variable.) For
example, in the default configuration the type `file is associated with the
symbol `embark-file-map'.  That symbol names a keymap with single-letter key
bindings for common Emacs file commands, for instance `c is bound to
`copy-file'.  This means that if you are in the minibuffer after running a
command that prompts for a file, such as `find-file or `rename-file', you can
copy a file by running `embark-act and then pressing `c'.  These action keymaps
are very convenient but not strictly necessary when using `embark-act': you can
use any command that reads from the minibuffer as an action and the target of
the action will be inserted at the first minibuffer prompt.  After running
`embark-act all of your key bindings and even `execute-extended-command can be
used to run a command.  For example, if you want to replace all occurrences of
the symbol at point, just use `M-% as the action, there is no need to bind
`query-replace in one of Embark's keymaps.  Also, those action keymaps are
normal Emacs keymaps and you should feel free to bind in them whatever commands
you find useful as actions and want to be available through convenient bindings.
 The actions in `embark-general-map are available no matter what type of
completion you are in the middle of.  By default this includes bindings to save
the current candidate in the kill ring and to insert the current candidate in
the previously selected buffer (the buffer that was current when you executed a
command that opened up the minibuffer).  Emacs's minibuffer completion system
includes metadata indicating the /category/ of what is being completed.  For
example, `find-file''s metadata indicates a category of `file and
`switch-to-buffer''s metadata indicates a category of `buffer'.  Embark has the
related notion of the /type/ of a target for actions, and by default when
category metadata is present it is taken to be the type of minibuffer completion
candidates when used as targets.  Emacs commands often do not set useful
category metadata so the [Marginalia] package, which supplies this missing
metadata, is highly recommended for use with Embark.  Embark's default
configuration has actions for the following target types: files, buffers,
symbols, packages, URLs, bookmarks, and as a somewhat special case, actions for
when the region is active.  You can read about the [default actions and their
key bindings] on the @code{GitHub} project wiki. [Marginalia]
<https://github.com/minad/marginalia> [default actions and their key bindings]
<https://github.com/oantolin/embark/wiki/Default-Actions> 1.2 The default action
on a target ────────────────────────────────── Embark has a notion of default
action for a target: • If the target is a minibuffer completion candidate, then
the default action is whatever command opened the minibuffer in the first place.
 For example if you run `kill-buffer', then the default action will be to kill
buffers. • If the target comes from a regular buffer (i.e., not a minibuffer),
then the default action is whatever is bound to `RET in the keymap of actions
for that type of target.  For example, in Embark's default configuration for a
URL found at point the default action is `browse-url', because `RET is bound to
`browse-url in the `embark-url-map keymap.  To run the default action you can
press `RET after running `embark-act'.  Note that if there are several different
targets at a given location, each has its own default action, so first cycle to
the target you want and then press `RET to run the corresponding default action.
 There is also `embark-dwim which runs the default action for the first target
found.  It's pretty handy in non-minibuffer buffers: with Embark's default
configuration it will: • Open the file at point. • Open the URL at point in a
web browser (using the `browse-url command). • Compose a new email to the email
address at point. • In an Emacs Lisp buffer, if point is on an opening
parenthesis or right after a closing one, it will evaluate the corresponding
expression. • Go to the definition of an Emacs Lisp function, variable or macro
at point. • Find the file corresponding to an Emacs Lisp library at point.  1.3
Working with sets of possible targets ─────────────────────────────────────────
Besides acting individually on targets, Embark lets you work collectively on a
set of target /candidates/.  For example, while you are in the minibuffer the
candidates are simply the possible completions of your input.  Embark provides
three main commands to work on candidate sets: • The `embark-act-all command
runs the same action on each of the current candidates.  It is just like using
`embark-act on each candidate in turn. (Because you can easily act on many more
candidates than you meant to, by default Embark asks you to confirm uses of
`embark-act-all'; you can turn this off by setting the user option
`embark-confirm-act-all to `nil'.) • The `embark-collect command produces a
buffer listing all the current candidates, for you to peruse and run actions on
at your leisure.  The candidates are displayed as a list showing additional
annotations.  If any of the candidates contain newlines, then horizontal lines
are used to separate candidates.  The Embark Collect buffer is somewhat
\"dired-like\": you can select and deselect candidates through `embark-select
(available as an action in `embark-act', bound to `SPC'; but you could also give
it a global key binding).  In an Embark Collect buffer `embark-act is bound to
`a and `embark-act-all is bound to `A'; `embark-act-all will act on all
currently marked candidates if there any, and will act on all candidates if none
are marked.  In particular, this means that `a SPC will toggle whether the
candidate at point is selected, and `A SPC will select all candidates if none
are selected, or deselect all selected candidates if there are some. • The
`embark-export command tries to open a buffer in an appropriate major mode for
the set of candidates.  If the candidates are files export produces a Dired
buffer; if they are buffers, you get an Ibuffer buffer; and if they are packages
you get a buffer in package menu mode.  If you use the grepping commands from
the [Consult] package, `consult-grep', `consult-git-grep or `consult-ripgrep',
then you should install the `embark-consult package, which adds support for
exporting a list of grep results to an honest grep-mode buffer, on which you can
even use [wgrep] if you wish.  When in doubt choosing between exporting and
collecting, a good rule of thumb is to always prefer `embark-export since when
an exporter to a special major mode is available for a given type of target, it
will be more featureful than an Embark collect buffer, and if no such exporter
is configured the `embark-export command falls back to the generic
`embark-collect'.  These commands are always available as \"actions\" (although
they do not act on just the current target but on all candidates) for
`embark-act and are bound to `A', `S (for \"snapshot\"), and `E', respectively, in
`embark-general-map'.  This means that you do not have to bind your own key
bindings for these (although you can, of course!), just a key binding for
`embark-act'.  In Embark Collect or Embark Export buffers that were obtained by
running `embark-collect or `embark-export from within a minibuffer completion
session, `g is bound to a command that restarts the completion session, that is,
the command that opened the minibuffer is run again and the minibuffer contents
restored.  You can then interact normally with the command, perhaps editing the
minibuffer contents, and, if you wish, you can rerun `embark-collect or
`embark-export to get an updated buffer. [Consult]
<https://github.com/minad/consult/> [wgrep]
<https://github.com/mhayashi1120/Emacs-wgrep> 1.3.1 Selecting some targets to
make an ad hoc candidate set
╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌ The commands for
working with sets of candidates just described, namely `embark-act-all',
`embark-export and `embark-collect by default work with all candidates defined
in the current context.  For example, in the minibuffer they operate on all
currently completion candidates, or in a dired buffer they work on all marked
files (or all files if none are marked).  Embark also has a notion of
/selection/, where you can accumulate an ad hoc list of targets for these
commands to work on.  The selection is controlled by using the `embark-select
action, bound to `SPC in `embark-general-map so that it is always available (you
can also give `embark-select a global key binding if you wish; when called
directly, not as an action for `embark-act', it will select the first target at
point).  Calling this action on a target toggles its membership in the current
buffer's Embark selection; that is, it adds it to selection if not selected and
removes it from the selection if it was selected.  Whenever the selection for a
buffer is non-empty, the commands `embark-act-all', `embark-export and
`embark-collect will act on the selection.  To deselect all selected targets,
you can use the `embark-select action through `embark-act-all', since this will
run `embark-select on each member of the current selection.  Similarly if no
targets are selected and you are in a minibuffer completion session, running
`embark-select from `embark-act-all will select all the current completion
candidates.  By default, whenever some targets are selected in the current
buffer, a count of selected targets appears in the mode line.  This can be
turned off or customized through the `embark-selection-indicator user option.
The selection functionality is supported in every buffer: • In the minibuffer
this gives a convenient way to act on several completion candidates that don't
follow any simple pattern: just go through the completions selecting the ones
you want, then use `embark-act-all'.  For example, you could attach several
files at once to an email. • For Embark Collect buffers this functionality
enables a dired-like workflow, in which you mark various candidates and apply an
action to all at once. (It supersedes a previous ad hoc dired-like interface
that was implemented only in Embark Collect buffers, with a slightly different
interface.) • In a eww buffer you could use this to select various links you
wish to follow up on, and then collect them into a buffer.  Similarly, while
reading Emacs's info manual you could select some symbols you want to read more
about and export them to an `apropos-mode buffer. • You can use selections in
regular text or programming buffers to do complex editing operations.  For
example, if you have three paragraphs scattered over a file and you want to
bring them together, you can select each one, insert them all somewhere and
finally delete all of them (from their original locations).  1.3.2 `embark-live
a live-updating variant of `embark-collect
╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌ Finally, there
is also an `embark-live variant of the `embark-collect command which
automatically updates the collection after each change in the source buffer.
Users of a completion UI that automatically updates and displays the candidate
list (such as Vertico, Icomplete, Fido-mode, or MCT) will probably not want to
use `embark-live from the minibuffer as they will then have two live updating
displays of the completion candidates! A more likely use of `embark-live is to
be called from a regular buffer to display a sort of live updating \"table of
contents\" for the buffer.  This depends on having appropriate candidate
collectors configured in `embark-candidate-collectors'.  There are not many in
Embark's default configuration, but you can try this experiment: open a dired
buffer in a directory that has very many files, mark a few, and run
`embark-live'.  You'll get an Embark Collect buffer containing only the marked
files, which updates as you mark or unmark files in dired.  To make `embark-live
genuinely useful other candidate collectors are required.  The `embark-consult
package (documented near the end of this manual) contains a few: one for imenu
items and one for outline headings as used by `outline-minor-mode'.  Those
collectors really do give `embark-live a table-of-contents feel.  1.4 Switching
to a different command without losing what you've typed
───────────────────────────────────────────────────────────────────── Embark
also has the `embark-become command which is useful for when you run a command,
start typing at the minibuffer and realize you meant a different command.  The
most common case for me is that I run `switch-to-buffer', start typing a buffer
name and realize I haven't opened the file I had in mind yet! I'll use this
situation as a running example to illustrate `embark-become'.  When this happens
I can, of course, press `C-g and then run `find-file and open the file, but this
requires retyping the portion of the file name you already typed.  This process
can be streamlined with `embark-become': while still in the `switch-to-buffer
you can run `embark-become and effectively make the `switch-to-buffer command
become `find-file for this run.  You can bind `embark-become to a key in
`minibuffer-local-map', but it is also available as an action under the letter
`B (uppercase), so you don't need a binding if you already have one for
`embark-act'.  So, assuming I have `embark-act bound to, say, `C-.', once I
realize I haven't open the file I can type `C-.  B C-x C-f to have
`switch-to-buffer become `find-file without losing what I have already typed in
the minibuffer.  But for even more convenience, `embark-become offers shorter
key bindings for commands you are likely to want the current command to become.
When you use `embark-become it looks for the current command in all keymaps
named in the list `embark-become-keymaps and then activates all keymaps that
contain it.  For example, the default value of `embark-become-keymaps contains a
keymap `embark-become-file+buffer-map with bindings for several commands related
to files and buffers, in particular, it binds `switch-to-buffer to `b and
`find-file to `f'.  So when I accidentally try to switch to a buffer for a file
I haven't opened yet, `embark-become finds that the command I ran,
`switch-to-buffer', is in the keymap `embark-become-file+buffer-map', so it
activates that keymap (and any others that also contain a binding for
`switch-to-buffer').  The end result is that I can type `C-.  B f to switch to
`find-file'.  2 Quick start ═════════════ The easiest way to install Embark is
from GNU ELPA, just run `M-x package-install RET embark RET'. (It is also
available on MELPA.) It is highly recommended to also install [Marginalia] (also
available on GNU ELPA), so that Embark can offer you preconfigured actions in
more contexts.  For `use-package users, the following is a very reasonable
starting configuration: ┌──── │ (use-package marginalia │ :ensure t │ :config │
(marginalia-mode)) │ │ (use-package embark │ :ensure t │ │ :bind │ ((\"C-.\" .
embark-act) ;; pick some comfortable binding │ (\"C-;\" .  embark-dwim) ;; good
alternative: M-. │ (\"C-h B\" .  embark-bindings)) ;; alternative for
`describe-bindings │ │ :init │ │ ;; Optionally replace the key help with a
completing-read interface │ (setq prefix-help-command
#'embark-prefix-help-command) │ │ ;; Show the Embark target at point via Eldoc.
You may adjust the │ ;; Eldoc strategy, if you want to see the documentation
from │ ;; multiple providers.  Beware that using this can be a little │ ;;
jarring since the message shown in the minibuffer can be more │ ;; than one
line, causing the modeline to move up and down: │ │ ;; (add-hook
eldoc-documentation-functions #'embark-eldoc-first-target) │ ;; (setq
eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly) │ │ :config
│ │ ;; Hide the mode line of the Embark live/completions buffers │ (add-to-list
display-buffer-alist │ 	 (\"\\\\`\\\\*Embark Collect \\\\(Live\\\\|Completions\\\\)\\\\*\" │
		 nil │ 		 (window-parameters (mode-line-format .  none))))) │ │ ;; Consult
users will also want the embark-consult package. │ (use-package embark-consult │
:ensure t ; only need to install it, embark loads it after consult if found │
:hook │ (embark-collect-mode .  consult-preview-at-point-mode)) └──── About the
suggested key bindings for `embark-act and `embark-dwim': • Those key bindings
are unlikely to work in the terminal, but terminal users are probably well aware
of this and will know to select different bindings. • The suggested `C-.
binding is used by default in (at least some installations of) GNOME to input
emojis, and Emacs doesn't even get a chance to respond to the binding.  You can
select a different key binding for `embark-act or use `ibus-setup to change the
shortcut for emoji insertion (Emacs 29 will likely use `C-x 8 e e', in case you
want to set the same one system-wide). • The suggested alternative of `M-.  for
`embark-dwim is bound by default to `xref-find-definitions'.  That is a very
useful command but overwriting it with `embark-dwim is sensible since in
Embark's default configuration, `embark-dwim will also find the definition of
the identifier at point. (Note that `xref-find-definitions with a prefix
argument prompts you for an identifier, `embark-dwim does not cover this case).
Other Embark commands such as `embark-act-all', `embark-become',
`embark-collect', and `embark-export can be run through `embark-act as actions
bound to `A', `B', `S (for \"snapshot\"), and `E respectively, and thus don't
really need a dedicated key binding, but feel free to bind them directly if you
so wish.  If you do choose to bind them directly, you'll probably want to bind
them in `minibuffer-local-map', since they are most useful in the minibuffer (in
fact, `embark-become only works in the minibuffer).  The command `embark-dwim
executes the default action at point.  Another good keybinding for `embark-dwim
is `M-.  since `embark-dwim acts like `xref-find-definitions on the symbol at
point. `C-.  can be seen as a right-click context menu at point and `M-.  acts
like left-click.  The keybindings are mnemonic, both act at the point (`.').
Embark needs to know what your minibuffer completion system considers to be the
list of candidates and which one is the current candidate.  Embark works out of
the box if you use Emacs's default tab completion, the built-in `icomplete-mode
or `fido-mode', or the third-party packages [Vertico] or [Ivy].  If you are a
[Helm] or [Ivy] user you are unlikely to want Embark since those packages
include comprehensive functionality for acting on minibuffer completion
candidates. (Embark does come with Ivy integration despite this.) [Marginalia]
<https://github.com/minad/marginalia> [Vertico]
<https://github.com/minad/vertico> [Ivy] <https://github.com/abo-abo/swiper>
[Helm] <https://emacs-helm.github.io/helm/> 3 Advanced configuration
════════════════════════ 3.1 Showing information about available targets and
actions ─────────────────────────────────────────────────────────── By default,
if you run `embark-act and do not immediately select an action, after a short
delay Embark will pop up a buffer called `*Embark Actions* containing a list of
available actions with their key bindings.  You can scroll that buffer with the
mouse of with the usual commands `scroll-other-window and
`scroll-other-window-down (bound by default to `C-M-v and `C-M-S-v').  That
functionality is provided by the `embark-mixed-indicator', but Embark has other
indicators that can provide information about the target and its type, what
other targets you can cycle to, and which actions have key bindings in the
action map for the current type of target.  Any number of indicators can be
active at once and the user option `embark-indicators should be set to a list of
the desired indicators.  Embark comes with the following indicators: •
`embark-minimal-indicator': shows a messages in the echo area or minibuffer
prompt showing the current target and the types of all targets starting with the
current one. • `embark-highlight-indicator': highlights the target at point; on
by default. • `embark-verbose-indicator': displays a table of actions and their
key bindings in a buffer; this is not on by default, in favor of the mixed
indicator described next. • `embark-mixed-indicator': starts out by behaving as
the minimal indicator but after a short delay acts as the verbose indicator;
this is on by default. • `embark-isearch-highlight-indicator': this only does
something when the current target is the symbol at point, in which case it
lazily highlights all occurrences of that symbol in the current buffer, like
isearch; also on by default.  Users of the popular [which-key] package may
prefer to use the `embark-which-key-indicator from the [Embark wiki].  Just copy
its definition from the wiki into your configuration and customize the
`embark-indicators user option to exclude the mixed and verbose indicators and
to include `embark-which-key-indicator'.  If you use [Vertico], there is an even
easier way to get a `which-key'-like display that also lets you use completion
to narrow down the list of alternatives, described at the end of the next
section. [which-key] <https://github.com/justbur/emacs-which-key> [Embark wiki]
<https://github.com/oantolin/embark/wiki/Additional-Configuration#use-which-key-like-a-key-menu-prompt>
[Vertico] <https://github.com/minad/vertico> 3.2 Selecting commands via
completions instead of key bindings
────────────────────────────────────────────────────────────── As an alternative
to reading the list of actions in the verbose or mixed indicators (see the
previous section for a description of these), you can press the
`embark-help-key', which is `C-h by default (but you may prefer `? to free up
`C-h for use as a prefix) after running `embark-act'.  Pressing the help key
will prompt you for the name of an action with completion (but feel free to
enter a command that is not among the offered candidates!), and will also remind
you of the key bindings.  You can press `embark-keymap-prompter-key', which is
`@@ by default, at the prompt and then one of the key bindings to enter the name
of the corresponding action.  You may think that with the `*Embark Actions*
buffer popping up to remind you of the key bindings you'd never want to use
completion to select an action by name, but personally I find that typing a
small portion of the action name to narrow down the list of candidates feels
significantly faster than visually scanning the entire list of actions.  If you
find you prefer selecting actions that way, you can configure embark to always
prompt you for actions by setting the variable `embark-prompter to
`embark-completing-read-prompter'.  On the other hand, you may wish to continue
using key bindings for the actions you perform most often, and to use completion
only to explore what further actions are available or when you've forgotten a
key binding.  In that case, you may prefer to use the minimal indicator, which
does not pop-up an `*Embark Actions* buffer at all, and to use the
`embark-help-key whenever you need help.  This unobtrusive setup is achieved
with the following configuration: ┌──── │ (setq embark-indicators │
(embark-minimal-indicator ; default is embark-mixed-indicator │
	embark-highlight-indicator │ 	embark-isearch-highlight-indicator)) └────
[Vertico] users may wish to configure a grid display for the actions and
key-bindings, reminiscent of the popular package [which-key], but, of course,
enhanced by the use of completion to narrow the list of commands.  In order to
get the grid display, put the following in your Vertico configuration: ┌──── │
(add-to-list vertico-multiform-categories (embark-keybinding grid)) │
(vertico-multiform-mode) └──── This will make the available keys be shown in a
compact grid like in `which-key'.  The `vertico-multiform-mode also enables keys
such as `M-V', `M-G', `M-B', and `M-U for manually switching between layouts in
Vertico buffers. [Vertico] <https://github.com/minad/vertico> [which-key]
<https://github.com/justbur/emacs-which-key> 3.2.1 Selecting commands via
completion outside of Embark
╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌ If you like this
completion interface for exploring key bindings for Embark actions, you may want
to use it elsewhere in Emacs.  You can use Embark's completion-based command
prompter to list: • key bindings under a prefix, • local key bindings, or • all
key bindings.  To use it for key bindings under a prefix (you can use this to
replace the `which-key package, for example), use this configuration: ┌──── │
(setq prefix-help-command #'embark-prefix-help-command) └──── Now, when you have
started on a prefix sequence such as `C-x or `C-c', pressing `C-h will bring up
the Embark version of the built-in `prefix-help-command', which will list the
keys under that prefix and their bindings, and lets you select the one you
wanted with completion, or by key binding if you press
`embark-keymap-prompter-key'.  To list local or global key bindings, use the
command `embark-bindings'.  You can bind that to `C-h b', which is the default
key binding for the built-in `describe-bindings command, which this command can
replace.  By default, `embark-bindings lists local key bindings, typically those
bound in the major mode keymap; to get global bindings as well, call it with a
`C-u prefix argument.  3.3 Quitting the minibuffer after an action
─────────────────────────────────────────── By default, if you call `embark-act
from the minibuffer it quits the minibuffer after performing the action.  You
can change this by setting the user option `embark-quit-after-action to `nil'.
Having `embark-act /not/ quit the minibuffer can be useful to turn commands into
little \"thing managers\".  For example, you can use `find-file as a little file
manager or `describe-package as a little package manager: you can run those
commands, perform a series of actions, and then quit the command.  If you want
to control the quitting behavior in a fine-grained manner depending on the
action, you can set `embark-quit-after-action to an alist, associating commands
to either `t for quitting or `nil for not quitting.  When using an alist, you
can use the special key `t to specify the default behavior.  For example, to
specify that by default actions should not quit the minibuffer but that using
`kill-buffer as an action should quit, you can use the following configuration:
┌──── │ (setq embark-quit-after-action ((kill-buffer .  t) (t .  nil))) └────
The variable `embark-quit-after-action only specifies a default, that is, it
only controls whether or not `embark-act quits the minibuffer when you call it
without a prefix argument, and you can select the opposite behavior to what the
variable says by calling `embark-act with `C-u'.  Also note that both the
variable `embark-quit-after-action and `C-u have no effect when you call
`embark-act outside the minibuffer.  If you find yourself using the quitting and
non-quitting variants of `embark-act about equally often, independently of the
action, you may prefer to simply have separate commands for them instead of a
single command that you call with `C-u half the time.  You could, for example,
keep the default exiting behavior of `embark-act and define a non-quitting
version as follows: ┌──── │ (defun embark-act-noquit () │ \"Run action but don't
quit the minibuffer afterwards.\" │ (interactive) │ (let
((embark-quit-after-action nil)) │ (embark-act))) └──── 3.4 Running some setup
after injecting the target ───────────────────────────────────────────────── You
can customize what happens after the target is inserted at the minibuffer prompt
of an action.  There are `embark-target-injection-hooks', that are run by
default after injecting the target into the minibuffer.  The variable
`embark-target-injection-hooks is an alist associating commands to their setup
hooks.  There are two special keys: if no setup hook is specified for a given
action, the hook associated to `t is run; and the hook associated to `:always is
run regardless of the action. (This variable used to have the less explicit name
of `embark-setup-action-hooks', so please update your configuration.) For
example, consider using `shell-command as an action during file completion.  It
would be useful to insert a space before the target file name and to leave the
point at the beginning, so you can immediately type the shell command to run on
that file.  That's why in Embark's default configuration there is an entry in
`embark-target-injection-hooks associating `shell-command to a hook that
includes `embark--shell-prep', a simple helper function that quotes all the
spaces in the file name, inserts an extra space at the beginning of the line and
leaves point to the left of it.  Now, the preparation that `embark--shell-prep
does would be useless if Embark did what it normally does after it inserts the
target of the action at the minibuffer prompt, which is to \"press `RET'\" for
you, accepting the target as is; if Embark did that for `shell-command you
wouldn't get a chance to type in the command to execute! That is why in Embark's
default configuration the entry for `shell-command in
`embark-target-injection-hooks also contains the function `embark--allow-edit'.
Embark used to have a dedicated variable `embark-allow-edit-actions to which you
could add commands for which Embark should forgo pressing `RET for you after
inserting the target.  Since its effect can also be achieved via the general
`embark-target-injection-hooks mechanism, that variable has been removed to
simplify Embark.  Be sure to update your configuration; if you had something
like: ┌──── │ (add-to-list embark-allow-edit-actions my-command) └──── you
should replace it with: ┌──── │ (push embark--allow-edit │ (alist-get my-command
embark-target-injection-hooks)) └──── Also note that while you could abuse
`embark--allow-edit so that you have to confirm \"dangerous\" actions such as
`delete-file', it is better to implement confirmation by adding the
`embark--confirm function to the appropriate entry of a different hook alist,
namely, `embark-pre-action-hooks'.  Besides `embark--allow-edit', Embark comes
with another function that is of general utility in action setup hooks:
`embark--ignore-target'.  Use it for commands that do prompt you in the
minibuffer but for which inserting the target would be inappropriate.  This is
not a common situation but does occasionally arise.  For example it is used by
default for `shell-command-on-region': that command is used as an action for
region targets, and it prompts you for a shell command; you typically do /not/
want the target, that is the contents of the region, to be entered at that
prompt! 3.5 Running hooks before, after or around an action
─────────────────────────────────────────────────── Embark has three variables,
`embark-pre-action-hooks', `embark-post-action-hooks and
`embark-around-action-hooks', which are alists associating commands to hooks
that should run before or after or as around advice for the command when used as
an action.  As with `embark-target-injection-hooks', there are two special keys
for the alists: `t designates the default hook to run when no specific hook is
specified for a command; and the hook associated to `:always runs regardless.
The default values of those variables are fairly extensive, adding creature
comforts to make running actions a smooth experience.  Embark comes with several
functions intended to be added to these hooks, and used in the default values of
`embark-pre-action-hooks', `embark-post-action-hooks and
`embark-around-action-hooks'.  For pre-action hooks: `embark--confirm Prompt the
user for confirmation before executing the action.  This is used be default for
commands deemed \"dangerous\", or, more accurately, hard to undo, such as
`delete-file and `kill-buffer'. `embark--unmark-target Unmark the active region.
 Use this for commands you want to act on the region contents but without the
region being active.  The default configuration uses this function as a
pre-action hook for `occur and `query-replace', for example, so that you can use
them as actions with region targets to search the whole buffer for the text
contained in the region.  Without this pre-action hook using `occur as an action
for a region target would be pointless: it would search for the the region
contents /in the region/, (typically, due to the details of regexps) finding
only one match! `embark--beginning-of-target Move to the beginning of the target
(for targets that report bounds).  This is used by default for backward motion
commands such as `backward-sexp', so that they don't accidentally leave you on
the current target. `embark--end-of-target Move to the end of the target.  This
is used similarly to the previous function, but also for commands that act on
the last s-expression like `eval-last-sexp'.  This allow you to act on an
s-expression from anywhere inside it and still use `eval-last-sexp as an action.
`embark--xref-push-markers Push the current location on the xref marker stack.
Use this for commands that take you somewhere and for which you'd like to be
able to come back to where you were using `xref-pop-marker-stack'.  This is used
by default for `find-library'.  For post-action hooks: `embark--restart Restart
the command currently prompting in the minibuffer, so that the list of
completion candidates is updated.  This is useful as a post action hook for
commands that delete or rename a completion candidate; for example the default
value of `embark-post-action-hooks uses it for `delete-file', `kill-buffer',
`rename-file', `rename-buffer', etc.  For around-action hooks:
`embark--mark-target Save existing mark and point location, mark the target and
run the action.  Most targets at point outside the minibuffer report which
region of the buffer they correspond to (this is the information used by
`embark-highlight-indicator to know what portion of the buffer to highlight);
this function marks that region.  It is useful as an around action hook for
commands that expect a region to be marked, for example, it is used by default
for `indent-region so that it works on s-expression targets, or for `fill-region
so that it works on paragraph targets. `embark--cd Run the action with
`default-directory set to the directory associated to the current target.  The
target should be of type `file', `buffer', `bookmark or `library', and the
associated directory is what you'd expect in each case.
`embark--narrow-to-target Run the action with buffer narrowed to current target.
 Use this as an around hook to localize the effect of actions that don't already
work on just the region.  In the default configuration it is used for
`repunctuate-sentences'. `embark--save-excursion Run the action restoring point
at the end.  The current default configuration doesn't use this but it is
available for users.  3.6 Creating your own keymaps
───────────────────────────── All internal keymaps are defined with the standard
helper macro `defvar-keymap'.  For example a simple version of the file action
keymap could be defined as follows: ┌──── │ (defvar-keymap embark-file-map │
:doc \"Example keymap with a few file actions\" │ :parent embark-general-map │ \"d\"
#'delete-file │ \"r\" #'rename-file │ \"c\" #'copy-file) └──── These action keymaps
are perfectly normal Emacs keymaps.  You may want to inherit from the
`embark-general-map if you want to access the default Embark actions.  Note that
`embark-collect and `embark-export are also made available via
`embark-general-map'.  3.7 Defining actions for new categories of targets
────────────────────────────────────────────────── It is easy to configure
Embark to provide actions for new types of targets, either in the minibuffer or
outside it.  I present below two very detailed examples of how to do this.  At
several points I'll explain more than one way to proceed, typically with the
easiest option first.  I include the alternative options since there will be
similar situations where the easiest option is not available.  3.7.1 New
minibuffer target example - tab-bar tabs
╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌ As an example, take the new
[tab bars] from Emacs 27.  I'll explain how to configure Embark to offer
tab-specific actions when you use the tab-bar-mode commands that mention tabs by
name.  The configuration explained here is now built-in to Embark (and
Marginalia), but it's still a good self-contained example.  In order to setup up
tab actions you would need to: (1) make sure Embark knows those commands deal
with tabs, (2) define a keymap for tab actions and configure Embark so it knows
that's the keymap you want. [tab bars]
<https://www.gnu.org/software/emacs/manual/html_node/emacs/Tab-Bars.html> ◊
3.7.1.1 Telling Embark about commands that prompt for tabs by name For step (1),
it would be great if the `tab-bar-mode commands reported the completion category
`tab when asking you for a tab with completion. (All built-in Emacs commands
that prompt for file names, for example, do have metadata indicating that they
want a `file'.) They do not, unfortunately, and I will describe a couple of ways
to deal with this.  Maybe the easiest thing is to configure [Marginalia] to
enhance those commands.  All of the `tab-bar-*-tab-by-name commands have the
words \"tab by name\" in the minibuffer prompt, so you can use: ┌──── │
(add-to-list marginalia-prompt-categories (\"tab by name\" .  tab)) └──── That's
it! But in case you are ever in a situation where you don't already have
commands that prompt for the targets you want, I'll describe how writing your
own command with appropriate `category metadata looks: ┌──── │ (defun
my-select-tab-by-name (tab) │ (interactive │ (list │ (let ((tab-list (or (mapcar
(lambda (tab) (cdr (assq name tab))) │ 				(tab-bar-tabs)) │ 			(user-error \"No
tabs found\")))) │ (completing-read │ \"Tabs: \" │ (lambda (string predicate
action) │ 	 (if (eq action metadata) │ 	 (metadata (category .  tab)) │
(complete-with-action │ 	 action tab-list string predicate))))))) │
(tab-bar-select-tab-by-name tab)) └──── As you can see, the built-in support for
setting the category meta-datum is not very easy to use or pretty to look at.
To help with this I recommend the `consult--read function from the excellent
[Consult] package.  With that function we can rewrite the command as follows:
┌──── │ (defun my-select-tab-by-name (tab) │ (interactive │ (list │ (let
((tab-list (or (mapcar (lambda (tab) (cdr (assq name tab))) │
				(tab-bar-tabs)) │ 			(user-error \"No tabs found\")))) │ (consult--read
tab-list │ 		 :prompt \"Tabs: \" │ 		 :category tab)))) │
(tab-bar-select-tab-by-name tab)) └──── Much nicer! No matter how you define the
`my-select-tab-by-name command, the first approach with Marginalia and prompt
detection has the following advantages: you get the `tab category for all the
`tab-bar-*-bar-by-name commands at once, also, you enhance built-in commands,
instead of defining new ones. [Marginalia] <https://github.com/minad/marginalia>
[Consult] <https://github.com/minad/consult/> ◊ 3.7.1.2 Defining and configuring
a keymap for tab actions Let's say we want to offer select, rename and close
actions for tabs (in addition to Embark general actions, such as saving the tab
name to the kill-ring, which you get for free).  Then this will do: ┌──── │
(defvar-keymap embark-tab-actions │ :doc \"Keymap for actions for tab-bar tabs
(when mentioned by name).\" │ :parent embark-general-map │ \"s\"
#'tab-bar-select-tab-by-name │ \"r\" #'tab-bar-rename-tab-by-name │ \"k\"
#'tab-bar-close-tab-by-name) │ │ (add-to-list embark-keymap-alist (tab .
embark-tab-actions)) └──── What if after using this for a while you feel closing
the tab without confirmation is dangerous? You have a couple of options: 1.  You
can keep using the `tab-bar-close-tab-by-name command, but have Embark ask you
for confirmation: ┌──── │ (push #'embark--confirm │ (alist-get
tab-bar-close-tab-by-name │ 		 embark-pre-action-hooks)) └──── 2.  You can write
your own command that prompts for confirmation and use that instead of
`tab-bar-close-tab-by-name in the above keymap: ┌──── │ (defun
my-confirm-close-tab-by-name (tab) │ (interactive \"@code{sTab} to close: \") │
(when (y-or-n-p (format \"Close tab %s'? \" tab)) │ (tab-bar-close-tab-by-name
tab))) └──── Notice that this is a command you can also use directly from `M-x
independently of Embark.  Using it from `M-x leaves something to be desired,
though, since you don't get completion for the tab names.  You can fix this if
you wish as described in the previous section.  3.7.2 New target example in
regular buffers - short Wikipedia links
╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌ Say you want
to teach Embark to treat text of the form `wikipedia:Garry_Kasparov in any
regular buffer as a link to Wikipedia, with actions to open the Wikipedia page
in eww or an external browser or to save the URL of the page in the kill-ring.
We can take advantage of the actions that Embark has preconfigured for URLs, so
all we need to do is teach Embark that `wikipedia:Garry_Kasparov stands for the
URL `https://en.wikipedia.org/wiki/Garry_Kasparov'.  You can be as fancy as you
want with the recognized syntax.  Here, to keep the example simple, I'll assume
the link matches the regexp `wikipedia:[[:alnum:]_]+'.  We will write a function
that looks for a match surrounding point, and returns a dotted list of the form
`'(url URL-OF-THE-PAGE START .  END) where `START and `END are the buffer
positions bounding the target, and are used by Embark to highlight it if you
have `embark-highlight-indicator included in the list `embark-indicators'.
(There are a couple of other options for the return value of a target finder:
the bounding positions are optional and a single target finder is allowed to
return multiple targets; see the documentation for `embark-target-finders for
details.) ┌──── │ (defun my-short-wikipedia-link () │ \"Target a link at point of
the form wikipedia:Page_Name.\" │ (save-excursion │ (let* ((start (progn
(skip-chars-backward \"[:alnum:]_:\") (point))) │ 	 (end (progn
(skip-chars-forward \"[:alnum:]_:\") (point))) │ 	 (str
(buffer-substring-no-properties start end))) │ (save-match-data │ 	(when
(string-match \"wikipedia:\\\\([[:alnum:]_]+\\\\)\" str) │ 	 `(url │ 	 ,(format
\"https://en.wikipedia.org/wiki/%s\" │ 		 (match-string 1 str)) │ 	 ,start .
,end)))))) │ │ (add-to-list embark-target-finders my-short-wikipedia-link) └────
4 How does Embark call the actions? ═══════════════════════════════════ Embark
actions are normal Emacs commands, that is, functions with an interactive
specification.  In order to execute an action, Embark calls the command with
`call-interactively', so the command reads user input exactly as if run directly
by the user.  For example the command may open a minibuffer and read a string
(`read-from-minibuffer') or open a completion interface (`completing-read').  If
this happens, Embark takes the target string and inserts it automatically into
the minibuffer, simulating user input this way.  After inserting the string,
Embark exits the minibuffer, submitting the input. (The immediate minibuffer
exit can be disabled for specific actions in order to allow editing the input;
this is done by adding the `embark--allow-edit function to the appropriate entry
of `embark-target-injection-hooks').  Embark inserts the target string at the
first minibuffer opened by the action command, and if the command happens to
prompt the user for input more than once, the user still interacts with the
second and further prompts in the normal fashion.  Note that if a command does
not prompt the user for input in the minibuffer, Embark still allows you to use
it as an action, but of course, never inserts the target anywhere. (There are
plenty of examples in the default configuration of commands that do not prompt
the user bound to keys in the action maps, most of the region actions, for
instance.) This is how Embark manages to reuse normal commands as actions.  The
mechanism allows you to use as Embark actions commands that were not written
with Embark in mind (and indeed almost all actions that are bound by default in
Embark's action keymaps are standard Emacs commands).  It also allows you to
write new custom actions in such a way that they are useful even without Embark.
 Staring from version 28.1, Emacs has a variable `y-or-n-p-use-read-key', which
when set to `t causes `y-or-n-p to use `read-key instead of
`read-from-minibuffer'.  Setting `y-or-n-p-use-read-key to `t is recommended for
Embark users because it keeps Embark from attempting to insert the target at a
`y-or-n-p prompt, which would almost never be sensible.  Also consider this as a
warning to structure your own action commands so that if they use `y-or-n-p',
they do so only after the prompting for the target.  Here is a simple example
illustrating the various ways of reading input from the user mentioned above.
Bind the following commands to the `embark-symbol-map to be used as actions,
then put the point on some symbol and run them with `embark-act': ┌──── │ (defun
example-action-command1 () │ (interactive) │ (message \"The input was `%s'.\"
(read-from-minibuffer \"Input: \"))) │ │ (defun example-action-command2 (arg
input1 input2) │ (interactive \"P\\@code{nsInput} 1: \\@code{nsInput} 2: \") │
(message \"The first input %swas `%s', and the second was `%s'.\" │ 	 (if arg
\"truly \" \"\") │ 	 input1 │ 	 input2)) │ │ (defun example-action-command3 () │
(interactive) │ (message \"Your selection was `%s'.\" │ 	 (completing-read
\"Select: \" (\"E\" \"M\" \"B\" \"A\" \"R\" \"K\")))) │ │ (defun example-action-command4 () │
(interactive) │ (message \"I don't prompt you for input and thus ignore the
target!\")) │ │ (keymap-set embark-symbol-map \"X 1\" #'example-action-command1) │
(keymap-set embark-symbol-map \"X 2\" #'example-action-command2) │ (keymap-set
embark-symbol-map \"X 3\" #'example-action-command3) │ (keymap-set
embark-symbol-map \"X 4\" #'example-action-command4) └──── Also note that if you
are using the key bindings to call actions, you can pass prefix arguments to
actions in the normal way.  For example, you can use `C-u X2 with the above
demonstration actions to make the message printed by `example-action-command2
more emphatic.  This ability to pass prefix arguments to actions is useful for
some actions in the default configuration, such as
`embark-shell-command-on-buffer'.  4.1 Non-interactive functions as actions
──────────────────────────────────────── Alternatively, Embark does support one
other type of action: a non-interactive function of a single argument.  The
target is passed as argument to the function.  For example: ┌──── │ (defun
example-action-function (target) │ (message \"The target was `%s'.\" target)) │ │
(keymap-set embark-symbol-map \"X 4\" #'example-action-function) └──── Note that
normally binding non-interactive functions in a keymap is useless, since when
attempting to run them using the key binding you get an error message similar to
\"Wrong type argument: commandp, example-action-function\".  In general it is more
flexible to write any new Embark actions as commands, that is, as interactive
functions, because that way you can also run them directly, without Embark.  But
there are a couple of reasons to use non-interactive functions as actions: 1.
You may already have the function lying around, and it is convenient to simply
reuse it.  2.  For command actions the targets can only be simple string, with
no text properties.  For certain advanced uses you may want the action to
receive a string /with/ some text properties, or even a non-string target.  5
Embark, Marginalia and Consult ════════════════════════════════ Embark
cooperates well with the [Marginalia] and [Consult] packages.  Neither of those
packages is a dependency of Embark, but both are highly recommended companions
to Embark, for opposite reasons: Marginalia greatly enhances Embark's
usefulness, while Embark can help enhance Consult.  In the remainder of this
section I'll explain what exactly Marginalia does for Embark, and what Embark
can do for Consult. [Marginalia] <https://github.com/minad/marginalia> [Consult]
<https://github.com/minad/consult> 5.1 Marginalia ────────────── Embark comes
with actions for symbols (commands, functions, variables with actions such as
finding the definition, looking up the documentation, evaluating, etc.) in the
`embark-symbol-map keymap, and for packages (actions like install, delete,
browse url, etc.) in the `embark-package-keymap'.  Unfortunately Embark does not
automatically offers you these keymaps when relevant, because many built-in
Emacs commands don't report accurate category metadata.  For example, a command
like `describe-package', which reads a package name from the minibuffer, does
not have metadata indicating this fact.  In an earlier Embark version, there
were functions to supply this missing metadata, but they have been moved to
Marginalia, which augments many Emacs command to report accurate category
metadata.  Simply activating `marginalia-mode allows Embark to offer you the
package and symbol actions when appropriate again.  Candidate annotations in the
Embark collect buffer are also provided by the Marginalia package: • If you
install Marginalia and activate `marginalia-mode', Embark Collect buffers will
use the Marginalia annotations automatically. • If you don't install Marginalia,
you will see only the annotations that come with Emacs (such as key bindings in
`M-x', or the unicode characters in `C-x 8 RET').  5.2 Consult ─────────── The
excellent Consult package provides many commands that use minibuffer completion,
via the `completing-read function; plenty of its commands can be considered
enhanced versions of built-in Emacs commands, and some are completely new
functionality.  One common enhancement provided in all commands for which it
makes sense is preview functionality, for example `consult-buffer will show you
a quick preview of a buffer before you actually switch to it.  If you use both
Consult and Embark you should install the `embark-consult package which provides
integration between the two.  It provides exporters for several Consult commands
and also tweaks the behavior of many Consult commands when used as actions with
`embark-act in subtle ways that you may not even notice, but make for a smoother
experience.  You need only install it to get these benefits: Embark will
automatically load it after Consult if found.  The `embark-consult package
provides the following exporters: • You can use `embark-export from
`consult-line', `consult-outline', or `consult-mark to obtain an `occur-mode
buffer.  As with the built-in `occur command you use that buffer to jump to a
match and after that, you can then use `next-error and `previous-error to
navigate to other matches.  You can also press `e to activate `occur-edit-mode
and edit the matches in place! • You can export from any of the Consult
asynchronous search commands, `consult-grep', `consult-git-grep', or
`consult-ripgrep to get a `grep-mode buffer.  Here too you can use `next-error
and `previous-error to navigate among matches, and, if you install the [wgrep]
package, you can use it to edit the matches in place.  In both cases, pressing
`g will rerun the Consult command you had exported from and re-enter the input
you had typed (which is similar to reverting but a little more flexible).  You
can then proceed to re-export if that's what you want, but you can also edit the
input changing the search terms or simply cancel if you see you are done with
that search.  The `embark-consult also contains some candidates collectors that
allow you to run `embark-live to get a live-updating table of contents for your
buffer: • `embark-consult-outline-candidates produces the outline headings of
the current buffer, using `consult-outline'. • `embark-consult-imenu-candidates
produces the imenu items of the current buffer, using `consult-imenu'. •
`embark-consult-imenu-or-outline-candidates is a simple combination of the two
previous functions: it produces imenu items in buffers deriving from `prog-mode
and otherwise outline headings.  The way to configure `embark-live (or
`embark-collect and `embark-export for that matter) to use one of these function
is to add it at the end of the `embark-candidate-collectors list.  The
`embark-consult package by default adds the last one, which seems to be the most
sensible default.  Besides those exporters and candidate collectors, the
`embark-consult package provides many subtle tweaks and small integrations
between Embark and Consult.  Some examples are: • When used as actions, the
asynchronous search commands will search only the files associated to the
targets: if the targets /are/ files, it searches those files; for buffers it
will search either the associated file if there is one, else all files in the
buffer's `default-directory'; for bookmarks it will search the file they point
to, same for Emacs Lisp libraries.  This is particularly powerful when using
`embark-act-all to act on multiple files at once, for example you can use
`consult-find to search among file /names/ and then `embark-act-all and
`consult-grep to search within the matching files. • For all other target types,
those that do not have a sensible notion of associated file, a Consult search
command (asynchronous or not) will search for the text of the target but leave
the minibuffer open so you can interact with the Consult command. •
`consult-imenu will search for the target and take you directly to the location
if it matches a unique imenu entry, otherwise it will leave the minibuffer open
so you can navigate among the matches. [wgrep]
<http://github.com/mhayashi1120/Emacs-wgrep/raw/master/wgrep.el > 6 Related
Packages ══════════════════ There are several packages that offer functionality
similar to Embark's.  Acting on minibuffer completion candidates The popular Ivy
and Helm packages have support for acting on the completion candidates of
commands written using their APIs, and there is an extensive ecosystem of
packages meant for Helm and for Ivy (the Ivy ones usually have \"counsel\" in the
name) providing commands and appropriate actions.  Acting on things at point The
built-in `context-menu-mode provides a mouse-driven context-sensitive
configurable menu.  The `do-at-point package by Philip Kaludercic (available on
GNU ELPA), on the other hand is keyboard-driven.  Collecting completion
candidates into a buffer The Ivy package has the command `ivy-occur which is
similar to `embark-collect'.  As with Ivy actions, `ivy-occur only works for
commands written using the Ivy API. 7 Resources ═══════════ If you want to learn
more about how others have used Embark here are some links to read: • [Fifteen
ways to use Embark], a blog post by Karthik Chikmagalur. • [Protesilaos
Stavrou's dotemacs], look for the section called \"Extended minibuffer actions
and more (embark.el and prot-embark.el)\" And some videos to watch: • [Embark and
my extras] by Protesilaos Stavrou. • [Embark – Key features and tweaks] by Raoul
Comninos on the Emacs-Elements @code{YouTube} channel. • [Livestreamed: Adding
an Embark context action to send a stream message] by Sacha Chua. • [System
Crafters Live! - The Many Uses of Embark] by David Wilson. • [Using Emacs
Episode 80 - Vertico, Marginalia, Consult and Embark] by Mike Zamansky. [Fifteen
ways to use Embark] <https://karthinks.com/software/fifteen-ways-to-use-embark/>
[Protesilaos Stavrou's dotemacs] <https://protesilaos.com/dotemacs/> [Embark and
my extras] <https://protesilaos.com/codelog/2021-01-09-emacs-embark-extras/>
[Embark – Key features and tweaks] <https://youtu.be/@code{qpoQiiinCtY>}
[Livestreamed: Adding an Embark context action to send a stream message]
<https://youtu.be/@code{WsxXr1ncukY>} [System Crafters Live! - The Many Uses of
Embark] <https://youtu.be/qk2Is_@code{sC8Lk>} [Using Emacs Episode 80 - Vertico,
Marginalia, Consult and Embark] <https://youtu.be/5ffb2at2d7w> 8 Contributions
═══════════════ Contributions to Embark are very welcome.  There is a [wish
list] for actions, target finders, candidate collectors and exporters.  For
other ideas you have for Embark, feel free to open an issue on the [issue
tracker].  Any neat configuration tricks you find might be a good fit for the
[wiki].  Code contributions are very welcome too, but since Embark is now on GNU
ELPA, copyright assignment to the FSF is required before you can contribute
code. [wish list] <https://github.com/oantolin/embark/issues/95> [issue tracker]
<https://github.com/oantolin/embark/issues> [wiki]
<https://github.com/oantolin/embark/wiki> 9 Acknowledgments ═════════════════
While I, Omar Antolín Camarena, have written most of the Embark code and remain
very stubborn about some of the design decisions, Embark has received
substantial help from a number of other people which this document has neglected
to mention for far too long.  In particular, Daniel Mendler has been absolutely
invaluable, implementing several important features, and providing a lot of
useful advice.  Code contributions: • [Daniel Mendler] • [Clemens Radermacher] •
[José Antonio Ortega Ruiz] • [Itai Y. Efrat] • [a13] • [jakanakaevangeli] •
[mihakam] • [Brian Leung] • [Karthik Chikmagalur] • [Roshan Shariff] •
[condy0919] • [Damien Cassou] • [@code{JimDBh}] Advice and useful discussions: •
[Daniel Mendler] • [Protesilaos Stavrou] • [Clemens Radermacher] • [Howard
Melman] • [Augusto Stoffel] • [Bruce d'Arcus] • [JD Smith] • [Karthik
Chikmagalur] • [jakanakaevangeli] • [Itai Y. Efrat] • [Mohsin Kaleem] [Daniel
Mendler] <https://github.com/minad> [Clemens Radermacher]
<https://github.com/clemera/> [José Antonio Ortega Ruiz]
<https://codeberg.org/jao/> [Itai Y. Efrat] <https://github.com/iyefrat> [a13]
<https://github.com/a13> [jakanakaevangeli]
<https://github.com/jakanakaevangeli> [mihakam] <https://github.com/mihakam>
[Brian Leung] <https://github.com/leungbk> [Karthik Chikmagalur]
<https://github.com/karthink> [Roshan Shariff]
<https://github.com/roshanshariff> [condy0919] <https://github.com/condy0919>
[Damien Cassou] <https://github.com/@code{DamienCassou>} [@code{JimDBh}]
<https://github.com/@code{JimDBh>} [Protesilaos Stavrou]
<https://gitlab.com/protesilaos/> [Howard Melman] <https://github.com/hmelman/>
[Augusto Stoffel] <https://github.com/astoff> [Bruce d'Arcus]
<https://github.com/bdarcus> [JD Smith] <https://github.com/jdtsmith> [Mohsin
Kaleem] <https://github.com/mohkale>.")
    (license license:gpl3+)))

(define-public emacs-copilot
  (package
    (name "emacs-copilot")
    (version "v0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/copilot-emacs/copilot.el.git")
             (commit "11b0739da1f74285dd661914c0ef92e24f9c4aa7")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pf5j3xhhcrv4dj2cgp7627s67wsw4hm308szqyr4f58snlkx044"))))
    (build-system emacs-build-system)
    (arguments
       `(#:include (cons "^dist\\/" %default-include)))
    (propagated-inputs (list emacs-s emacs-dash emacs-editorconfig
                             emacs-jsonrpc emacs-f node-lts))
    (home-page "https://github.com/copilot-emacs/copilot.el")
    (synopsis "An unofficial Copilot plugin")
    (description "An unofficial Copilot plugin for Emacs.")
    (license #f)))

(define-public emacs-embrace
  (package
    (name "emacs-embrace")
    (version "20231027.419")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cute-jumper/embrace.el.git")
             (commit "c7e748603151d7d91c237fd2d9cdf56e9f3b1ea8")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1c6fbkw1hl9bhdy62g782js8i9kgjr0pr132mpga12jd4cwf8mmz"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-expand-region))
    (home-page "https://github.com/cute-jumper/embrace.el")
    (synopsis "Add/Change/Delete pairs based on `expand-region'")
    (description
     "_____________ EMBRACE.EL Junpeng Qiu _____________ Table of Contents
_________________ 1 Overview 2 Usage ..  2.1 Example ..  2.2 Screencasts ..  2.3
`embrace-change and `embrace-delete ..  2.4 `embrace-add 3 Customization ..  3.1
Adding More Semantic Units ..  3.2 Adding More Surrounding Pairs ..  3.3 Disable
Help Message ..  3.4 Example Settings 4 For `evil-surround Users ..  4.1 Where
`embrace is better ..  4.2 Why not use together? 5 Contributions 6 Related
Packages Add/Change/Delete pairs based on [expand-region].  For `evil-surround
integration, see [evil-embrace]. [expand-region]
https://github.com/magnars/expand-region.el [evil-embrace]
https://github.com/cute-jumper/evil-embrace.el 1 Overview ========== This
package is heavily inspired by [evil-surround] (which is a port of the vim
plugin [surround.vim]).  But instead of using `evil and its text objects, this
package relies on another excellent package [expand-region].  For Emacs users
who don't like `evil and thus don't use `evil-surround', `embrace provides
similar commands that can be found in `evil-surround'. `Evil is absolutely *not*
required.  For `evil-surround users, `embrace can make your `evil-surround
commands even better! (Have you noticed that `evil-surround doesn't work on many
custom pairs?) [evil-surround] https://github.com/timcharper/evil-surround
[surround.vim] https://github.com/tpope/vim-surround [expand-region]
https://github.com/magnars/expand-region.el 2 Usage ======= There are three
commands: `embrace-add', `embrace-change and `embrace-delete that can add,
change, and delete surrounding pairs respectively.  You can bind these commands
to your favorite key bindings.  There is also a dispatch command
`embrace-commander'.  After invoking `embrace-commander', you can hit: - `a for
`embrace-add - `c for `embrace-change - `d for `embrace-delete 2.1 Example
~~~~~~~~~~~ It might be a little hard for users who have no experience in `evil
and `evil-surround to understand what `embrace can do.  So let's give an example
to show what `embrace can do fist.  You can look at the following sections to
see the meaning of key bindings.  In this example, I bind C-, to
`embrace-commander'.  Assume we have following text in `c-mode and the cursor
position is indicated by `|': ,---- | fo|o `---- Press C-, a w  to add  to the
current word: ,---- | fo|o `---- Press C-, a q { to add {} to outside of the
quotes: ,---- | {'fo|o'} `---- Press C-, c  \" to change the  to \"\": ,---- |
{\"fo|o\"} `---- Press C-, c { t, and then enter the tag: body class=\"page-body\",
to change the {} to a tag: ,---- | <body class=\"page-body\">\"fo|o\"</body> `----
Press C-, c t f, and enter the function name `bar to change the tag to a
function call: ,---- | bar(\"fo|o\") `---- Press C-, d f to remove the function
call: ,---- | \"fo|o\" `---- If you're an `evil-surround user, you might notice
that the last command can't be achieved by `evil-surround'.  However, it works
in `embrace'! And yes, you can find even more examples in which `evil-surround
doesn't work while `embrace works! 2.2 Screencasts ~~~~~~~~~~~~~~~ For non
`evil-mode users, use the following settings (they will be explained later):
,---- | (global-set-key (kbd \"C-,\") #'embrace-commander) | (add-hook
org-mode-hook #'embrace-org-mode-hook) `---- Open an org-mode file, we can
perform the following pair changing: [./screencasts/embrace.gif] For `evil-mode
users, here is a similar screencast (see [evil-embrace] for more details):
[https://github.com/cute-jumper/evil-embrace.el/blob/master/screencasts/evil-embrace.gif]
[evil-embrace] https://github.com/cute-jumper/evil-embrace.el 2.3
`embrace-change and `embrace-delete ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
These two commands can change and delete the surround pair respectively.  For
`evil-surround users, `embrace-change is similar to `cs and `embrace-delete is
similar to `ds'.  The surrounding pair is specified by a key, which is very
similar to the key used for Vim's text objects.  For example, `( stands for the
surrounding pair `( and `)', and `{ stands for the surrouding pair, `{ and `}'.
The default key mappings are shown below: Key Left right
-------------------------------- ( \"(\" \")\" ) \"( \" \" )\" { \"{\" \"}\" } \"{ \" \" }\" [
\"[\" \"]\" ] \"[ \" \" ]\" > \"<\" \">\" \" \"\\\"\" \"\\\"\"  \"\\'\" \"\\'\" ` \"`\" \"`\" t \"<foo bar=100>\"
\"</foo>\" f \"func(\" \")\" Note that for `t and `f key, the real content is based on
the user's input.  Also, you can override the closing quote when entering a `
(backquote) in emacs-lisp to get a  (apostrophe) instead of a ` (backquote) by
using `embrace-emacs-lisp-mode-hook (see below).  2.4 `embrace-add
~~~~~~~~~~~~~~~~~ This command is similar to `evil-surround''s `ys command.  We
need to enter a key for the semantic unit to which we want to add a surrounding
pair.  The semantic unit is marked by the functions provided by `expand-region'.
 Here is the default mapping: key mark function ----------------------------- w
er/mark-word s er/mark-symbol d er/mark-defun p er/mark-outside-pairs P
er/mark-inside-pairs q er/mark-outside-quotes Q er/mark-inside-quotes .
er/mark-sentence h er/mark-paragraph After pressing a key to select the semantic
unit, you can press another key to add the surrounding pair, which is the same
as `embrace-change and `embrace-delete'.  3 Customization =============== 3.1
Adding More Semantic Units ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ You can modify the
variable `embrace-semantic-units-alist and note that this variable is
buffer-local so it is better to change the value in a hook: ,---- | (add-hook
text-mode-hook | (lambda () | (add-to-list embrace-semantic-units-alist (?e .
er/mark-email)))) `---- 3.2 Adding More Surrounding Pairs
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Use the command `embrace-add-pair to add a
pair: ,---- | (embrace-add-pair key left right) `---- The change is also
buffer-local, so wrap it in a hook function: ,---- | (add-hook
@code{LaTeX-mode-hook} | (lambda () | (embrace-add-pair ?e \"\\\\begin{\" \"}\")))
`---- If you want add something like the `t key for the tag, you can look at the
function `embrace-add-pair-regexp in the source code.  Note that if you're using
`embrace-add-pair to add an existing key, then it will replace the old one.  3.3
Disable Help Message ~~~~~~~~~~~~~~~~~~~~~~~~ If you find the help message
annoying, use the following code to disable it: ,---- | (setq
embrace-show-help-p nil) `---- 3.4 Example Settings ~~~~~~~~~~~~~~~~~~~~ I
recommend binding a convenient key for `embrace-commander'.  For example, ,----
| (global-set-key (kbd \"C-,\") #'embrace-commander) `---- We have defined several
example hook functions that provide additional key bindings which can be used in
different major modes.  Right now there are hooks for `@code{LaTeX-mode}',
`org-mode', `ruby-mode (including `enh-ruby-mode') and `emacs-lisp-mode':
`@code{LaTeX-mode}': Key Left Right ---------------------- = \\verb| | ~ \\texttt{
} * \\textbf{ } `org-mode': Key Left Right
------------------------------------------ = = = ~ ~ ~ * * * _ _ _ + + + k
`@@@@html:<kbd>@@@@ `@@@@html:</kbd>@@@@ `ruby-mode and `enh-ruby-mode': Key
Left Right ------------------ # #{ } d do end `emacs-lisp-mode': Key Left Right
------------------ ` `  To use them: ,---- | (add-hook @code{LaTeX-mode-hook}
embrace-@code{LaTeX-mode-hook}) | (add-hook org-mode-hook embrace-org-mode-hook)
| (add-hook ruby-mode-hook embrace-ruby-mode-hook) ;; or enh-ruby-mode-hook |
(add-hook emacs-lisp-mode-hook embrace-emacs-lisp-mode-hook) `---- The code of
two of the hooks above (which are defined in `embrace.el'): ,---- | (defun
embrace-@code{LaTeX-mode-hook} () | (dolist (lst ((?= \"\\\\verb|\" . \"|\") | (?~
\"\\\\texttt{\" . \"}\") | (?/ \"\\\\emph{\" . \"}\") | (?* \"\\\\textbf{\" . \"}\"))) |
(embrace-add-pair (car lst) (cadr lst) (cddr lst)))) | (defun
embrace-org-mode-hook () | (dolist (lst ((?= \"=\" . \"=\") | (?~ \"~\" . \"~\") | (?/
\"/\" . \"/\") | (?* \"*\" . \"*\") | (?_ \"_\" . \"_\") | (?+ \"+\" . \"+\") | (?k
\"@@@@html:<kbd>@@@@\" . \"@@@@html:</kbd>@@@@\"))) | (embrace-add-pair (car lst)
(cadr lst) (cddr lst)))) `---- You can define and use your own hook function
similar to the code above.  Welcome to add some settings for more major modes.
4 For `evil-surround Users =========================== 4.1 Where `embrace is
better ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ From the previous example, you can see that
`embrace actually replicates all the funcionalities provided in `evil-surround
and it can even do more than `evil-surround'.  Actually, they are quite
different.  Since `embrace uses `expand-region behind the scene, you can expect
it to work as long as `expand-region works.  Unlike `evil-surround', which is
restricted to the pre-defined text objects, `embrace can define nearly arbitrary
surrounding pairs and three core commands always work.  On the contratry, you
get nearly no customization in `evil-surround': custom pairs don't work in `cs
or `ds if you don't have a corresponding text object defined (they work in
`ys'). *TL;DR*: `embrace is more customizable.  4.2 Why not use together?
~~~~~~~~~~~~~~~~~~~~~~~~~ Sure! You can make `embrace and `evil-surround work
together.  Look at [evil-embrace]! [evil-embrace]
https://github.com/cute-jumper/evil-embrace.el 5 Contributions ===============
This package is still in early stage, but it is quite usable right now.  More
functions can be added and the evil integration is not perfect yet.
Contributions are always welcome! 6 Related Packages ================== -
[evil-embrace] - [expand-region] - [evil-surround] - [change-inner] -
[smartparens] [evil-embrace] https://github.com/cute-jumper/evil-embrace.el
[expand-region] https://github.com/magnars/expand-region.el [evil-surround]
https://github.com/timcharper/evil-surround [change-inner]
https://github.com/magnars/change-inner.el [smartparens]
https://github.com/Fuco1/smartparens.")
    (license #f)))

(define-public emacs-evil-embrace
  (package
    (name "emacs-evil-embrace")
    (version "20230820.445")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cute-jumper/evil-embrace.el.git")
             (commit "3081d37811b6a3dfaaf01d578c7ab7a746c6064d")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "13rqkdhhzvnw3s49zm3v9xska8j8l1mr85czcfaf5vrm99lx8rl3"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-embrace emacs-evil-surround))
    (home-page "https://github.com/cute-jumper/evil-embrace.el")
    (synopsis "Evil integration of embrace.el")
    (description
     "______________ EVIL-EMBRACE Junpeng Qiu ______________ Table of Contents
_________________ 1 Overview 2 Why 3 Usage 4 Screencasts Evil integration of
[embrace.el]. [embrace.el] https://github.com/cute-jumper/embrace.el 1 Overview
========== This package provides evil integration of [embrace.el].  Since
`evil-surround provides a similar set of features as `embrace.el', this package
aims at adding the goodies of `embrace.el to `evil-surround and making
`evil-surround even better. [embrace.el]
https://github.com/cute-jumper/embrace.el 2 Why ===== `evil-surround is good
when there is a text object defined.  But unfortunately, if you want to add
custom surrouding pairs, `evil-surround will not be able to delete/change the
pairs if there are no evil text objects defined for these pairs.  For example,
if you want to make `\\textbf{ and `} as a surround pair in `@code{LaTeX-mode}',
you can't either change or delete the surround pair since there is no text
object for `\\textbf{ and `}'.  However, using `embrace', you can define whatever
surrounding pairs you like, and adding, changing, and deleting will *always*
work.  The idea of this package is that let `evil-surround handle the keys that
corresponds to existing text objects (i.e., `(', `[', etc.), which is what
`evil-surround is good at, and make `embrace handles all the other keys of
custom surrounding pairs so that you can also benefit from the extensibility
that `embrace offers.  In a word, you can use the default `evil-surround'.  But
whenever you want to add a custom surrounding pair, use `embrace instead.  To
see how to add a custom pair in `embrace', look at the README of [embrace.el].
[embrace.el] https://github.com/cute-jumper/embrace.el 3 Usage ======= To enable
the `evil-surround integration: ,---- |
(evil-embrace-enable-evil-surround-integration) `---- And use
`evil-embrace-disable-evil-surround-integration to disable whenever you don't
like it.  The keys that are processed by `evil-surround are saved in the
variable `evil-embrace-evil-surround-keys'.  The default value is: ,---- | (?\\(
?\\[ ?\\{ ?\\) ?\\] ?\\} ?\\\" ?\\ ?< ?> ?b ?B ?t ?w ?W ?s ?p) `---- Note that this
variable is buffer-local.  You should change it in the hook: ,---- | (add-hook
@code{LaTeX-mode-hook} | (lambda () | (add-to-list
evil-embrace-evil-surround-keys ?o))) `---- Only these keys saved in the
variable are processed by `evil-surround', and all the other keys will be
processed by `embrace'.  If you find the help message popup annoying, use the
following code to disable it: ,---- | (setq evil-embrace-show-help-p nil) `----
4 Screencasts ============= Use the following settings: ,---- | (add-hook
org-mode-hook embrace-org-mode-hook) |
(evil-embrace-enable-evil-surround-integration) `---- In an org-mode file, we
can change the surrounding pair in the following way (note that this whole
process can't be achieved solely by `evil-surround'):
[./screencasts/evil-embrace.gif].")
    (license #f)))

(define-public emacs-evil-iedit-state
  (package
    (name "emacs-evil-iedit-state")
    (version "20220219.1432")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/syl20bnr/evil-iedit-state.git")
             (commit "6f7b502447ba35676375169d7707372ebad2791f")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0vjzjmp3ba0nzf0v04bhxvzgdwwm11vivxqjzgnvp3kq95kajr5h"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-evil emacs-iedit))
    (home-page "https://github.com/syl20bnr/evil-iedit-state")
    (synopsis "Evil states to interface iedit mode")
    (description
     "Adds two new Evil states `iedit and `iedit insert with expand-region
integration.  For more info visit: https://github.com/syl20bnr/evil-iedit-state.")
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

(define-public emacs-ibuffer-project
  (package
    (name "emacs-ibuffer-project")
    (version "20220321.1312")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/muffinmad/emacs-ibuffer-project.git")
             (commit "9002abd9cb4c8753fe4f6c522d9407b4d52e7873")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "08164kv60r1mr0y9ha613w321xqnrsmdq9s3vfmjpmz4fm058bv6"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/muffinmad/emacs-ibuffer-project")
    (synopsis "Group ibuffer's list by project or any function")
    (description
     "This pacakage provides ibuffer filtering and sorting functions to group buffers
by function or regexp applied to `default-directory'.  By default buffers are
grouped by `project-current or by `default-directory'.  Buffer group and group
type name is determined by function or regexp listed in
`ibuffer-project-root-functions'.  E.g.  by adding `file-remote-p like this:
(add-to-list ibuffer-project-root-functions (file-remote-p . \"Remote\")) remote
buffers will be grouped by protocol and host.  To group buffers set
`ibuffer-filter-groups to result of `ibuffer-project-generate-filter-groups
function: (add-hook ibuffer-hook (lambda () (setq ibuffer-filter-groups
(ibuffer-project-generate-filter-groups)))) This package also provides column
with filename relative to project.  If there are no file in buffer then column
will display `buffer-name with `font-lock-comment-face face.  Add
project-file-relative to `ibuffer-formats': (custom-set-variables
(ibuffer-formats ((mark modified read-only locked \" \" (name 18 18 :left :elide)
\" \" (size 9 -1 :right) \" \" (mode 16 16 :left :elide) \" \"
project-file-relative)))) It's also possible to sort buffers by that column by
calling `ibuffer-do-sort-by-project-file-relative or: (add-hook ibuffer-hook
(lambda () (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
(unless (eq ibuffer-sorting-mode project-file-relative)
(ibuffer-do-sort-by-project-file-relative)))) To avoid calculating project root
each time, one can set `ibuffer-project-use-cache'.  Root info per directory
will be stored in the `ibuffer-project-roots-cache variable.  Command
`ibuffer-project-clear-cache allows to clear project info cache.")
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

(define-public emacs-package-lint-flymake
  (package
    (name "emacs-package-lint-flymake")
    (version "20240923.931")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/purcell/package-lint.git")
             (commit "6f05a369e0718e93c5dce0951cad5e6646296612")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "08f8pw27nk6pjx8qydn2xf6d5026afhaa50263wlwsgymsbkyimb"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-package-lint))
    (arguments
     '(#:include '("^package-lint-flymake.el$")
       #:tests? #f
       #:exclude '()))
    (home-page "https://github.com/purcell/package-lint")
    (synopsis "A package-lint Flymake backend")
    (description
     "Flymake is the built-in Emacs package to support on-the-fly syntax checking.
This library adds support for flymake to `package-lint'.  It requires Emacs 26.
Enable it by calling `package-lint-flymake-setup from a file-visiting buffer.
To enable in all `emacs-lisp-mode buffers: (add-hook emacs-lisp-mode-hook
#'package-lint-flymake-setup).")
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
