(require 'org-super-agenda)

(setq org-directory (expand-file-name "org" (getenv "CEREBRUM_PATH")))
(setq org-agenda-files (list org-directory))

;; Set up org directory
(setq org-default-notes-file (expand-file-name "notes.org" org-directory))
(setq inbox-file (expand-file-name "inbox.org" org-directory))
(setq recycle-bin-file (expand-file-name "recycle-bin.org" org-directory))
(setq backlog-file (expand-file-name "backlog.org" org-directory))
(setq projects-file (expand-file-name "projects.org" org-directory))

;; Save org buffers on org-agenda-redo (redraw agenda)
(advice-add 'org-agenda-redo :after 'org-save-all-org-buffers)

;; Done task have timestamps attached
(setq org-log-done 'time)

;; Tag inheritance
(setq org-use-tag-inheritance t)
(setq org-tags-exclude-from-inheritance '("PROJECT"))

(defun org-agenda-todo-next ()
    "Org agenda todo next cycle"
    (interactive)
    (org-call-with-arg 'org-agenda-todo 'right))

(defun org-agenda-todo-prev ()
    "Org agenda todo next cycle"
    (interactive)
    (org-call-with-arg 'org-agenda-todo 'left))

;; Define priority faces
(setq org-highest-priority ?A)
(setq org-default-priority ?D)
(setq org-lowest-priority ?C)
(setq org-priority-start-cycle-with-default nil)
(setq org-priority-faces
      '((?A . (:foreground "red" :weight bold))
        (?B . (:foreground "orange"))
        (?C . (:foreground "yellow"))))

;; Define todo keywords
(setq org-todo-keywords
      '((sequence "WAITING(w)" "TODO(t)" "NEXT(n)" "IN-PROGRESS(i)" "|" "DONE(d)")))

;; custom faces for todo keywords
(setq org-todo-keyword-faces
      '(("WAITING" . (:foreground "blue" :weight bold))
        ("TODO" . (:foreground "white" :weight bold))
        ("NEXT" . (:foreground "yellow" :weight bold))
        ("IN-PROGRESS" . (:foreground "orange" :weight bold))
        ("DONE" . (:foreground "green" :weight bold))))

;; Clock in/out when task state changes to/from IN-PROGRESS
(defun org-clock-toggle-by-state ()
  (if (and (string= org-state "IN-PROGRESS")
           (not (org-clock-is-active)))
      (org-clock-in)
    (when (org-clock-is-active)
      (org-clock-out))))
(add-hook 'org-after-todo-state-change-hook #'org-clock-toggle-by-state)

;; Custom agenda views
(org-super-agenda-mode)
(defconst default-agenda-prefix-format "%-25b %t")
(setq org-agenda-sort-notime-is-late nil)
(setq org-agenda-breadcrumbs-separator ":")
(setq org-agenda-custom-commands
      '(("g" "Get Things Done (GTD)"
         ((agenda ""
                  ((org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'deadline))
                  (org-deadline-warning-days 0)
                  (org-agenda-remove-tags t)
                  (org-agenda-prefix-format (concat default-agenda-prefix-format " "))))
          (todo "WAITING"
                ((org-agenda-prefix-format default-agenda-prefix-format)
                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline))
                 (org-agenda-sorting-strategy '(priority-down timestamp-down))
                 (org-agenda-overriding-header "Waiting (on hold")))
          (todo "NEXT"
                ((org-agenda-prefix-format default-agenda-prefix-format)
                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline))
                 (org-agenda-sorting-strategy '(priority-down timestamp-down))
                 (org-agenda-overriding-header "Ready to Pick Up")))
          (todo "IN-PROGRESS"
                ((org-agenda-prefix-format default-agenda-prefix-format)
                 (org-agenda-sorting-strategy '(priority-down timestamp-down))
                 (org-agenda-overriding-header "In Progress")))
          (agenda ""
                  ((org-agenda-entry-types '(:deadline))
                   (org-deadline-warning-days 0)
                   (org-agenda-remove-tags t)
                   (org-agenda-sorting-strategy '(priority-down timestamp-down))
                   (org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'todo '("DONE")))
                   (org-agenda-overriding-header "Deadlines")))
          (tags "inbox"
                     ((org-agenda-files (list inbox-file))
                      (org-agenda-remove-tags t)
                      (org-agenda-prefix-format "")
                      (org-agenda-sorting-strategy '(timestamp-down))
                      (org-agenda-overriding-header "Inbox")))
          (tags "CLOSED>=\"<-1d>\""
                ((org-agenda-prefix-format default-agenda-prefix-format)
                 (org-agenda-sorting-strategy '(timestamp-down))
                 (org-agenda-overriding-header "Completed Recently (Last 24h)")))))
        ("c" "Tasks"
          ((todo "WAITING"
                ((org-agenda-prefix-format default-agenda-prefix-format)
                 (org-agenda-files (list projects-file))
                 (org-agenda-sorting-strategy '(priority-down timestamp-down))
                 (org-agenda-overriding-header "Waiting (on hold)")))
          (todo "TODO"
                ((org-agenda-prefix-format default-agenda-prefix-format)
                 (org-agenda-files (list projects-file))
                 (org-agenda-sorting-strategy '(priority-down timestamp-down))
                 (org-agenda-overriding-header "TODO")))
          (todo "NEXT"
                ((org-agenda-prefix-format default-agenda-prefix-format)
                 (org-agenda-files (list projects-file))
                 (org-agenda-sorting-strategy '(priority-down timestamp-down))
                 (org-agenda-overriding-header "Next TODO")))
          (todo "IN-PROGRESS"
                ((org-agenda-prefix-format default-agenda-prefix-format)
                 (org-agenda-files (list projects-file))
                 (org-agenda-sorting-strategy '(priority-down timestamp-down))
                 (org-agenda-overriding-header "In-Progress")))
          (tags "CLOSED>=\"<today>\""
                ((org-agenda-prefix-format default-agenda-prefix-format)
                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'nottodo 'done))
                 (org-agenda-files (list projects-file))
                 (org-agenda-overriding-header "Completed Today")))
          (todo "DONE"
                ((org-agenda-max-entries 10)
                 (org-agenda-sorting-strategy '(timestamp-down))
                 (org-agenda-prefix-format default-agenda-prefix-format)
                 (org-agenda-files (list projects-file))
                 (org-agenda-overriding-header "Completed")))))
        ("p" "Projects"
         ((todo ""
                ((org-agenda-prefix-format "%t")
                 (org-super-agenda-groups '((:auto-parent t)))))))
        ("B" "Backlog Items"
         ((tags "LEVEL=1+backlog"
                ((org-agenda-overriding-header "Backlog Items")
                  (org-agenda-files (list backlog-file))
                  (org-agenda-prefix-format "")
                  (org-agenda-todo-ignore-with-date t)))))))

;; Refile settings
;;; refile to the top level
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
;;; create a parent when refiling on-the-fly
(setq org-refile-allow-creating-parent-nodes 'confirm)
;;; refile targets
(setq org-refile-targets
      '((org-default-notes-file :maxlevel . 1)
        (backlog-file :maxlevel . 1)
        (projects-file :maxlevel . 2)
        (recycle-bin-file :maxlevel . 1)))

;; Archive settings
(setq archive-directory (expand-file-name "archive" org-directory))
(setq org-archive-location (concat archive-directory "/%s_archive::"))

;; Stucked projects
(setq org-stuck-projects
      '("PROJECT" ("NEXT" "IN-PROGRESS") nil ""))

(setq org-capture-templates
      '(("i" "New Inbox" entry (file inbox-file)
         "* TODO %? \n:PROPERTIES:\n:CREATED: %U\n:END:"
         :empty-lines 1)
        ("p" "New Project" entry
         (file projects-file)
         "* %^{Project Name} %^g:PROJECT:\n:PROPERTIES:\n:CREATED: %U\n:DESCRIPTION: %^{Brief Description}\n:DEFINITION-OF-DONE: %^{Definition of Done}\n:END:"
         :empty-lines 1)
        ("n" "Note" entry (file org-default-notes-file)
         "* %? \n:PROPERTIES:\n:CREATED: %U\n:ID: %(org-id-new)\n:END:"
         :jump-to-captured t
         :empty-lines 1)))

(provide 'emacs-config-org)
