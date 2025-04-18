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

;; Turn on tag inheritance
(setq org-use-tag-inheritance t)

(defun org-agenda-todo-next ()
    "Org agenda todo next cycle"
    (interactive)
    (org-call-with-arg 'org-agenda-todo 'right))

(defun org-agenda-todo-prev ()
    "Org agenda todo next cycle"
    (interactive)
    (org-call-with-arg 'org-agenda-todo 'left))

;; Define priority faces
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
(setq org-agenda-custom-commands
      '(("g" "Get Things Done (GTD)"
         ((agenda ""
                  ((org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'deadline))
                   (org-deadline-warning-days 0)))
          (todo "NEXT"
                ((org-agenda-skip-function
                  '(org-agenda-skip-entry-if 'deadline))
                 (org-agenda-sorting-strategy '(timestamp-down)
                 (org-agenda-prefix-format "  %i %-25:c [%e] ")
                 (org-agenda-overriding-header "\nNext TODO\n"))))
          (agenda nil
                  ((org-agenda-entry-types '(:deadline))
                   (org-agenda-format-date "")
                   (org-deadline-warning-days 7)
                   (org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'todo '("NEXT")))
                   (org-agenda-overriding-header "\nDeadlines")))
          (tags "inbox"
                     ((org-agenda-prefix-format "  %?-25t% s")
                      (org-agenda-files (list inbox-file))
                      (org-agenda-sorting-strategy '(timestamp-down)
                      (org-agenda-overriding-header "\nInbox\n")))
          (tags "CLOSED>=\"<today>\""
                ((org-agenda-overriding-header "\nCompleted today\n"))))))
        ("c" "Tasks"
          ((todo "WAITING"
                ((org-agenda-prefix-format "%?-25(car (org-get-outline-path)) %t %s")
                 (org-agenda-files (list projects-file))
                 (org-agenda-breadcrumbs-separator "")
                 (org-agenda-overriding-header "Waiting (on hold)")))
          (todo "TODO"
                ((org-agenda-prefix-format "%?-25(car (org-get-outline-path)) %t %s")
                 (org-agenda-breadcrumbs-separator "")
                 (org-agenda-files (list projects-file))
                 (org-agenda-overriding-header "TODO")))
          (todo "NEXT"
                ((org-agenda-prefix-format "%?-25(car (org-get-outline-path)) %t %s")
                 (org-agenda-breadcrumbs-separator "")
                 (org-agenda-files (list projects-file))
                 (org-agenda-overriding-header "Next TODO")))
          (todo "IN-PROGRESS"
                ((org-agenda-prefix-format "%?-25(car (org-get-outline-path)) %t %s")
                 (org-agenda-breadcrumbs-separator "")
                 (org-agenda-files (list projects-file))
                 (org-agenda-overriding-header "In-Progress")))
          (tags "CLOSED>=\"<today>\""
                ((org-agenda-prefix-format "%?-25(car (org-get-outline-path)) %t %s")
                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'nottodo 'done))
                 (org-agenda-breadcrumbs-separator "")
                 (org-agenda-files (list projects-file))
                 (org-agenda-overriding-header "Completed Today")))
          (todo "DONE"
                ((org-agenda-max-entries 10)
                 (org-agenda-sorting-strategy '(timestamp-down))
                 (org-agenda-prefix-format "%?-25(car (org-get-outline-path)) %t %s")
                 (org-agenda-breadcrumbs-separator "")
                 (org-agenda-files (list projects-file))
                 (org-agenda-overriding-header "Completed")))))
        ("p" "Projects"
                ((org-agenda-prefix-format "%t %s")
                 (org-super-agenda-groups '((:auto-parent t)))))
        ("B" "Backlog Items"
         ((tags "backlog"
                ((org-agenda-overriding-header "Backlog Items")
                  (org-agenda-files (list backlog-file))
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

(setq org-capture-templates
      '(("i" "New Inbox" entry (file inbox-file)
         "* TODO %? \n:PROPERTIES:\n:CREATED: %U\n:END:"
         :empty-lines 1)
        ("p" "New Project" entry
         (file projects-file)
         "* %^{Project Name} %^g\n:PROPERTIES:\n:CREATED: %U\n:DESCRIPTION: %^{Brief Description}\n:DEFINITION-OF-DONE: %^{Definition of Done}\n:END:"
         :empty-lines 1)
        ("n" "Note" entry (file org-default-notes-file)
         "* %? \n:PROPERTIES:\n:CREATED: %U\n:END:"
         :jump-to-captured t
         :empty-lines 1)))

(provide 'emacs-config-org)
