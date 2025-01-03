(defgroup emacs-config-startup '()
  "Startup configuration for Emacs"
  :tag "Startup"
  :group 'emacs-config)

(defcustom emacs-config-startup-concise-splash nil
  "Show a more concise Emacs Splash screen, without the logo."
  :type 'boolean
  :group 'emacs-config-startup)

(defvar emacs-config-startup-screen-inhibit-startup-screen nil)

(defcustom emacs-config-startup-graphical-logo "image"
  "Show a logo on the splash screen when in a graphical environment. When set to \"image\", it will display the image defined in `fancy-splash-image', when set to \"ascii\", it will display the contents of `emacs-config-startup-ascii-logo'. For everything else, it will not display a logo."
  :type 'string
  :group 'emacs-config-startup)

(defcustom emacs-config-startup-terminal-logo t
  "Show a logo on the splash screen when in a terminal. When set to non-nil, it will display the contents of `emacs-config-startup-ascii-logo'."
  :type 'boolean
  :group 'emacs-config-startup)

(defcustom emacs-config-startup-recentf-count 10
  "The number of recent files to display on the splash screen"
  :type 'number
  :group 'emacs-config-startup)

(defcustom emacs-config-startup-project-count 10
  "The number of projects to display on the splash screen"
  :type 'number
  :group 'emacs-config-startup)

(defface emacs-config-greeting-face
  '((t (:inherit font-lock-comment-face :weight bold :height 1.5)))
  "Face for the welcoming message."
  :group 'emacs-config-faces)

(customize-set-variable 'fancy-splash-image
                        (expand-file-name
                         "emacs.png" user-emacs-directory))

(defconst emacs-config-startup-ascii-logo
  '("EMACS!")
  "A list of strings representing the lines of an ASCII logo for the
splash screen. Make sure the first line is as wide as the widest
line or the centering will by off.")

(defun emacs-config-startup--center-with-face (str &optional face)
  "Figure out how many spaces must be prepended to STR so it is centered with FACE. Return the text with as many prepended spaces as needed."
  (let* ((str-mid (/ (length str) 2))
         (line-width (window-max-chars-per-line nil face)))
    (concat
     (make-string (abs (- (/ line-width 2) str-mid)) ? ) ;;fill w/ spaces
     str)))

(defconst emacs-config-welcome-text
  "Welcome to Emacs!"
  "First text on the splash screen.")

(defcustom emacs-config-startup-module-list '(emacs-config-startup-projects)
  "List of functions to call to display \"modules\" on the splash
screen.  Functions are called in the order listed.  See
`emacs-config-startup-recentf' as an example.  Current list provided
 by config is `emacs-config-startup-projects', `emacs-config-startup-recentf'"
  :type '(repeat function)
  :group 'emacs-config-startup)

(defun emacs-config-startup-splash-head ()
  (if (display-graphic-p)
      (cond
       ((string= emacs-config-startup-graphical-logo
                 "image") (emacs-config-startup--splash-head-image))
       ((string= emacs-config-startup-graphical-logo
                 "ascii") (emacs-config-startup--splash-head-ascii))
       (t nil)) ;;do nothing in all other cases
    ;; ASCII art in the terminal?
    (when emacs-config-startup-terminal-logo
      (emacs-config-startup--splash-head-ascii))))

(defun emacs-config-startup--splash-head-image ()
  "Insert the head part of the splash screen into the current buffer."
  (let* ((image-file (fancy-splash-image-file))
	 (img (create-image image-file))
	 (text-width (window-width))
	 (image-width (and img (car (image-size img)))))
    (when img
      (when (> text-width image-width)
        ;; Center the image
        (insert (propertize " " 'display
                            `(space :align-to (+ ,(- (/ text-width 2) 0)
                                                 (-0.5 . ,img)))))
        (insert-image img)
        (insert "\n\n")))))

(defun emacs-config-startup--splash-head-ascii ()
  "Insert the contents of `emacs-config-startup-ascii-logo' into the
 splash screen as a logo."
  (let* ((logo-face 'fixed-pitch)
         (first-line (car emacs-config-startup-ascii-logo))
         (line-width (window-max-chars-per-line nil logo-face))
         (spaces
          (make-string
           (abs (- (/ line-width 2)
                   (/ (length first-line) 2)))
           ? )))
    (dolist (line emacs-config-startup-ascii-logo)
      (fancy-splash-insert :face logo-face (concat spaces line "\n")))
    (insert "\n")))

(defun emacs-config-startup-projects ()
  (require 'project nil :noerror)
  (when (file-exists-p project-list-file)
    (project--read-project-list)
    (message "Showing projects on splash screen")
    (fancy-splash-insert
     :face '(variable-pitch font-lock-string-face italic)
     (condition-case project--list
         (if (not (seq-empty-p project--list))
             "Projects:\n"
           "\n")
       (error "\n")))
    (condition-case project--list
        (if (not (seq-empty-p project--list))
            (dolist (proj (seq-take project--list emacs-config-startup-project-count))
              (let ((project-name (car proj))) ; Introduce a closure to capture the value of `proj`
                (fancy-splash-insert
                :link `(,project-name (lambda (_button) (project-switch-project ,project-name)))
                "\n")))
          "\n")
      (error "\n"))))

(defun emacs-config-startup-recentf ()
  (message "Showing recents on splash screen")
  (fancy-splash-insert
   :face '(variable-pitch font-lock-string-face italic)
   (condition-case recentf-list
       (if (not (seq-empty-p recentf-list))
           "Recent Files:\n"
         "\n")
     (error "\n")))
  (condition-case recentf-list
      (if (not (seq-empty-p recentf-list))
          (dolist (file (seq-take recentf-list emacs-config-startup-recentf-count))
            (let ((file-name file)) ; Introduce a closure to capture the value of `proj`
              (fancy-splash-insert
              :link `(,file-name (lambda (_button) (find-file ,file-name)))
              "\n")))
        "\n")
    (error "\n")))

(defun emacs-config-startup-screen ()
  "Calls `emacs-config-startup--display-startup-screen' with
`emacs-config-startup-concise-splash'."
  (emacs-config-startup--display-startup-screen nil))

(defun emacs-config-startup--display-startup-screen (&optional concise)
  "Display fancy startup screen.
If CONCISE is non-nil, display a concise version of the splash
screen in another window.  This function can be bound to
`initial-buffer-choice' which will run this function when Emacs
starts.  See the variable documenation for
`initial-buffer-choice' for more information."
  (message "Loading Startup Screen")
  (let ((splash-buffer (get-buffer-create "*Home*")))
    (with-current-buffer splash-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (setq default-directory command-line-default-directory)
        (make-local-variable 'emacs-config-startup-screen-inhibit-startup-screen)
        (if pure-space-overflow
            (insert pure-space-overflow-message))
        (unless concise
          (emacs-config-startup-splash-head))            ;; display the logo
        (apply #'fancy-splash-insert                ;; insert welcome text
               `(:face (emacs-config-greeting-face)
                      ,(emacs-config-startup--center-with-face
                        emacs-config-welcome-text
                        'emacs-config-greeting-face)))
        (insert "\n\n")
        (mapc (lambda (f)
                (insert "\n")
                (funcall f)
                (skip-chars-backward "\n")
                (delete-region (point) (point-max))
                (insert "\n"))
              emacs-config-startup-module-list)
        (skip-chars-backward "\n")
        (delete-region (point) (point-max))
        (insert "\n"))
      (use-local-map splash-screen-keymap)
      (setq tab-width 22
            buffer-read-only t)
      (set-buffer-modified-p nil)
      (if (and view-read-only (not view-mode))
          (view-mode-enter nil 'kill-buffer))
      (goto-char (point-min))
      (forward-line (if concise 2 4)))
    (if concise
        (progn
          (display-buffer splash-buffer)
          ;; If the splash screen is in a split window, fit it.
          (let ((window (get-buffer-window splash-buffer t)))
            (or (null window)
                (eq window (selected-window))
                (eq window (next-window window))
                (fit-window-to-buffer window))
            buf)) ;; return the buffer
      (switch-to-buffer splash-buffer))))

(setq initial-buffer-choice #'emacs-config-startup-screen)

(provide 'emacs-config-startup)
