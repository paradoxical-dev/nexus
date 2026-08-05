;;; nexus-dashboard.el --- Custom Doom Dashboard -*- lexical-binding: t; -*-

;;; ─── Configuration ────────────────────────────────────────────────────────────

(defvar nexus-dashboard-logo-path
  (expand-file-name "dash-logo.png" doom-user-dir)
  "Path to your PNG logo.")

(defvar nexus-dashboard-logo-width 200
  "Display width of the logo in pixels.")

(defvar nexus-dashboard-logo-max-height 120
  "Max display height of the logo in pixels.")

(defvar nexus-dashboard-quotes
  '("The quieter you become, the more you can hear."
    "Simplicity is the ultimate sophistication."
    "First, solve the problem. Then, write the code."
    "Make it work, make it right, make it fast."
    "Programs must be written for people to read."
    "Any fool can write code that a computer can understand."
    "The best code is no code at all.")
  "List of quotes to randomly display on the dashboard.")

(defvar nexus-dashboard-quick-actions
  '(("[f]" "find file"        doom-dashboard-find-file)
    ("[r]" "recent files"     consult-recent-file)
    ("[p]" "switch project"   projectile-switch-project)
    ("[t]" "open terminal"    +vterm/here)
    ("[q]" "quit"       save-buffers-kill-terminal))
  "Quick actions: (KEY LABEL COMMAND). Keys are bound in dashboard buffer.")

(defvar nexus-dashboard-box-width 50
  "Width (in chars) of each of the two bottom boxes.")

(defvar nexus-dashboard-top-padding 2
  "Number of blank lines to insert at the top of the dashboard.")

(defvar nexus-dashboard-max-todos 8
  "Maximum number of org-agenda TODOs to display.")

(defvar nexus-dashboard-max-projects 8
  "Maximum number of recent projects to display.")

(defvar nexus-dashboard--todo-cache 'not-loaded
  "Cached list of TODO entries. 'not-loaded means not yet fetched.")

(defvar nexus-dashboard--project-cache 'not-loaded
  "Cached list of recent projects. 'not-loaded means not yet fetched.")

;;; ─── Faces ────────────────────────────────────────────────────────────────────

(defface nexus-dashboard-title-face
  '((t :weight bold :height 1.3))
  "Face for the N E X U S title above the stats box.")

(defface nexus-dashboard-border-face
  '((t :inherit default))
  "Face for box border characters ╭─╮│╰╯ and box titles.")

(defface nexus-dashboard-box-title-face
  '((t :weight bold))
  "Face for the titles embedded in box borders.")

(defface nexus-dashboard-entry-face
  '((t :inherit default))
  "Face for entry content in Recent Projects and Open Tasks boxes.")

(defface nexus-dashboard-date-face
  '((t :slant italic))
  "Face for dates and separators in Recent Projects and Open Tasks entries.")

(defface nexus-dashboard-footer-face
  '((t :inherit shadow))
  "Face for the Quick Actions footer content.")

(defface nexus-dashboard-footer-separator-face
  '((t :inherit shadow))
  "Face for the ─── separator line above the footer.")

;;; ─── Box Drawing Helpers ──────────────────────────────────────────────────────

(defun nexus--box-top (width &optional title)
  (if title
      (let* ((title-len (+ 2 (length title)))
             (inner     (- width 2))
             (left      (/ (- inner title-len) 2))
             (right     (- inner title-len left)))
        (concat
         (propertize (concat "╭" (make-string left ?─) " ")
                     'face 'nexus-dashboard-border-face)
         (propertize title 'face 'nexus-dashboard-box-title-face)
         (propertize (concat " " (make-string right ?─) "╮")
                     'face 'nexus-dashboard-border-face)))
    (propertize
     (concat "╭" (make-string (- width 2) ?─) "╮")
     'face 'nexus-dashboard-border-face)))

(defun nexus--box-bottom (width)
  (propertize
   (concat "╰" (make-string (- width 2) ?─) "╯")
   'face 'nexus-dashboard-border-face))

(defun nexus--box-row (content width)
  (let* ((plain (substring-no-properties content))
         (pad   (max 0 (- width 4 (nexus--string-width-px content)))))
    (concat (propertize "│ " 'face 'nexus-dashboard-border-face)
            content
            (make-string pad ?\s)
            (propertize " │" 'face 'nexus-dashboard-border-face))))

(defun nexus--body-width ()
  "Return the usable body width of the dashboard window."
  (window-body-width))

(defun nexus--center (str total-width)
  "Return STR left-padded so it appears centered in TOTAL-WIDTH chars."
  (let* ((len (string-width (substring-no-properties str)))
         (pad (max 0 (/ (- total-width len) 2))))
    (concat (make-string pad ?\s) str)))

(defun nexus--insert-centered (str)
  "Insert STR centered in the dashboard window."
  (insert (nexus--center str (nexus--body-width)) "\n"))

(defun nexus--insert-centered-pixel (str)
  "Insert STR centered using pixel-accurate width measurement."
  (let* ((pixel-width (string-pixel-width str))
         (char-width  (frame-char-width))
         (effective   (ceiling pixel-width char-width))
         (pad         (max 0 (/ (- (nexus--body-width) effective) 2))))
    (insert (make-string pad ?\s) str "\n")))

(defun nexus--string-width-px (str)
  "Return effective char-cell width of STR accounting for wide glyphs."
  (ceiling (string-pixel-width str) (frame-char-width)))

;;; ─── Section: Logo ────────────────────────────────────────────────────────────

(defun nexus-dashboard--insert-logo ()
  "Insert the PNG logo centered, falling back to ASCII if unavailable."
  (if (and (display-graphic-p)
           (file-exists-p nexus-dashboard-logo-path))
      (let* ((img (create-image nexus-dashboard-logo-path 'png nil
                                :width nexus-dashboard-logo-width
                                :max-height nexus-dashboard-logo-max-height))
             (img-pixel-width (car (image-size img t)))
             (img-cols (ceiling img-pixel-width (frame-char-width)))
             (padding  (max 0 (/ (- (nexus--body-width) img-cols) 2))))
        (insert (make-string padding ?\s))
        (insert-image img)
        (insert "\n\n"))
    (nexus--insert-centered "███╗   ██╗███████╗██╗  ██╗██╗   ██╗███████╗")
    (nexus--insert-centered "████╗  ██║██╔════╝╚██╗██╔╝██║   ██║██╔════╝")
    (nexus--insert-centered "██╔██╗ ██║█████╗   ╚███╔╝ ██║   ██║███████╗")
    (nexus--insert-centered "██║╚██╗██║██╔══╝   ██╔██╗ ██║   ██║╚════██║")
    (nexus--insert-centered "██║ ╚████║███████╗██╔╝ ██╗╚██████╔╝███████║")
    (nexus--insert-centered "╚═╝  ╚═══╝╚══════╝╚═╝  ╚═╝ ╚═════╝ ╚══════╝")
    (insert "\n")))

;;; ─── Section: Greeting ────────────────────────────────────────────────────────

(defun nexus-dashboard--load-stats ()
  "Return a string with package and module counts and load time."
  (format "%d packages loaded across %d modules in %.2fs"
          (length (doom-package-list))
          (hash-table-count doom-modules)
          doom-init-time))

(defun nexus-dashboard--insert-greeting ()
  "Insert the NEXUS title above a stats box."
  (let* ((box-width 56)
         (title-str (propertize "N E X U S" 'face 'nexus-dashboard-title-face))
         (stats-str (nexus-dashboard--load-stats))
         (top    (nexus--box-top box-width))
         (spacer (nexus--box-row "" box-width))
         (stats  (nexus--box-row (nexus--center stats-str (- box-width 4)) box-width))
         (bot    (nexus--box-bottom box-width)))
    (nexus--insert-centered-pixel title-str)
    (insert "\n")
    (nexus--insert-centered top)
    (nexus--insert-centered spacer)
    (nexus--insert-centered stats)
    (nexus--insert-centered spacer)
    (nexus--insert-centered bot)
    (insert "\n")))

;;; ─── Section: Quote ───────────────────────────────────────────────────────────

(defun nexus-dashboard--insert-quote ()
  "Insert a randomly selected quote, centered."
  (let ((quote (nth (random (length nexus-dashboard-quotes))
                    nexus-dashboard-quotes)))
    (nexus--insert-centered
     (propertize (concat "❝  " quote "  ❞") 'face '(:slant italic)))
    (insert "\n")))

;;; ─── Section: Bottom Boxes ────────────────────────────────────────────────────

(defun nexus-dashboard--make-status-tag (keyword)
  "Return a propertized string displaying KEYWORD as an SVG tag."
  (let* ((face (pcase keyword
                 ("TODO"    'org-todo)
                 ("NEXT"    'org-upcoming-deadline)
                 ("WAITING" 'org-warning)
                 (_         'default)))
         (svg (svg-tag-make keyword :face face :inverse t :margin 1)))
    (propertize (concat " " keyword " ") 'display svg)))

(defun nexus-dashboard--truncate-title (title max-width)
  "Truncate TITLE to MAX-WIDTH chars, appending … if needed."
  (if (> (string-width title) max-width)
      (concat (substring title 0 (- max-width 1)) "…")
    title))

(defun nexus-dashboard--format-todo (entry)
  "Format a todo ENTRY (KEYWORD TITLE DATE) as a single box row string."
  (let* ((keyword   (car entry))
         (title     (cadr entry))
         (date      (caddr entry))
         (tag       (nexus-dashboard--make-status-tag keyword))
         (tag-chars (nexus--string-width-px tag))
         (date-str  (propertize date 'face 'nexus-dashboard-date-face))
         (date-len  (length date))
         (sep       "  ")
         (sep-len   (length sep))
         (inner     (- nexus-dashboard-box-width 4))
         (title-max (- inner tag-chars 1 sep-len 1 date-len))
         (truncated (propertize
                     (nexus-dashboard--truncate-title title title-max)
                     'face 'nexus-dashboard-entry-face))
         (title-pad (- inner tag-chars 1
                       (string-width (substring-no-properties truncated))
                       sep-len 1 date-len))
         (padded-sep (propertize
                      (concat (make-string (max 1 title-pad) ?\s) sep " ")
                      'face 'nexus-dashboard-date-face)))
    (concat tag " " truncated padded-sep date-str)))

(defun nexus-dashboard--fetch-todos-sync ()
  "Return list of (KEYWORD TITLE DATE) for deadline TODOs."
  (require 'org-agenda)
  (let (todos)
    (dolist (file (org-agenda-files))
      (when (file-exists-p file)
        (with-temp-buffer
          (insert-file-contents file)
          (delay-mode-hooks (org-mode))
          (org-element-map (org-element-parse-buffer) 'headline
            (lambda (hl)
              (let* ((kw       (org-element-property :todo-keyword hl))
                     (deadline (org-element-property :deadline hl)))
                (when (and (member kw '("TODO" "NEXT" "WAITING")) deadline)
                  (let* ((ts   (org-element-property :raw-value deadline))
                         (time (org-parse-time-string ts))
                         (date (format-time-string "%b %d"
                                                   (apply #'encode-time time))))
                    (push (list kw
                                (org-element-property :raw-value hl)
                                date)
                          todos)))))))))
    (seq-take (nreverse todos) nexus-dashboard-max-todos)))

;;; ─── Section: Recent Projects ─────────────────────────────────────────────────

(defun nexus-dashboard--project-access-time (project)
  "Return the most recent file access time within PROJECT as a time value."
  (let* ((files (condition-case nil
                    (directory-files-recursively
                     project ".*" nil
                     (lambda (f) (not (string-match-p
                                       "/\\(\\..+\\|node_modules\\|.git\\)/" f))))
                  (error nil)))
         (times (mapcar (lambda (f)
                          (condition-case nil
                              (nth 5 (file-attributes f))
                            (error nil)))
                        files)))
    (car (sort (delq nil times) #'time-less-p))))

(defun nexus-dashboard--fetch-projects-sync ()
  "Return list of (NAME DATE) for recently accessed projects."
  (require 'projectile)
  (let* ((projects (seq-take (projectile-relevant-known-projects)
                             nexus-dashboard-max-projects))
         (entries
          (mapcar (lambda (proj)
                    (let* ((name (file-name-nondirectory
                                  (directory-file-name proj)))
                           (attrs (condition-case nil
                                      (file-attributes proj)
                                    (error nil)))
                           (mtime (when attrs (nth 5 attrs)))
                           (date  (if mtime
                                      (format-time-string "%b %d" mtime)
                                    "unknown")))
                      (list name date proj)))
                  projects)))
    entries))

(defun nexus-dashboard--format-project (entry)
  "Format a project ENTRY (NAME DATE PATH) as a single box row string."
  (let* ((name     (car entry))
         (date     (cadr entry))
         (icon-str (propertize "  " 'face 'default))
         (name-str (concat icon-str name))
         ;; (date-str (propertize date 'face '(:slant italic)))
         (date-str  (propertize date 'face 'nexus-dashboard-date-face))
         (date-len (length date))
         (sep      "  ")
         (sep-len  (length sep))
         (inner    (- nexus-dashboard-box-width 4))
         (name-max (- inner 2 sep-len 1 date-len))
         (truncated (nexus-dashboard--truncate-title name name-max))
         ;; (name-disp (concat icon-str truncated))
         (name-disp (propertize (concat icon-str truncated) 'face 'nexus-dashboard-entry-face))
         (name-len (nexus--string-width-px name-disp))
         ;; (name-len  (+ 2 (string-width truncated)))
         (title-pad (- inner name-len sep-len 1 date-len))
         (padded-sep (propertize (concat (make-string (max 1 title-pad) ?\s) sep " ") 'face 'nexus-dashboard-date-face)))
    (concat name-disp
            (propertize padded-sep 'face '(:foreground "dim"))
            date-str)))

;;; ─── Cache Priming ────────────────────────────────────────────────────────────

(defun nexus-dashboard--prime-todo-cache ()
  "Populate todo and project caches then reload the dashboard."
  (run-with-idle-timer
   2 nil
   (lambda ()
     (setq nexus-dashboard--todo-cache
           (nexus-dashboard--fetch-todos-sync))
     (setq nexus-dashboard--project-cache
           (nexus-dashboard--fetch-projects-sync))
     (when (get-buffer +doom-dashboard-name)
       (when-let (win (get-buffer-window +doom-dashboard-name))
         (setq +doom-dashboard--width (window-body-width win)))
       (+doom-dashboard-reload)))))

;;; ─── Insert Bottom Boxes ──────────────────────────────────────────────────────

(defun nexus-dashboard--insert-bottom-boxes ()
  "Insert Recent Projects and Open Tasks boxes side by side."
  (let* ((w           nexus-dashboard-box-width)
         (gap         4)
         (total-width (+ w gap w))
         (left-margin (max 0 (/ (- (nexus--body-width) total-width) 2)))
         (margin-str  (make-string left-margin ?\s))

         ;; ── Recent Projects lines ──
         (project-lines
          (cond
           ((eq nexus-dashboard--project-cache 'not-loaded)
            (list (nexus--center "Loading…" (- w 4))))
           ((null nexus-dashboard--project-cache)
            (list (nexus--center "No projects found" (- w 4))))
           (t (mapcar #'nexus-dashboard--format-project
                      nexus-dashboard--project-cache))))

         ;; ── Open Tasks lines ──
         (todo-lines
          (cond
           ((eq nexus-dashboard--todo-cache 'not-loaded)
            (list (nexus--center "Loading…" (- w 4))))
           ((null nexus-dashboard--todo-cache)
            (list (nexus--center "No open tasks 🎉" (- w 4))))
           (t (mapcar #'nexus-dashboard--format-todo
                      nexus-dashboard--todo-cache))))

         (max-rows     (max (length project-lines) (length todo-lines)))
         (project-lines (append project-lines
                                (make-list (- max-rows (length project-lines)) "")))
         (todo-lines    (append todo-lines
                                (make-list (- max-rows (length todo-lines)) ""))))

    (insert margin-str
            (nexus--box-top w "Recent Projects")
            (make-string gap ?\s)
            (nexus--box-top w "Open Tasks")
            "\n")
    (insert margin-str
            (nexus--box-row "" w)
            (make-string gap ?\s)
            (nexus--box-row "" w)
            "\n")
    (cl-loop for left  in project-lines
             for right in todo-lines
             do (insert margin-str
                        (nexus--box-row left w)
                        (make-string gap ?\s)
                        (nexus--box-row right w)
                        "\n"))
    (insert margin-str
            (nexus--box-row "" w)
            (make-string gap ?\s)
            (nexus--box-row "" w)
            "\n")
    (insert margin-str
            (nexus--box-bottom w)
            (make-string gap ?\s)
            (nexus--box-bottom w)
            "\n")))

;;; ─── Footer Actions ───────────────────────────────────────────────────────────

(defun nexus-dashboard--insert-footer ()
  "Insert a borderless Quick Actions footer with a top separator."
  (let* ((w           nexus-dashboard-box-width)
         (gap         4)
         (total-width (+ w gap w))
         (left-margin (max 0 (/ (- (nexus--body-width) total-width) 2)))
         (margin-str  (make-string left-margin ?\s))
         (inner       (- total-width 2))
         (action-strs
          (mapcar (lambda (entry)
                    (let* ((key     (substring (car entry) 1 2))
                           (label   (cadr entry))
                           (key-str (propertize (concat "[" key "]")
                                                'face 'nexus-dashboard-footer-face))
                           (lbl-str (propertize (concat " " label)
                                                'face 'nexus-dashboard-footer-face)))
                      (concat key-str lbl-str)))
                  nexus-dashboard-quick-actions))
         (joined (mapconcat #'identity action-strs "      "))
         (rule   (propertize (make-string total-width ?─)
                             'face 'nexus-dashboard-footer-separator-face)))
    (insert "\n")
    (insert margin-str rule "\n")
    (insert margin-str (nexus--center joined inner) "\n")))

;;; ─── Key Bindings ─────────────────────────────────────────────────────────────

(defun nexus-dashboard--bind-actions ()
  "Bind quick action keys locally in the dashboard buffer."
  (dolist (entry nexus-dashboard-quick-actions)
    (let ((key (substring (car entry) 1 2))
          (cmd (caddr entry)))
      (local-set-key (kbd key) cmd))))

;;; ─── Main Dashboard Function ──────────────────────────────────────────────────

(defun nexus-dashboard ()
  "Render the NEXUS dashboard."
  (setq-local line-spacing 0)
  (nexus-dashboard--insert-logo)
  (nexus-dashboard--insert-greeting)
  (nexus-dashboard--insert-quote)
  (nexus-dashboard--insert-bottom-boxes)
  (nexus-dashboard--insert-footer)
  (insert "\n"))

;;; ─── Hook into Doom Dashboard ─────────────────────────────────────────────────

(defadvice! nexus-dashboard--no-vcenter (&rest _)
  :after '+doom-dashboard-resize-h
  (with-current-buffer (doom-fallback-buffer)
    (with-silent-modifications
      (goto-char (point-min))
      (delete-region (point) (progn (skip-chars-forward "\n") (point)))
      (insert (make-string nexus-dashboard-top-padding ?\n)))))

(add-hook 'doom-after-init-hook
          (lambda ()
            (setq +doom-dashboard-functions '(nexus-dashboard))
            (setq +doom-dashboard-banner-padding '(0 . 0))

            (add-hook '+doom-dashboard-mode-hook #'nexus-dashboard--bind-actions)

            (add-hook 'window-size-change-functions
                      (lambda (_)
                        (when (eq major-mode '+doom-dashboard-mode)
                          (setq +doom-dashboard--width (window-body-width))
                          (+doom-dashboard-reload))))

            (nexus-dashboard--prime-todo-cache)

            (run-with-idle-timer
             3 nil
             (lambda ()
               (when (get-buffer +doom-dashboard-name)
                 (+doom-dashboard-reload)))))
          100)
