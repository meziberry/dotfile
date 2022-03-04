;; autoload/project.el; -*- lexical-binding: t; -*-

;;;###autoload
(defvar radian--fd-binary
  (cl-find-if #'executable-find (list "fdfind" "fd"))
  "The filename of the `fd' executable. On some distros it's 'fdfind' (ubuntu,
debian, and derivatives). On most it's 'fd'.")

;;;###autoload
(defun radian-project-root (&optional maybe-prompt dir)
  "Return the project root of DIR (defaults to `default-directory').
Returns nil if not in a project."
  (let* ((f (bound-and-true-p leaf-this-file))
         (d (or dir (file-name-directory (or f ""))))
         (pr (project-current maybe-prompt d)))
    (if pr (directory-file-name
            (cond
             ((eq 'vc (car pr))
              (nth 2 pr))
             (t (cdr pr)))))))

;;;###autoload
(defun radian-project-name (&optional dir)
  "Return the name of the current project.

Returns '-' if not in a valid project."
  (if-let (project-root (or (radian-project-root dir)
                            (if dir (expand-file-name dir))))
      (file-name-nondirectory project-root)
    "-"))

(defun +project--subdirs-completion-table (dir)
  "Return list of subdirectories in DIR with completion table."
  (let* (lsdir
         (_ (walk-directory dir (lambda (d) (push d lsdir)) :on-directory t
              :test (lambda (x) (not (string-match-p "\\.git" x))))))
    (+vertico-completion-table 'file lsdir)))

;; DEPRECATED: use `project-find-dir' instead
(defvar +project--subdir-hist '()
  "Minibuffer history for `+project/find-subdir'.")
;;;###autoload
(defun +project/find-subdir ()
  "Find subdirectories in the current project, using completion."
  (interactive)
  (let* ((pr (project-current t))
         (dir (cdr pr))
         (subdirs (+project--subdirs-completion-table dir))
         (directory (completing-read "Select Project subdir: " subdirs
                                     nil t nil '+project--subdir-hist)))
    (dired directory)
    (add-to-history '+project--subdir-hist dir)))

(defvar +project-commit-log-limit 20
  "Limit commit logs for project to N entries by default.
A value of 0 means 'unlimited'.")

;; FIXME: the buttons at the bottom of the log for displaying more
;; commits do not seem to work with this.
;;;###autoload
(defun +project/commit-log (&optional arg)
  "Print commit log for the current project.
With optional prefix ARG (\\[universal-argument]) shows expanded
commit messages and corresponding diffs.

The log is limited to the integer specified by
`+project-commit-log-limit'.  A value of 0 means
'unlimited'."
  (interactive "P")
  (let* ((pr (project-current t))
         (dir (cdr pr))
         (default-directory dir) ; otherwise fails at spontaneous M-x calls
         (backend (vc-responsible-backend dir))
         (int +project-commit-log-limit)
         (limit (if (= int 0) t int))
         (diffs (if arg 'with-diff nil))
         (vc-log-short-style (unless diffs '(directory))))
    (vc-print-log-internal backend (list dir) nil nil limit diffs)))

;;;###autoload
(defun +project/retrieve-tag ()
  "Run `vc-retrieve-tag' on project and switch to the root dir.
Basically switches to a new branch or tag."
  (interactive)
  (let* ((pr (project-current t))
         (dir (cdr pr))
         (default-directory dir) ; otherwise fails at spontaneous M-x calls
         (name
          (vc-read-revision "Tag name: "
                            (list dir)
                            (vc-responsible-backend dir))))
    (vc-retrieve-tag dir name)
    (project-dired)))


(autoload 'magit-status "magit")
;;;###autoload
(defun +project/magit-status ()
  "Run `magit-status' on project."
  (interactive)
  (let* ((pr (project-current t))
         (dir (cdr pr)))
    (magit-status dir)))
