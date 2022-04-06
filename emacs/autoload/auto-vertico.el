;;; autoload/vertico.el; -*- lexical-binding: t; -*-

;; To prevent "Defining as dynamic an already lexical var" from +vertico/embark-preview
;;;###autoload
(defvar embark-quit-after-action)

;;;###autoload
(cl-defun +vertico-file-search (&key query in all-files (recursive t) prompt args)
  "Conduct a file search using ripgrep.

:query STRING
  Determines the initial input to search for.
:in PATH
  Sets what directory to base the search out of. Defaults to the current project's root.
:recursive BOOL
  Whether or not to search files recursively from the base directory."
  (declare (indent defun))
  (unless (executable-find "rg")
    (user-error "Couldn't find ripgrep in your PATH"))
  (require 'consult)
  (setq deactivate-mark t)
  (let* ((project-root (radian-project-root))
         (directory (or in project-root))
         (consult-ripgrep-args
          (concat "rg "
                  (if all-files "-uu ")
                  (unless recursive "--maxdepth 1 ")
                  "--null --line-buffered --color=never --max-columns=1000 "
                  "--path-separator /   --smart-case --no-heading --line-number "
                  "--hidden -g !.git -g !.svn -g !.hg "
                  (mapconcat #'shell-quote-argument args " ")
                  " ."))
         (query (or query
                    (when (use-region-p)
                      (regexp-quote (radian-thing-at-point-or-region)))))
         (consult-async-split-style consult-async-split-style)
         (consult-async-split-styles-alist consult-async-split-styles-alist))
    ;; Change the split style if the initial query contains the separator.
    (when query
      (cl-destructuring-bind (&key type separator initial)
          (consult--async-split-style)
        (pcase type
          (`separator
           (replace-regexp-in-string (regexp-quote (char-to-string separator))
                                     (concat "\\" (char-to-string separator))
                                     query t t))
          (`perl
           (when (string-match-p initial query)
             (setf (alist-get 'perlalt consult-async-split-styles-alist)
                   `(:initial ,(or (cl-loop for char in (list "%" "@" "!" "&" "/" ";")
                                            unless (string-match-p char query)
                                            return char)
                                   "%")
                     :type perl)
                   consult-async-split-style 'perlalt))))))
    (consult-ripgrep directory query)))

;;;###autoload
(defun +vertico/project-search (&optional arg initial-query directory)
  "Peforms a live project search from the project root using ripgrep.
If ARG (universal argument), include all files, even hidden or compressed ones,
in the search."
  (interactive "P")
  (+vertico-file-search :query initial-query :in directory :all-files arg))

;;;###autoload
(defun +vertico/project-search-from-cwd (&optional arg initial-query)
  "Performs a live project search from the current directory.
If ARG (universal argument), include all files, even hidden or compressed ones."
  (interactive "P")
  (+vertico/project-search arg initial-query default-directory))

;;;###autoload
(defun +vertico/search-symbol-at-point ()
  (interactive)
  (consult-line (thing-at-point 'symbol)))

;;;###autoload
(defun +vertico/embark-export-write ()
  "Export the current vertico results to a writable buffer if possible.

Supports exporting consult-grep to wgrep, file to wdeired, and consult-location to occur-edit"
  (interactive)
  (require 'embark)
  (require 'wgrep)
  (let* ((edit-command
          (pcase-let ((`(,type . ,candidates)
                       (run-hook-with-args-until-success 'embark-candidate-collectors)))
            (pcase type
              ('consult-grep #'wgrep-change-to-wgrep-mode)
              ('file #'wdired-change-to-wdired-mode)
              ('consult-location #'occur-edit-mode)
              (x (user-error "embark category %S doesn't support writable export" x)))))
         (embark-after-export-hook `(,@embark-after-export-hook ,edit-command)))
    (embark-export)))

;;;###autoload
(defun +vertico/embark-preview ()
  "Previews candidate in vertico buffer, unless it's a consult command"
  (interactive)
  (unless (bound-and-true-p consult--preview-function)
    (save-selected-window
      (let ((embark-quit-after-action nil))
        (embark-dwim)))))

;;;###autoload
(defun +vertico/next-candidate-preview (&optional n)
  "Go forward N candidates and preivew"
  (interactive)
  (vertico-next (or n 1))
  (+vertico/embark-preview))

;;;###autoload
(defun +vertico/previous-candidate-preview (&optional n)
  "Go backward N candidates and preivew"
  (interactive)
  (vertico-previous (or n 1))
  (+vertico/embark-preview))

(defvar +vertico/find-file-in--history nil)
;;;###autoload
(defun +vertico/find-file-in (&optional dir initial)
  "Jump to file under DIR (recursive).
If INITIAL is non-nil, use as initial input."
  (interactive)
  (require 'consult)
  (let* ((default-directory (or dir default-directory))
         (prompt-dir (consult--directory-prompt "Find" default-directory))
         (cmd (split-string-and-unquote +vertico-consult-fd-args " ")))
    (find-file
     (consult--read
      (split-string (cdr (apply #'radian-call-process cmd)) "\n" t)
      :prompt default-directory
      :sort nil
      :initial (if initial (shell-quote-argument initial))
      :add-history (thing-at-point 'filename)
      :category 'file
      :history '(:input +vertico/find-file-in--history)))))

;;;###autoload
(defun +vertico-embark-which-key-indicator ()
  "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
  (lambda (&optional keymap targets prefix)
    (if (null keymap)
        (kill-buffer which-key--buffer)
      (which-key--show-keymap
       (if (eq (plist-get (car targets) :type) 'embark-become)
           "Become"
         (format "Act on %s '%s'%s"
                 (plist-get (car targets) :type)
                 (embark--truncate-target (plist-get (car targets) :target))
                 (if (cdr targets) "…" "")))
       (if prefix (lookup-key keymap prefix) keymap)
       nil nil t (lambda (binding)
                   (not (string-suffix-p "-argument" (cdr binding))))))))

;;;###autoload
(defun +vertico/crm-select ()
  "Toggle selection of current candidate in `consult-completing-read-multiple'.
If the candidate has been selected, move the index up by one, to
allow for quick selection of multiple subsequent candidates."
  (interactive)
  (let* ((selected-p (get-text-property 0 'consult--crm-selected (vertico--candidate)))
         (goto-idx (+ vertico--index (if selected-p 0 1))))
    (run-at-time 0 nil (cmd! (vertico--goto goto-idx) (vertico--exhibit))))
  (vertico-exit))

;;;###autoload
(defun +vertico/crm-select-keep-input ()
  "Like `+vertico/crm-select', but keeps the current minibuffer input."
  (interactive)
  (let* ((input (substring-no-properties (car vertico--input)))
         (selected-p (get-text-property 0 'consult--crm-selected (vertico--candidate)))
         (goto-idx (+ vertico--index (if selected-p 0 1))))
    (run-at-time 0 nil (cmd! (insert input) (vertico--exhibit) (vertico--goto goto-idx) (vertico--exhibit))))
  (vertico-exit))

;;;###autoload
(defun +vertico/crm-exit ()
  "Exit `consult-completing-read-multiple' session in a dwim way.
If there are no selected candidates, select the current candidate and exit.
If there are selected candidates, disregard the current candidate and exit."
  (interactive)
  (if (consult--crm-selected)
      (progn
        (when (minibuffer-contents)
          (delete-minibuffer-contents)
          (vertico--exhibit))
        (vertico--goto -1)
        (vertico-exit))
    (run-at-time 0 nil #'vertico-exit)
    (vertico-exit)))

;;;###autoload
(defun +vertico-completion-table (category candidates)
  "Pass appropriate metadata CATEGORY to completion CANDIDATES.

This is intended for bespoke functions that need to pass
completion metadata that can then be parsed by other
tools (e.g. `embark')."
  (lambda (string pred action)
    (if (eq action 'metadata)
        `(metadata (category . ,category))
      (complete-with-action action candidates string pred))))

;;;###autoload
(defun +vertico/embark-magit-status (file)
  "Run `magit-status` on repo containing the embark target."
  (interactive "GFile: ")
  (magit-status (locate-dominating-file file ".git")))

;;;###autoload
(defun +vertico--consult--fd-builder (input)
  (pcase-let* ((cmd (split-string-and-unquote +vertico-consult-fd-args))
               (`(,arg . ,opts) (consult--command-split input))
               (`(,re . ,hl) (funcall consult--regexp-compiler
                                      arg 'extended)))
    (when re
      (list :command (append cmd
                             (list (consult--join-regexps re 'extended))
                             opts)
            :highlight hl))))

(autoload #'consult--directory-prompt "consult")
;;;###autoload
(defun +vertico/consult-fd (&optional dir initial)
  (interactive "P")
  (if radian--fd-binary
      (let* ((prompt-dir (consult--directory-prompt "Fd" dir))
             (default-directory (cdr prompt-dir)))
        (find-file (consult--find (car prompt-dir) #'+vertico--consult--fd-builder initial)))
    (consult-find dir initial)))

;;;###autoload
(defun +vertico-basic-remote-try-completion (string table pred point)
  (and (vertico--remote-p string)
       (completion-basic-try-completion string table pred point)))

;;;###autoload
(defun +vertico-basic-remote-all-completions (string table pred point)
  (and (vertico--remote-p string)
       (completion-basic-all-completions string table pred point)))

;;;###autoload
(defun +vertico/resume (&optional arg)
  "Resume the last session.
If prefix ARG is provided, select which session to resume."
  (interactive "P")
  (call-interactively (if arg #'vertico-repeat-select #'vertico-repeat-last)))
