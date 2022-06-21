;; -*- coding: utf-8; lexical-binding: t -*-

(defvar +org-startup-with-animated-gifs nil
  "If non-nil, and the cursor is over a gif inline-image preview, animate it!")

;;
;;`org-load' hooks
(defun +org-init-org-directory-h ()
  (unless org-directory
    (setq-default org-directory "~/org"))
  (unless org-id-locations-file
    (setq org-id-locations-file (expand-file-name ".orgids" org-directory))))

(defun +org-init-appearance-h ()
  "Configures the UI for `org-mode'."
  (setq ;org-indirect-buffer-display #'current-window
   org-use-property-inheritance t   ; it's convenient to have properties inherited
   org-log-done 'time              ; having the time a item is done sounds convininet

   ;; When you create a sparse tree and `org-indent-mode' is enabled,
   ;; the highlighting destroys the invisibility added by
   ;; `org-indent-mode'. Therefore, don't highlight when creating a
   ;; sparse tree.
   org-highlight-sparse-tree-matches nil
   org-eldoc-breadcrumb-separator " → "
   ;; Show headlines but not content by default.
   org-startup-folded 'content
   ;; Make it possible to dim or hide blocked tasks in the agenda view.
   org-enforce-todo-dependencies t
   org-entities-user
   '(("flat"  "\\flat" nil "" "" "266D" "♭")
     ("sharp" "\\sharp" nil "" "" "266F" "♯"))
   org-fontify-done-headline t
   org-fontify-quote-and-verse-blocks t
   org-archive-subtree-save-file-p t
   org-fontify-whole-heading-line t
   org-hide-leading-stars t
   org-hide-emphasis-markers t
   org-pretty-entities t
   org-image-actual-width nil
   org-imenu-depth 8
   ;; Global ID state means we can have ID links anywhere. This is required for
   ;; `org-brain', however.
   org-id-locations-file-relative t
   ;; Prevent modifications made in invisible sections of an org document, as
   ;; unintended changes can easily go unseen otherwise.
   org-fold-catch-invisible-edits 'smart
   ;; Don't number headings with these tags
   org-num-face '(:inherit org-special-keyword :underline nil :weight bold)
   org-num-skip-tags '("noexport" "nonum")
   ;; Sub-lists should have different bullets
   org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+") ("1." . "a."))
   org-ellipsis " ▾ "                   ;▾▼◣ ⤵ ⤶
   org-priority-highest ?A
   org-priority-lowest ?E
   org-priority-faces
   '((?A . 'org-habit-overdue-face)
     (?B . 'org-habit-overdue-future-face)
     (?C . 'org-habit-alert-face)
     (?D . 'org-habit-ready-face)
     (?E . 'org-habit-clear-future-face))
   org-startup-indented t
   org-tags-column 0
   org-use-sub-superscripts '{})

  (setq org-refile-targets
        '((nil :maxlevel . 3)
          (org-agenda-files :maxlevel . 3))
        ;; Without this, completers like ivy/helm are only given the first level of
        ;; each outline candidates. i.e. all the candidates under the "Tasks" heading
        ;; are just "Tasks/". This is unhelpful. We want the full path to each refile
        ;; target! e.g. FILE/Tasks/heading/subheading
        org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil)

  (plist-put org-format-latex-options :scale 1.5) ; larger previews

  ;; HACK Face specs fed directly to `org-todo-keyword-faces' don't respect
  ;;      underlying faces like the `org-todo' face does, so we define our own
  ;;      intermediary faces that extend from org-todo.
  (with-no-warnings
    (custom-declare-face '+org-todo-active  '((t (:inherit (bold font-lock-constant-face org-todo)))) "")
    (custom-declare-face '+org-todo-project '((t (:inherit (bold font-lock-doc-face org-todo)))) "")
    (custom-declare-face '+org-todo-onhold  '((t (:inherit (bold warning org-todo)))) "")
    (custom-declare-face '+org-todo-cancel  '((t (:inherit (bold error org-todo)))) ""))
  (setq org-todo-keywords
        '((sequence
           "TODO(t)"  ; A task that needs doing & is ready to do
           "PROJ(p)"  ; A project, which usually contains other tasks
           "LOOP(r)"  ; A recurring task
           "STRT(s)"  ; A task that is in progress
           "WAIT(w)"  ; Something external is holding up this task
           "HOLD(h)"  ; This task is paused/on hold because of me
           "IDEA(i)"  ; An unconfirmed and unapproved task or notion
           "|"
           "DONE(d)"  ; Task successfully completed
           "KILL(k@)") ; Task was cancelled, aborted or is no longer applicable
          (sequence
           "[ ](T)"   ; A task that needs doing
           "[-](S)"   ; Task is in progress
           "[?](W)"   ; Task is being held up or paused
           "|"
           "[X](D)")  ; Task was completed
          (sequence
           "|"
           "OKAY(o)"
           "YES(y)"
           "NO(n)"))
        org-todo-keyword-faces
        '(("[-]"  . +org-todo-active)
          ("STRT" . +org-todo-active)
          ("[?]"  . +org-todo-onhold)
          ("WAIT" . +org-todo-onhold)
          ("HOLD" . +org-todo-onhold)
          ("PROJ" . +org-todo-project)
          ("NO"   . +org-todo-cancel)
          ("KILL" . +org-todo-cancel)))

  (defadvice! +org-display-link-in-eldoc-a (&rest _)
    "Display full link in minibuffer when cursor/mouse is over it."
    :before-until #'org-eldoc-documentation-function
    (when-let* ((context (org-element-context))
                (path (org-element-property :path context)))
      (pcase (org-element-property :type context)
        (_ (format "Link: %s" (org-element-property :raw-link context))))))

  (set-ligatures! 'org-mode
    :name          "#+NAME:"
    :name          "#+name:"
    :src_block     "#+BEGIN_SRC"
    :src_block     "#+begin_src"
    :src_block_end "#+END_SRC"
    :src_block_end "#+end_src"
    :quote         "#+BEGIN_QUOTE"
    :quote         "#+begin_quote"
    :quote_end     "#+END_QUOTE"
    :quote_end     "#+end_quote"
    :checkbox      "[ ]"
    :pending       "[-]"
    :checkedbox    "[X]"
    :list_property "::"
    :em_dash       "---"
    :ellipsis      "..."
    :title         "#+title:"
    :title         "#+TITLE:"
    :subtitle      "#+subtitle:"
    :subtitle      "#+SUBTITLE:"
    :author        "#+author:"
    :author        "#+AUTHOR:"
    :date          "#+date:"
    :date          "#+DATE:"
    :property      "#+property:"
    :property      "#+PROPERTY:"
    :options       "#+options:"
    :options       "#+OPTIONS:"
    :latex_class   "#+latex_class:"
    :latex_header  "#+latex_header:"
    :beamer_header "#+beamer_header:"
    :attr_latex    "#+attr_latex:"
    :attr_html     "#+attr_html:"
    :attr_html     "#+html:"
    :caption       "#+caption:"
    :header        "#+header:"
    :begin_export  "#+begin_export"
    :end_export    "#+end_export"
    :results       "#+RESULTS:"
    :properties    ":properties:"
    :properties    ":PROPERTIES:"
    :end           ":END:"
    :end           ":end:"
    :priority_a    "[#A]"
    :priority_b    "[#B]"
    :priority_c    "[#C]"
    :priority_d    "[#D]"
    :priority_e    "[#E]"))

(defun +org-init-agenda-h ()
  (unless org-agenda-files
    (setq-default org-agenda-files (list org-directory)))
  (setq-default
   ;; Different colors for different priority levels
   org-agenda-deadline-faces
   '((1.001 . error)
     (1.0 . org-warning)
     (0.5 . org-upcoming-deadline)
     (0.0 . org-upcoming-distant-deadline))
   ;; Don't monopolize the whole frame just for the agenda
   ;; org-agenda-window-setup 'current-window
   org-agenda-skip-unavailable-files t
   ;; Shift the agenda to show the previous 3 days and the next 7 days for
   ;; better context on your week. The past is less important than the future.
   org-agenda-span 10
   org-agenda-start-on-weekday nil
   org-agenda-start-day "-3d"
   ;; Optimize `org-agenda' by inhibiting extra work while opening agenda
   ;; buffers in the background. They'll be "restarted" if the user switches to
   ;; them anyway (see `+org-exclude-agenda-buffers-from-workspace-h')
   org-agenda-inhibit-startup t))

(defun +org-init-attachments-h ()
  "Sets up org's attachment system."
  (setq org-attach-store-link-p t     ; store link after attaching files
        org-attach-use-inheritance t) ; inherit properties from parent nodes

  ;; Autoload all these commands that org-attach doesn't autoload itself
  `(eval (z org-attach/m
           :commands (org-attach-new
                      org-attach-open
                      org-attach-open-in-emacs
                      org-attach-reveal-in-emacs
                      org-attach-url
                      org-attach-set-directory
                      org-attach-sync)
           :config
           (unless org-attach-id-dir
             ;; Centralized attachments directory by default
             (setq-default org-attach-id-dir (expand-file-name ".attach/" org-directory)))
           (after! grep
             (add-to-list 'grep-find-ignored-directories org-attach-id-dir))))

  ;; Add inline image previews for attachment links
  (org-link-set-parameters "attachment" :image-data-fun #'+org-inline-image-data-fn))

(defvar +org-babel-native-async-langs '(python)
  "Languages that will use `ob-comint' instead of `ob-async' for `:async'.")

(eval-when-compile
  (defvar ob-async-no-async-languages-alist)
  (defvar +org-babel-mode-alist) )

(eval-unless! "I don't use babel"
(defun +org-init-babel-h ()
  (setq org-src-preserve-indentation t  ; use native major-mode indentation
        org-src-tab-acts-natively t     ; we do this ourselves
        ;; You don't need my permission (just be careful, mkay?)
        org-confirm-babel-evaluate nil
        org-link-elisp-confirm-function nil
        ;; Show src buffer in popup, and don't monopolize the frame
        org-src-window-setup 'other-window
        ;; Our :lang common-lisp module uses sly, so...
        org-babel-lisp-eval-fn #'sly-eval)

  ;; I prefer C-c C-c over C-c ' (more consistent)
  (define-key org-src-mode-map (kbd "C-c C-c") #'org-edit-src-exit)

  ;; Don't process babel results asynchronously when exporting org, as they
  ;; won't likely complete in time, and will instead output an ob-async hash
  ;; instead of the wanted evaluation results.
  (after! ob
    (add-to-list 'org-babel-default-lob-header-args '(:sync)))

  (defadvice! +org-babel-disable-async-maybe-a (fn &optional orig-fn arg info params)
    "Use ob-comint where supported, disable async altogether where it isn't.

We have access to two async backends: ob-comint or ob-async, which have
different requirements. This advice tries to pick the best option between them,
falling back to synchronous execution otherwise. Without this advice, they die
with an error; terrible UX!

Note: ob-comint support will only kick in for languages listed in
`+org-babel-native-async-langs'.

Also adds support for a `:sync' parameter to override `:async'."
    :around #'ob-async-org-babel-execute-src-block
    (if (null orig-fn)
        (funcall fn orig-fn arg info params)
      (let* ((info (or info (org-babel-get-src-block-info)))
             (params (org-babel-merge-params (nth 2 info) params)))
        (if (or (assq :sync params)
                (not (assq :async params))
                (member (car info) ob-async-no-async-languages-alist)
                ;; ob-comint requires a :session, ob-async does not, so fall
                ;; back to ob-async if no :session is provided.
                (unless (member (alist-get :session params) '("none" nil))
                  (unless (memq (let* ((lang (nth 0 info))
                                       (lang (cond ((symbolp lang) lang)
                                                   ((stringp lang) (intern lang)))))
                                  (or (alist-get lang +org-babel-mode-alist)
                                      lang))
                                +org-babel-native-async-langs)
                    (message "Org babel: %s :session is incompatible with :async. Executing synchronously!"
                             (car info))
                    (sleep-for 0.2))
                  t))
            (funcall orig-fn arg info params)
          (funcall fn orig-fn arg info params)))))

  ;; HACK Fix #6061. Seems `org-babel-do-in-edit-buffer' has the side effect of
  ;;   deleting side windows. Should be reported upstream! This advice
  ;;   suppresses this behavior wherever it is known to be used.
  (defadvice! +org-fix-window-excursions-a (fn &rest args)
    "Suppress changes to the window config anywhere
`org-babel-do-in-edit-buffer' is used."
    :around #'org-indent-region
    :around #'org-indent-line
    (save-window-excursion (apply fn args)))

  (defadvice! +org-fix-newline-and-indent-in-src-blocks-a (&optional indent _arg _interactive)
    "Mimic `newline-and-indent' in src blocks w/ lang-appropriate indentation."
    :after #'org-return
    (when (and indent
               org-src-tab-acts-natively
               (org-in-src-block-p t))
      (save-window-excursion
        (org-babel-do-in-edit-buffer
         (call-interactively #'indent-for-tab-command)))))

  (defadvice! +org-inhibit-mode-hooks-a (fn datum name &optional initialize &rest args)
    "Prevent potentially expensive mode hooks in `org-babel-do-in-edit-buffer' ops."
    :around #'org-src--edit-element
    (apply fn datum name
           (if (and (eq org-src-window-setup 'switch-invisibly)
                    (functionp initialize))
               ;; org-babel-do-in-edit-buffer is used to execute quick, one-off
               ;; logic in the context of another major mode, but initializing a
               ;; major mode with expensive hooks can be terribly expensive.
               ;; Since Radian adds its most expensive hooks to
               ;; MAJOR-MODE-local-vars-hook, we can savely inhibit those.
               (lambda ()
                 (let ((radian-inhibit-major-mode-post-hooks t))
                   (funcall initialize)))
             initialize)
           args))

  ;; Refresh inline images after executing src blocks (useful for plantuml or
  ;; ipython, where the result could be an image)
  (add-hook! 'org-babel-after-execute-hook
    (defun +org-redisplay-inline-images-in-babel-result-h ()
      (unless (or
               ;; ...but not while Emacs is exporting an org buffer (where
               ;; `org-display-inline-images' can be awfully slow).
               (bound-and-true-p org-export-current-backend)
               ;; ...and not while tangling org buffers (which happens in a temp
               ;; buffer where `buffer-file-name' is nil).
               (string-match-p "^ \\*temp" (buffer-name)))
        (save-excursion
          (when-let ((beg (org-babel-where-is-src-block-result))
                     (end (progn (goto-char beg) (forward-line) (org-babel-result-end))))
            (org-display-inline-images nil nil (min beg end) (max beg end)))))))

  (after! python
    (setq org-babel-python-command python-shell-interpreter))

  (after! ob-ditaa
    ;; TODO Should be fixed upstream
    (let ((default-directory (org-find-library-dir "org-contribdir")))
      (setq org-ditaa-jar-path     (expand-file-name "scripts/ditaa.jar")
            org-ditaa-eps-jar-path (expand-file-name "scripts/DitaaEps.jar")))))

 (defvar +org-babel-load-functions ()
   "A list of functions executed to load the current executing src block. They
take one argument (the language specified in the src block, as a string). Stops
at the first function to return non-nil.")

 (defun +org-init-babel-lazy-loader-h ()
   "Load babel libraries lazily when babel blocks are executed."
   (defun +org--babel-lazy-load (lang &optional async)
     (cl-check-type lang (or symbol null))
     (unless (cdr (assq lang org-babel-load-languages))
       (when async
         ;; ob-async has its own agenda for lazy loading packages (in the child
         ;; process), so we only need to make sure it's loaded.
         (require 'ob-async nil t))
       (prog1 (or (run-hook-with-args-until-success '+org-babel-load-functions lang)
                  (require (intern (format "ob-%s" lang)) nil t)
                  (require lang nil t))
         (add-to-list 'org-babel-load-languages (cons lang t)))))

   (defadvice! +org--export-lazy-load-library-h (&optional element)
     "Lazy load a babel package when a block is executed during exporting."
     :before #'org-babel-exp-src-block
     (+org--babel-lazy-load-library-a (org-babel-get-src-block-info nil element)))

   (defadvice! +org--src-lazy-load-library-a (lang)
     "Lazy load a babel package to ensure syntax highlighting."
     :before #'org-src--get-lang-mode
     (or (cdr (assoc lang org-src-lang-modes))
         (+org--babel-lazy-load lang)))

   ;; This also works for tangling
   (defadvice! +org--babel-lazy-load-library-a (info)
     "Load babel libraries lazily when babel blocks are executed."
     :after-while #'org-babel-confirm-evaluate
     (let* ((lang (nth 0 info))
            (lang (cond ((symbolp lang) lang)
                        ((stringp lang) (intern lang))))
            (lang (or (cdr (assq lang +org-babel-mode-alist))
                      lang)))
       (+org--babel-lazy-load
        lang (and (not (assq :sync (nth 2 info)))
                  (assq :async (nth 2 info))))
       t))

   (advice-add #'org-babel-do-load-languages :override #'ignore)))

(defvar +org-capture-todo-file "todo.org"
  "Default target for todo entries.

Is relative to `org-directory', unless it is absolute. Is used in Radian's default
`org-capture-templates'.")

(defvar +org-capture-changelog-file "changelog.org"
  "Default target for changelog entries.

Is relative to `org-directory' unless it is absolute. Is used in Radian's default
`org-capture-templates'.")

(defvar +org-capture-notes-file "notes.org"
  "Default target for storing notes.

Used as a fall back file for org-capture.el, for templates that do not specify a
target file.

Is relative to `org-directory', unless it is absolute. Is used in Radian's default
`org-capture-templates'.")

(defvar +org-capture-journal-file "journal.org"
  "Default target for storing timestamped journal entries.

Is relative to `org-directory', unless it is absolute. Is used in Radian's default
`org-capture-templates'.")

(defvar +org-capture-projects-file "projects.org"
  "Default, centralized target for org-capture templates.")

(defun +org-init-capture-defaults-h ()
  "Sets up some reasonable defaults, as well as two `org-capture' workflows that
I like:

1. The traditional way: invoking `org-capture' directly, via SPC X, or through
   the :cap ex command.
2. Through a org-capture popup frame that is invoked from outside Emacs (the
   ~/.emacs.d/bin/org-capture script). This can be invoked from qutebrowser,
   vimperator, dmenu or a global keybinding."
  (setq org-default-notes-file
        (expand-file-name +org-capture-notes-file org-directory)
        +org-capture-journal-file
        (expand-file-name +org-capture-journal-file org-directory)
        org-capture-templates
        '(("t" "Personal todo" entry
           (file+headline +org-capture-todo-file "Inbox")
           "* [ ] %?\n%i\n%a" :prepend t)
          ("n" "Personal notes" entry
           (file+headline +org-capture-notes-file "Inbox")
           "* %u %?\n%i\n%a" :prepend t)
          ("j" "Journal" entry
           (file+olp+datetree +org-capture-journal-file)
           "* %U %?\n%i\n%a" :prepend t)

          ;; Will use {project-root}/{todo,notes,changelog}.org, unless a
          ;; {todo,notes,changelog}.org file is found in a parent directory.
          ;; Uses the basename from `+org-capture-todo-file',
          ;; `+org-capture-changelog-file' and `+org-capture-notes-file'.
          ("p" "Templates for projects")
          ("pt" "Project-local todo" entry  ; {project-root}/todo.org
           (file+headline +org-capture-project-todo-file "Inbox")
           "* TODO %?\n%i\n%a" :prepend t)
          ("pn" "Project-local notes" entry  ; {project-root}/notes.org
           (file+headline +org-capture-project-notes-file "Inbox")
           "* %U %?\n%i\n%a" :prepend t)
          ("pc" "Project-local changelog" entry  ; {project-root}/changelog.org
           (file+headline +org-capture-project-changelog-file "Unreleased")
           "* %U %?\n%i\n%a" :prepend t)

          ;; Will use {org-directory}/{+org-capture-projects-file} and store
          ;; these under {ProjectName}/{Tasks,Notes,Changelog} headings. They
          ;; support `:parents' to specify what headings to put them under, e.g.
          ;; :parents ("Projects")
          ("o" "Centralized templates for projects")
          ("ot" "Project todo" entry
           (function +org-capture-central-project-todo-file)
           "* TODO %?\n %i\n %a"
           :heading "Tasks"
           :prepend nil)
          ("on" "Project notes" entry
           (function +org-capture-central-project-notes-file)
           "* %U %?\n %i\n %a"
           :heading "Notes"
           :prepend t)
          ("oc" "Project changelog" entry
           (function +org-capture-central-project-changelog-file)
           "* %U %?\n %i\n %a"
           :heading "Changelog"
           :prepend t)))

  ;; Kill capture buffers by default (unless they've been visited)
  (after! org-capture
    (org-capture-put :kill-buffer t))

  ;; Fix #462: when refiling from org-capture, Emacs prompts to kill the
  ;; underlying, modified buffer. This fixes that.
  (add-hook 'org-after-refile-insert-hook #'save-buffer)

  ;; HACK Radian doesn't support `customize'. Best not to advertise it as an
  ;;      option in `org-capture's menu.
  (defadvice! +org--remove-customize-option-a (fn table title &optional prompt specials)
    :around #'org-mks
    (ignore specials)
    (let ((keyboard-translate-table (make-char-table 'keyboard-translate-table)))
      (aset keyboard-translate-table ?\q ?\^g)
      (funcall fn table title prompt '(("q" "Abort")))))

  (defadvice! +org--capture-expand-variable-file-a (file)
    "If a variable is used for a file path in `org-capture-template', it is used
as is, and expanded relative to `default-directory'. This changes it to be
relative to `org-directory', unless it is an absolute path."
    :filter-args #'org-capture-expand-file
    (if (and (symbolp file) (boundp file))
        (expand-file-name (symbol-value file) org-directory)
      file))

  (add-hook! 'org-capture-mode-hook
    (defun +org-show-target-in-capture-header-h ()
      (setq header-line-format
            (format "%s%s%s"
                    (propertize (abbreviate-file-name (buffer-file-name (buffer-base-buffer)))
                                'face 'font-lock-string-face)
                    org-eldoc-breadcrumb-separator
                    header-line-format)))))

(defun +org-init-capture-frame-h ()
  (add-hook 'org-capture-after-finalize-hook #'+org-capture-cleanup-frame-h)

  (defadvice! +org-capture-refile-cleanup-frame-a (&rest _)
    :after #'org-capture-refile
    (+org-capture-cleanup-frame-h)))

(defun +org-init-custom-links-h ()
  ;; Modify default file: links to colorize broken file links red
  (org-link-set-parameters
   "file"
   :face (lambda (path)
           (if (or (file-remote-p path)
                   ;; filter out network shares on windows (slow)
                   (and *WINDOWS (string-prefix-p "\\\\" path))
                   (file-exists-p path))
               'org-link
             '(warning org-link))))

  ;; Additional custom links for convenience
  (pushnew! org-link-abbrev-alist
            '("github"      . "https://github.com/%s")
            '("youtube"     . "https://youtube.com/watch?v=%s")
            '("google"      . "https://google.com/search?q=")
            '("gimages"     . "https://google.com/images?q=%s")
            '("gmap"        . "https://maps.google.com/maps?q=%s")
            '("duckduckgo"  . "https://duckduckgo.com/?q=%s")
            '("wikipedia"   . "https://en.wikipedia.org/wiki/%s")
            '("wolfram"     . "https://wolframalpha.com/input/?i=%s"))

  (+org-define-basic-link "org" 'org-directory)
  (+org-define-basic-link ".emacs" '*emacsd/*)
  (+org-define-basic-link "rcontrib" '*radian-contrib/*)
  (+org-define-basic-link "rlisp" '*radian-lisp/*)

  (defadvice! +org--follow-search-string-a (fn link &optional arg)
    "Support ::SEARCH syntax for id: links."
    :around #'org-id-open
    :around #'org-roam-id-open
    (save-match-data
      (cl-destructuring-bind (id &optional search)
          (split-string link "::")
        (prog1 (funcall fn id arg)
          (cond ((null search))
                ((string-match-p "\\`[0-9]+\\'" search)
                 ;; Move N lines after the ID (in case it's a heading), instead
                 ;; of the start of the buffer.
                 (forward-line (string-to-number option)))
                ((string-match "^/\\([^/]+\\)/$" search)
                 (let ((match (match-string 1 search)))
                   (save-excursion (org-link-search search))
                   ;; `org-link-search' only reveals matches. Moving the point
                   ;; to the first match after point is a sensible change.
                   (when (re-search-forward match)
                     (goto-char (match-beginning 0)))))
                ((org-link-search search)))))))

  ;; Allow inline image previews of http(s)? urls or data uris.
  ;; `+org-http-image-data-fn' will respect `org-display-remote-inline-images'.
  (setq org-display-remote-inline-images 'download) ; TRAMP urls
  (org-link-set-parameters "http"  :image-data-fun #'+org-http-image-data-fn)
  (org-link-set-parameters "https" :image-data-fun #'+org-http-image-data-fn)
  (org-link-set-parameters "img"   :image-data-fun #'+org-inline-image-data-fn)

  ;; Add support for youtube links + previews
  (require 'org-yt nil t)

  (defadvice! +org-dont-preview-if-disabled-a (&rest _)
    "Make `org-yt' respect `org-display-remote-inline-images'."
    :before-while #'org-yt-image-data-fun
    (not (eq org-display-remote-inline-images 'skip))))

(defun +org-init-export-h ()
  "TODO"
  (setq org-export-with-smart-quotes t
        org-html-validation-link nil
        org-export-in-background t       ; run export processes in external emacs process
        org-latex-prefer-user-labels t)

  `(eval
    (z ox-pandoc/m
      :when (executable-find "pandoc")
      :after ox
      :init
      (add-to-list 'org-export-backends 'pandoc)
      (setq org-pandoc-options
            '((standalone . t)
              (mathjax . t)
              (variable . "revealjs-url=https://revealjs.com")))))

  (defadvice! +org--dont-trigger-save-hooks-a (fn &rest args)
    "Exporting and tangling trigger save hooks; inadvertantly triggering
mutating hooks on exported output, like formatters."
    :around '(org-export-to-file org-babel-tangle)
    (let (before-save-hook after-save-hook)
      (apply fn args)))

  (defadvice! +org--fix-async-export-a (fn &rest args)
    :around '(org-export-to-file org-export-as)
    (let ((old-async-init-file org-export-async-init-file)
          (org-export-async-init-file (make-temp-file "radian-org-async-export")))
      (with-temp-file org-export-async-init-file
        (prin1 `(progn (setq org-export-async-debug
                             ,(or org-export-async-debug
                                  debug-on-error)
                             load-path ',load-path)
                       (unwind-protect
                           (load ,(or old-async-init-file user-init-file)
                                 nil t)
                         (delete-file load-file-name)))
               (current-buffer)))
      (apply fn args))))

(defvar +org-habit-graph-padding 2
  "The padding added to the end of the consistency graph")

(defvar +org-habit-min-width 30
  "Hides the consistency graph if the `org-habit-graph-column' is less than this value")

(defvar +org-habit-graph-window-ratio 0.3
  "The ratio of the consistency graphs relative to the window width")

(eval-when-compile (require 'org-habit))
(defun +org-init-habit-h ()
  (add-hook! 'org-agenda-mode-hook
    (defun +org-habit-resize-graph-h ()
      "Right align and resize the consistency graphs based on
`+org-habit-graph-window-ratio'"
      (when (featurep 'org-habit)
        (let* ((total-days (float (+ org-habit-preceding-days org-habit-following-days)))
               (preceding-days-ratio (/ org-habit-preceding-days total-days))
               (graph-width (floor (* (window-width) +org-habit-graph-window-ratio)))
               (preceding-days (floor (* graph-width preceding-days-ratio)))
               (following-days (- graph-width preceding-days))
               (graph-column (- (window-width) (+ preceding-days following-days)))
               (graph-column-adjusted (if (> graph-column +org-habit-min-width)
                                          (- graph-column +org-habit-graph-padding)
                                        nil)))
          (setq-local org-habit-preceding-days preceding-days)
          (setq-local org-habit-following-days following-days)
          (setq-local org-habit-graph-column graph-column-adjusted))))))

(defun +org-init-hacks-h ()
  "Getting org to behave."
  ;; Open file links in current window, rather than new ones
  (setf (alist-get 'file org-link-frame-setup) #'find-file)
  ;; Open directory links in dired
  (add-to-list 'org-file-apps '(directory . emacs))
  (add-to-list 'org-file-apps '(remote . emacs))

  ;; Open help:* links with helpful-* instead of describe-*
  (after! helpful (advice-add #'org-link--open-help :around #'radian-use-helpful-a))

  ;; Unlike the stock showNlevels options, these will also show the parents of
  ;; the target level, recursively.
  (pushnew! org-startup-options
            '("show2levels*" org-startup-folded show2levels*)
            '("show3levels*" org-startup-folded show3levels*)
            '("show4levels*" org-startup-folded show4levels*)
            '("show5levels*" org-startup-folded show5levels*))

  (defadvice! +org--more-startup-folded-options-a ()
    "Adds support for 'showNlevels*' startup options.
Unlike showNlevels, this will also unfold parent trees."
    :before #'org-set-startup-visibility
    (when-let (n (pcase org-startup-folded
                   (`show2levels* 2)
                   (`show3levels* 3)
                   (`show4levels* 4)
                   (`show5levels* 5)))
      (org-show-all '(headings drawers))
      (save-excursion
        (goto-char (point-max))
        (let ((regexp (if (and (wholenump n) (> n 0))
                          (format "^\\*\\{%d,%d\\} " (1- n) n)
                        "^\\*+ "))
              (last (point)))
          (while (re-search-backward regexp nil t)
            (when (or (not (wholenump n))
                      (= (org-current-level) n))
              (org-flag-region (line-end-position) last t 'outline))
            (setq last (line-end-position 0)))))))

  ;; Some uses of `org-fix-tags-on-the-fly' occur without a check on
  ;; `org-auto-align-tags', such as in `org-self-insert-command' and
  ;; `org-delete-backward-char'.
  ;; TODO Should be reported/PR'ed upstream
  (defadvice! +org--respect-org-auto-align-tags-a (&rest _)
    :before-while #'org-fix-tags-on-the-fly
    org-auto-align-tags)

  (defadvice! +org--recenter-after-follow-link-a (&rest _args)
    "Recenter after following a link, but only internal or file links."
    :after '(org-footnote-action
             org-follow-timestamp-link
             org-link-open-as-file
             org-link-search)
    (when (get-buffer-window)
      (recenter)))

  (defadvice! +org--strip-properties-from-outline-a (fn &rest args)
    "Fix variable height faces in eldoc breadcrumbs."
    :around #'org-format-outline-path
    (let ((org-level-faces
           (cl-loop for face in org-level-faces
                    collect `(:foreground ,(face-foreground face nil t)
                                          :weight bold))))
      (apply fn args)))

  (after! org-eldoc
    ;; HACK Fix #2972: infinite recursion when eldoc kicks in in 'org' or
    ;;      'python' src blocks.
    ;; TODO Should be reported upstream!
    (puthash "org" #'ignore org-eldoc-local-functions-cache)
    (puthash "plantuml" #'ignore org-eldoc-local-functions-cache)
    (puthash "python" #'python-eldoc-function org-eldoc-local-functions-cache))

  (defun +org--restart-mode-h ()
    "Restart `org-mode', but only once."
    (quiet! (org-mode-restart))
    (delq! (current-buffer) org-agenda-new-buffers)
    (remove-hook 'radian-switch-buffer-hook #'+org--restart-mode-h
                 'local)
    (run-hooks 'find-file-hook))

  (add-hook! 'org-agenda-finalize-hook
    (defun +org-defer-mode-in-agenda-buffers-h ()
      "`org-agenda' opens temporary, incomplete org-mode buffers.
I've disabled a lot of org-mode's startup processes for these invisible buffers
to speed them up (in `+org--exclude-agenda-buffers-from-recentf-a'). However, if
the user tries to visit one of these buffers they'll see a gimped, half-broken
org buffer. To avoid that, restart `org-mode' when they're switched to so they
can grow up to be fully-fledged org-mode buffers."
      (dolist (buffer org-agenda-new-buffers)
        (when (buffer-live-p buffer)      ; Ensure buffer is not killed
          (with-current-buffer buffer
            (add-hook 'radian-switch-buffer-hook #'+org--restart-mode-h
                      nil 'local))))))

  (defvar recentf-exclude)
  (defadvice! +org--optimize-backgrounded-agenda-buffers-a (fn file)
    "Prevent temporarily opened agenda buffers from polluting recentf."
    :around #'org-get-agenda-file-buffer
    (let ((recentf-exclude (list (lambda (_file) t)))
          org-startup-indented
          org-startup-folded
          vc-handled-backends
          org-mode-hook
          find-file-hook)
      (funcall fn file)))

  ;; HACK With https://code.orgmode.org/bzg/org-mode/commit/48da60f4, inline
  ;;      image previews broke for users with imagemagick support built in. This
  ;;      reverses the problem, but should be removed once it is addressed
  ;;      upstream (if ever).
  (defadvice! +org--fix-inline-images-for-imagemagick-users-a (fn &rest args)
    :around #'org-display-inline-images
    (letf! (defun create-image (file-or-data &optional type data-p &rest props)
             (let ((type (if (plist-get props :width) type)))
               (apply create-image file-or-data type data-p props)))
      (apply fn args)))

  (defadvice! +org--fix-inconsistent-uuidgen-case-a (uuid)
    "Ensure uuidgen always produces lowercase output regardless of system."
    :filter-return #'org-id-new
    (if (eq org-id-method 'uuid)
        (downcase uuid)
      uuid)))

(defun +org-init-keybinds-h ()
  "Sets up org-mode keybindings. Tries to fix the idiosyncrasies between the two."
  (add-hook 'radian-escape-hook #'+org-remove-occur-highlights-h)

  ;; C-a & C-e act like `radian/backward-to-bol-or-indent' and
  ;; `radian/forward-to-last-non-comment-or-eol', but with more org awareness.
  (setq org-special-ctrl-a/e t)
  (setq org-special-ctrl-k t)

  (setq org-M-RET-may-split-line nil
        ;; insert new headings after current subtree rather than inside it
        org-insert-heading-respect-content t)

  (add-hook! 'org-tab-first-hook
             #'+org-yas-expand-maybe-h
             #'+org-indent-maybe-h)

  (add-hook 'radian-delete-backward-functions
            #'+org-delete-backward-char-and-realign-table-maybe-h)

  (-keys ((org-mode-map
           ;; Recently, a [tab] keybind in `outline-mode-cycle-map' has begun
           ;; overriding org's [tab] keybind in GUI Emacs. This is needed to undo
           ;; that, and should probably be PRed to org.
           ([tab]        . org-cycle)
           ("C-c C-S-l"  . +org/remove-link)
           ("C-c C-i"    . org-toggle-inline-images)
           ;; textmate-esque newline insertion
           ("S-RET"      . +org/shift-return)
           ([S-return]   . +org/shift-return)
           ("C-M-RET"    . +org/insert-item-below)
           ("C-S-RET"    . +org/insert-item-above)
           ([C-M-return] . +org/insert-item-below)
           ([C-S-return] . +org/insert-item-above)
           ;; more intuitive RET keybinds
           ([return]     . (cmds! (meow-normal-mode-p) #'+org/dwim-at-point #'+org/return))
           ("RET"        . (cmds! (meow-normal-mode-p) #'+org/dwim-at-point #'+org/return))
           ;; ("G"          . (cmd! (org-clock-goto 'select)))
           ;; ("l"          . +org/toggle-last-clock)

           ;; ;; sensible vim-esque folding keybinds
           ;; ("za"  . +org/toggle-fold)
           ;; ("zA"  . org-shifttab)
           ;; ("zc" . +org/close-fold)
           ;; ("zC"  . outline-hide-subtree)
           ;; ("zm"  . +org/hide-next-fold-level)
           ;; ("zM"  . +org/close-all-folds)
           ;; ("zn"  . org-tree-to-indirect-buffer)
           ;; ("zo" . +org/open-fold)
           ;; ("zO"  . outline-show-subtree)
           ;; ("zr"  . +org/show-next-fold-level)
           ;; ("zR" . +org/open-all-folds)
           ;; Org-aware C-a/C-e
           ("C-a"        . org-beginning-of-line)
           ("C-e"        . org-end-of-line))

          (org-read-date-minibuffer-local-map
           ("C-<left>"    . (cmd! (org-eval-in-calendar '(calendar-backward-day 1))))
           ("C-<right>"   . (cmd! (org-eval-in-calendar '(calendar-forward-day 1))))
           ("C-<up>"      . (cmd! (org-eval-in-calendar '(calendar-backward-week 1))))
           ("C-<down>"    . (cmd! (org-eval-in-calendar '(calendar-forward-week 1))))
           ("C-S-<right>" . (cmd! (org-eval-in-calendar '(calendar-backward-month 1))))
           ("C-S-<left>"  . (cmd! (org-eval-in-calendar '(calendar-forward-month 1))))
           ("C-S-<up>"    . (cmd! (org-eval-in-calendar '(calendar-backward-year 1))))
           ("C-S-<down>"  . (cmd! (org-eval-in-calendar '(calendar-forward-year 1))))))))

(defun +org-init-popup-rules-h ()
  (set-popup-rules!
   '(("^\\*Org Links" :slot -1 :vslot -1 :size 2 :ttl 0)
     ("^ ?\\*\\(?:Agenda Com\\|Calendar\\|Org Export Dispatcher\\)"
      :slot -1 :vslot -1 :size #'+popup-shrink-to-fit :ttl 0)
     ("^\\*Org \\(?:Select\\|Attach\\)" :slot -1 :vslot -2 :ttl 0 :size 0.25)
     ("^\\*Org Agenda"     :ignore t)
     ("^\\*Org Src"        :size 0.42  :quit nil :select t :autosave t :modeline t :ttl nil)
     ("^\\*Org-Babel")
     ("^\\*Capture\\*$\\|CAPTURE-.*$" :size 0.42 :quit nil :select t :autosave ignore))))

(defun +org-init-smartparens-h ()
  ;; Disable the slow defaults
  (provide 'smartparens-org))

(eval-unless! "NOT USE"
(defun +org-init-protocol-lazy-loader-h ()
   "Brings lazy-loaded support for org-protocol, so external programs (like
browsers) can invoke specialized behavior from Emacs. Normally you'd simply
require `org-protocol' and use it, but the package loads all of org for no
compelling reason, so..."
   (defadvice! +org--server-visit-files-a (args)
     "Advise `server-visit-flist' to invoke `org-protocol' lazily."
     :filter-args #'server-visit-files
     (cl-destructuring-bind (files proc &optional nowait) args
       (catch 'greedy
         (let ((flist (reverse files)))
           (dolist (var flist)
             (when (string-match-p ":/+" (car var))
               (require 'server)
               (require 'org-protocol)
               ;; `\' to `/' on windows
               (let ((fname (org-protocol-check-filename-for-protocol
                             (expand-file-name (car var))
                             (member var flist)
                             proc)))
                 (cond ((eq fname t) ; greedy? We need the t return value.
                        (setq files nil)
                        (throw 'greedy t))
                       ((stringp fname) ; probably filename
                        (setcar var fname))
                       ((setq files (delq var files)))))))))
       (list files proc nowait)))

   ;; Disable built-in, clumsy advice
   (after! org-protocol
     (ad-disable-advice 'server-visit-files 'before 'org-protocol-detect-protocol-server))))

;; Set to nil so we can detect user changes to them later (and fall back on
;; defaults otherwise).
(defvar org-directory nil)
(defvar org-id-locations-file nil)
(defvar org-attach-id-dir nil)

(setq org-publish-timestamp-directory (concat *cache/* "org-timestamps/")
      org-preview-latex-image-directory (concat *cache/* "org-latex/")
      ;; Recognize a), A), a., A., etc -- must be set before org is loaded.
      org-list-allow-alphabetical t)

;; Make most of the default modules opt-in to lighten its first-time load
;; delay. I sincerely doubt most users use them all.
(defvar org-modules
  '(;; ol-w3m
    ;; ol-bbdb
    ol-bibtex
    ;; ol-docview
    ;; ol-gnus
    ;; ol-info
    ;; ol-irc
    ;; ol-mhe
    ;; ol-rmail
    ;; ol-eww
    ))

;; Add our general hooks after the submodules, so that any hooks the
;; submodules add run after them, and can overwrite any defaults if necessary.
(add-hook! 'org-mode-hook
           ;; `show-paren-mode' causes flickering with indent overlays made by
           ;; `org-indent-mode', so we turn off show-paren-mode altogether
           #'radian-disable-show-paren-mode-h
           ;; disable `show-trailing-whitespace'; shows a lot of false positives
           #'radian-disable-show-trailing-whitespace-h
           #'+org-enable-auto-reformat-tables-h
           #'+org-enable-auto-update-cookies-h
           #'+org-make-last-point-visible-h)

(with-eval-after-load 'org
  (+org-init-org-directory-h)
  (+org-init-appearance-h)
  (+org-init-agenda-h)
  (+org-init-attachments-h)
  ;; (+org-init-babel-h)
  ;; (+org-init-babel-lazy-loader-h)
  (+org-init-capture-defaults-h)
  (+org-init-capture-frame-h)
  (+org-init-custom-links-h)
  (+org-init-export-h)
  (+org-init-habit-h)
  (+org-init-hacks-h)
  (+org-init-keybinds-h)
  (+org-init-popup-rules-h)
  (+org-init-smartparens-h))


;; Wait until an org-protocol link is opened via emacsclient to load
;; `org-protocol'. Normally you'd simply require `org-protocol' and use it,
;; but the package loads all of org for no compelling reason, so...
(defadvice! +org--server-visit-files-a (fn files &rest args)
  "Advise `server-visit-files' to load `org-protocol' lazily."
  :around #'server-visit-files
  (if (not (cl-loop for var in files
                    if (string-match-p "://" (car var))
                    return t))
      (apply fn files args)
    (require 'org-protocol)
    (apply #'org--protocol-detect-protocol-server fn files args)))
(after! org-protocol
  (advice-remove 'server-visit-files #'org--protocol-detect-protocol-server))

;; (Re)activate eldoc-mode in org-mode a little later, because it disables
;; itself if started too soon (which is the case with `global-eldoc-mode').
(add-hook 'org-mode-local-vars-hook #'eldoc-mode)

(setq-hook! 'org-mode-hook radian-lsp-disable t)
