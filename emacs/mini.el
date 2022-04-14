;; init.el --- -*- lexical-binding: t -*-

;; This file wraps the primary Radian configuration (which lives in
;; radian.el) so that we don't have to wrap the entire file in various
;; `let' forms, etc. We put as much as possible in radian.el.

;; This allows us to instead load a different Emacs configuration by
;; exporting USER_EMACS_DIRECTORY to another .emacs.d directory.

(defvar radian-minimum-emacs-version "24"
  "Radian Emacs does not support any Emacs version below this.")

;; Prevent package.el from modifying this file.
(setq package-enable-at-startup nil)

;; Prevent Custom from modifying this file.
(setq custom-file (expand-file-name "etc/custom.el" user-emacs-directory))
(load custom-file 'noerror 'nomessage)

;; Disable frequency of GC during init, and after init shall be set
;; properly. Value is in bytes
(setq gc-cons-threshold most-positive-fixnum)

;; Make sure we are running a modern enough Emacs, otherwise abort
;; init.
(if (version< emacs-version radian-minimum-emacs-version)
    (error (concat "Radian Emacs requires at least Emacs %s, "
                   "but you are running Emacs %s")
           radian-minimum-emacs-version emacs-version))

(defvar *radian-directory* "~/dotfile/")

(defvar radian--finalize-init-hook nil
  "Hook run unconditionally after init, even if it fails.
Unlike `after-init-hook', this hook is run every time the
init-file is loaded, not just once.")


;;; Comp
(defconst *NATIVECOMP (if (fboundp 'native-comp-available-p) (native-comp-available-p)))

(and
 *NATIVECOMP
 (with-eval-after-load 'comp
   ;; NOTE: Some variable is defined in `init.el', load it when
   ;; native-comp-async. `defalias' will cause `native--compile-async'
   ;; to compiling the file located.
   (customize-set-variable
    'native-comp-async-env-modifier-form
    '(load (expand-file-name "init.el" user-emacs-directory) t t t))
   (customize-set-variable 'native-comp-compiler-options '("-O2" "-mtune=native"))
   (customize-set-variable 'native-comp-async-jobs-number 1)
   ;; Disable byte-compilation warnings from native-compiled
   ;; packages from being reported asynchronously into the UI.
   (customize-set-variable 'native-comp-async-report-warnings-errors nil)))

;;; Radian Variables/Hooks
(defvar radian--current-feature 'core "The feature loading")

(defconst *EMACS29+   (> emacs-major-version 28))
(defconst *EMACS28+   (> emacs-major-version 27))
(defconst *MAC        (eq system-type 'darwin))
(defconst *LINUX      (eq system-type 'gnu/linux))
(defconst *WINDOWS    (memq system-type ' (cygwin windows-nt ms-dos)))
(defconst *BSD        (or *MAC (eq system-type 'berkeley-unix)))

(defvar radian-debug-p (or (getenv-internal "DEBUG") init-file-debug)
  "If non-nil, Radian will log more.

Use `radian-debug-mode' to toggle it. The --debug-init flag and
setting the DEBUG envvar will enable this at startup.")

(defconst radian-interactive-p (not noninteractive)
  "If non-nil, Emacs is in interactive mode.")

(defvar radian-init-time nil
  "The time it took, in seconds, for Radian Emacs to initialize.")

;; Directories/files
(defconst *emacsd/* (eval-when-compile (file-truename user-emacs-directory))
  "The path to the currently loaded .emacs.d directory. Must end with a slash.")

(defconst *etc/* (concat *emacsd/* "etc/")
  "Directory for non-volatile local storage.

Use this for files that don't change much, like server binaries, external
dependencies or long-term shared data. Must end with a slash.")

(defconst *cache/* (concat *emacsd/* "cache/")
  "Directory for non-volatile local storage.

Use this for files that don't change much, like server binaries, external
dependencies or long-term shared data. Must end with a slash.")

;; Remember these variables' initial values, so we can safely reset them at a
;; later time, or consult them without fear of contamination.
(dolist (var '(exec-path load-path process-environment))
  (unless (get var 'initial-value)
    (put var 'initial-value (default-value var))))

;; Define Radian customization groups
(defgroup radian-hooks nil
  "Startup hooks for Radian Emacs."
  :group 'radian
  :link '(url-link :tag "GitHub" "https://github.com/raxod502"))

(defgroup radian nil
  "Customize your Radian Emacs experience."
  :prefix "radian-"
  :group 'emacs
  :link '(url-link :tag "GitHub" "https://github.com/raxod502"))

(defcustom radian-first-input-hook nil
  "Transient hooks run before the first user input."
  :group 'radian-hooks
  :type 'hook)
(put 'radian-first-input-hook 'permanent-local t)

(defcustom radian-first-file-hook nil
  "Transient hooks run before the first interactively opened file."
  :group 'radian-hooks
  :type 'hook)
(put 'radian-first-file-hook 'permanent-local t)

(defcustom radian-first-buffer-hook nil
  "Transient hooks run before the first interactively opened buffer."
  :group 'radian-hooks
  :type 'hook)
(put 'radian-first-buffer-hook 'permanent-local t)

(defcustom radian-after-reload-hook nil
  "A list of hooks to run after `radian/reload' has reloaded Radian."
  :group 'radian-hooks
  :type 'hook)

(defcustom radian-before-reload-hook nil
  "A list of hooks to run before `radian/reload' has reloaded Radian."
  :group 'radian-hooks
  :type 'hook)

(defcustom radian-init-ui-hook nil
  "List of hooks to run when the UI has been initialized."
  :group 'radian-hooks
  :type 'hook)

(defcustom radian-load-theme-hook nil
  "List of hooks to run when load theme"
  :group 'radian-hooks
  :type 'hook)

(defcustom radian-switch-buffer-hook nil
  "List of hooks to run when changing the current buffer"
  :group 'radian-hooks
  :type 'hook)

(defcustom radian-switch-window-hook nil
  "List of hooks to run when changing the focused window"
  :group 'radian-hooks
  :type 'hook)

(defcustom radian-switch-frame-hook nil
  "List of hooks to run when changing the focused frame"
  :group 'radian-hooks
  :type 'hook)

;;; Load some libraries.
;; Load utility libraries
(require 'cl-lib)
(require 'map)
(require 'subr-x)

;;
;;; Some tiny function
(defmacro eval-unless! (cond &optional doc &rest args)
  "Exclude exps according to COND"
  (declare (doc-string 2)
           (indent (lambda (_ s) (goto-char (elt s 1)) (current-column))))
  (unless (stringp doc) (push doc args))
  (unless (eval cond) (macroexp-progn args)))

(defun radian--resolve-hook-forms (hooks)
  "Converts a list of modes into a list of hook symbols.

If a mode is quoted, it is left as is. If the entire HOOKS list is quoted, the
list is returned as-is."
  (declare (pure t) (side-effect-free t))
  (let ((hook-list (radian-enlist (radian-unquote hooks))))
    (if (eq (car-safe hooks) 'quote)
        hook-list
      (cl-loop for hook in hook-list
               if (eq (car-safe hook) 'quote)
               collect (cadr hook)
               else collect (intern (format "%s-hook" (symbol-name hook)))))))

(defun radian--setq-hook-fns (hooks rest &optional singles)
  (unless (or singles (= 0 (% (length rest) 2)))
    (signal 'wrong-number-of-arguments (list #'cl-evenp (length rest))))
  (cl-loop with vars = (let ((args rest)
                             vars)
                         (while args
                           (push (if singles
                                     (list (pop args))
                                   (cons (pop args) (pop args)))
                                 vars))
                         (nreverse vars))
           for hook in (radian--resolve-hook-forms hooks)
           for mode = (string-remove-suffix "-hook" (symbol-name hook))
           append
           (cl-loop for (var . val) in vars
                    collect
                    (list var val hook
                          (intern (format "radian--setq-%s-for-%s-h"
                                          var mode))))))

;;
;;;; Public library

(defun radian-unquote (exp)
  "Return EXP unquoted."
  (declare (pure t) (side-effect-free t))
  (while (memq (car-safe exp) '(quote function))
    (setq exp (cadr exp)))
  exp)

(defun radian-enlist (exp)
  "Return EXP wrapped in a list, or as-is if already a list."
  (declare (pure t) (side-effect-free t))
  (if (proper-list-p exp) exp (list exp)))

(defun radian-keyword-intern (str)
  "Converts STR (a string) into a keyword (`keywordp')."
  (declare (pure t) (side-effect-free t))
  (cl-check-type str string)
  (intern (concat ":" str)))

(defun radian-keyword-name (keyword)
  "Returns the string name of KEYWORD (`keywordp') minus the leading colon."
  (declare (pure t) (side-effect-free t))
  (cl-check-type keyword keyword)
  (substring (symbol-name keyword) 1))

(defmacro radian-log (format-string &rest args)
  "Log to *Messages* if `radian-debug-p' is on.
Does not display text in echo area, but still logs to *Messages*. Accepts the
same arguments as `message'."
  `(when radian-debug-p
     (let ((inhibit-message (active-minibuffer-window)))
       (message
        ,(concat (propertize "RADIAN " 'face 'font-lock-comment-face)
                 (propertize (format "[%s] " radian--current-feature)
                             'face 'warning)
                 format-string)
        ,@args))))

(defun radian-rpartial (fn &rest args)
  "Return a partial application of FUN to right-hand ARGS.

ARGS is a list of the last N arguments to pass to FUN. The result is a new
function which does the same as FUN, except that the last N arguments are fixed
at the values with which this function was called."
  (declare (side-effect-free t))
  (lambda (&rest pre-args)
    (apply fn (append pre-args args))))

(defun radian-lookup-key (keys &rest keymaps)
  "Like `lookup-key', but search active keymaps if KEYMAP is omitted."
  (if keymaps
      (cl-some (radian-rpartial #'lookup-key keys) keymaps)
    (cl-loop for keymap
             in (append (cl-loop for alist in emulation-mode-map-alists
                                 append (mapcar #'cdr
                                                (if (symbolp alist)
                                                    (if (boundp alist) (symbol-value alist))
                                                  alist)))
                        (list (current-local-map))
                        (mapcar #'cdr minor-mode-overriding-map-alist)
                        (mapcar #'cdr minor-mode-map-alist)
                        (list (current-global-map)))
             if (keymapp keymap)
             if (lookup-key keymap keys)
             return it)))

(cl-defun walk-directory (dirname func &key on-directory (test 'identity))
  "Walk DIRNAME recursively with calling FUNC. Exclude hidden dir."
  (declare (indent defun))
  (cl-labels ((walk (path)
                (cond
                 ((file-directory-p path)
                  (when (and on-directory (funcall test path))
                    (funcall func path))
                  (dolist (d (directory-files path t "^[^\\.]"))
                    (walk d)))
                 ((and (file-exists-p path) (funcall test path))
                  (funcall func path)))))
    (walk dirname)))

;;;; Sugars

(defun dir! ()
  "Returns the directory of the emacs lisp file this macro is called from."
  (when-let (path (file!))
    (directory-file-name (file-name-directory path))))

(defun file! ()
  "Return the emacs lisp file this macro is called from."
  (cond ((bound-and-true-p byte-compile-current-file))
        (load-file-name)
        ((stringp (car-safe current-load-list))
         (car current-load-list))
        (buffer-file-name)
        ((error "Cannot get this file-path"))))

(defmacro letenv! (envvars &rest body)
  "Lexically bind ENVVARS in BODY, like `let' but for `process-environment'."
  (declare (indent 1))
  `(let ((process-environment (copy-sequence process-environment)))
     (dolist (var (list ,@(cl-loop for (var val) in envvars
                                   collect `(cons ,var ,val))))
       (setenv (car var) (cdr var)))
     ,@body))

(defmacro letf! (bindings &rest body)
  "Temporarily rebind function, macros, and advice in BODY.

Intended as syntax sugar for `cl-letf', `cl-labels', `cl-macrolet', and
temporary advice.

BINDINGS is either:

  A list of, or a single, `defun', `defun*', `defmacro', or `defadvice' forms.
  A list of (PLACE VALUE) bindings as `cl-letf*' would accept.

TYPE is one of:

  `defun' (uses `cl-letf')
  `defun*' (uses `cl-labels'; allows recursive references),
  `defmacro' (uses `cl-macrolet')
  `defadvice' (uses `defadvice!' before BODY, then `undefadvice!' after)

NAME, ARGLIST, and BODY are the same as `defun', `defun*', `defmacro', and
`defadvice!', respectively.

\(fn ((TYPE NAME ARGLIST &rest BODY) ...) BODY...)"
  (declare (indent defun))
  (setq body (macroexp-progn body))
  (when (memq (car bindings) '(defun defun* defmacro defadvice))
    (setq bindings (list bindings)))
  (dolist (binding (reverse bindings) body)
    (let ((type (car binding))
          (rest (cdr binding)))
      (setq
       body (pcase type
              (`defmacro `(cl-macrolet ((,@rest)) ,body))
              (`defadvice `(progn (defadvice! ,@rest)
                                  (unwind-protect ,body (undefadvice! ,@rest))))
              ((or `defun `defun*)
               `(cl-letf ((,(car rest) (symbol-function ',(car rest))))
                  (ignore ,(car rest))
                  ,(if (eq type 'defun*)
                       `(cl-labels ((,@rest)) ,body)
                     `(cl-letf (((symbol-function ',(car rest))
                                 (fn! ,(cadr rest) ,@(cddr rest))))
                        ,body))))
              (_
               (when (eq (car-safe type) 'function)
                 (setq type (list 'symbol-function type)))
               (list 'cl-letf (list (cons type rest)) body)))))))

(defmacro regx-quiet! (regexps &rest body)
  "Silencing any messages that match REGEXPS, execute BODY.
REGEXPS is a list of strings; if `message' would display a
message string (not including the trailing newline) matching any
element of REGEXPS, nothing happens. The REGEXPS need not match
the entire message; include ^ and $ if necessary. REGEXPS may
also be a single string."
  (declare (indent 1))
  (let ((regexps-sym (cl-gensym "regexps")))
    `(let ((,regexps-sym ,regexps))
       (when (stringp ,regexps-sym)
         (setq ,regexps-sym (list ,regexps-sym)))
       (letf! ((defun message (format &rest args)
                 (let ((str (apply #'format format args)))
                   ;; Can't use an unnamed block because during
                   ;; byte-compilation, some idiot loads `cl', which
                   ;; sticks an advice onto `dolist' that makes it
                   ;; behave like `cl-dolist' (i.e., wrap it in
                   ;; another unnamed block) and therefore breaks
                   ;; this code.
                   (cl-block done
                     (dolist (regexp ,regexps-sym)
                       (when (or (null regexp)
                                 (string-match-p regexp str))
                         (cl-return-from done)))
                     (funcall message "%s" str)))))
         ,@body))))

(defun fn-quiet! (func &rest args)
  "Invoke FUNC with ARGS, silencing all messages.
This is an `:around' advice for many different functions."
  (cl-letf (((symbol-function #'message) #'ignore))
    (apply func args)))

(defmacro quiet! (&rest forms)
  "Run FORMS without generating any output.

This silences calls to `message', `load', `write-region' and anything
that writes to `standard-output'. In interactive sessions this won't
suppress writing to *Messages*, only inhibit output in the echo area,
except that FORCE-P is no-nil."
  (declare (indent 0))
  `(if radian-debug-p
       (progn ,@forms)
     ,(if radian-interactive-p
          `(let ((inhibit-message t)
                 (save-silently t))
             (prog1 ,@forms (message "")))
        `(letf! ((standard-output (lambda (&rest _)))
                 (defun message (&rest _))
                 (defun load (file &optional noerror nomessage nosuffix must-suffix)
                   (ignore nomessage)
                   (funcall load file noerror t nosuffix must-suffix))
                 (defun write-region (start end filename &optional append visit lockname mustbenew)
                   (unless visit (setq visit 'no-message))
                   (funcall write-region start end filename append visit lockname mustbenew)))
           ,@forms))))

(defmacro eval-if! (cond then &rest body)
  "Expands to THEN if COND is non-nil, to BODY otherwise.
COND is checked at compile/expansion time, allowing BODY to be omitted entirely
when the elisp is byte-compiled. Use this for forms that contain expensive
macros that could safely be removed at compile time."
  (declare (indent 2))
  (if (eval cond) then (macroexp-progn body)))

(defmacro eval-when! (cond &rest body)
  "Expands to BODY if CONDITION is non-nil at compile/expansion time.
See `eval-if!' for details on this macro's purpose."
  (declare (indent 1))
  (when (eval cond) (macroexp-progn body)))

(defmacro eval-cond! (&rest args)
  "ARGS is a list of (COND . BODY)
Expands to BODY if COND is non-nil at compile/expansion time.
See `eval-cond!' for details on this macro's purpose."
  (declare (indent 0))
  (if (consp args)
      (let* ((clause (car args))
             (rest (cdr args))
             (cond (car clause))
             (body (cdr clause)))
        (if (and (consp clause) (eval cond))
            (macroexp-progn body)
          `(eval-cond! ,@rest)))))

;;;; Closure factories
(defmacro fn! (arglist &rest body)
  "Returns (cl-function (lambda ARGLIST BODY...))
The closure is wrapped in `cl-function', meaning ARGLIST will accept anything
`cl-defun' will. Implicitly adds `&allow-other-keys' if `&key' is present in
ARGLIST."
  (declare (indent defun) (doc-string 1) (pure t) (side-effect-free t))
  ;; Don't complain about undeclared keys.
  `(cl-function
    (lambda
      ,(letf! (defun* allow-other-keys (args)
                (mapcar
                 (lambda (arg)
                   (cond ((nlistp (cdr-safe arg)) arg)
                         ((listp arg) (allow-other-keys arg))
                         (arg)))
                 (if (and (memq '&key args)
                          (not (memq '&allow-other-keys args)))
                     (if (memq '&aux args)
                         (let (newargs arg)
                           (while args
                             (setq arg (pop args))
                             (when (eq arg '&aux)
                               (push '&allow-other-keys newargs))
                             (push arg newargs))
                           (nreverse newargs))
                       (append args (list '&allow-other-keys)))
                   args)))
         (allow-other-keys arglist))
      ,@body)))

(defmacro cmd! (&rest body)
  "Returns (lambda () (interactive) ,@body)
A factory for quickly producing interaction commands, particularly for keybinds
or aliases."
  (declare (doc-string 1) (pure t) (side-effect-free t))
  (let ((docstring (if (stringp (car body)) (pop body) "")))
    `(lambda (&rest _) ,docstring (interactive) ,@body)))

(defmacro cmd!! (command &optional prefix &rest args)
  "Returns a closure that interactively calls COMMAND with ARGS and PREFIX-ARG.
Like `cmd!', but allows you to change `current-prefix-arg' or pass arguments to
COMMAND. This macro is meant to be used as a target for keybinds (e.g. with
`define-key' or `map!')."
  (declare (doc-string 1) (pure t) (side-effect-free t))
  (let ((docstring (if (stringp (car args)) (pop args))))
    `(lambda (arg &rest _) ,docstring (interactive "P")
       (let ((current-prefix-arg (or ,prefix arg)))
         (,(if args
               'funcall-interactively
             'call-interactively)
          ,command ,@args)))))

(defmacro cmds! (&rest branches)
  "Returns a dispatcher that runs the a command in BRANCHES.
Meant to be used as a target for keybinds (e.g. with `define-key' or `map!').

BRANCHES is a flat list of CONDITION COMMAND pairs. CONDITION is a lisp form
that is evaluated when (and each time) the dispatcher is invoked. If it returns
non-nil, COMMAND is invoked, otherwise it falls through to the next pair.

The last element of BRANCHES can be a COMMANd with no CONDITION. This acts as
the fallback if all other conditions fail.

Otherwise, Emacs will fall through the keybind and search the next keymap for a
keybind (as if this keybind never existed).

See `general-key-dispatch' for what other arguments it accepts in BRANCHES."
  (declare (doc-string 1))
  (let ((docstring (if (stringp (car branches)) (pop branches) ""))
        fallback)
    (when (cl-oddp (length branches))
      (setq fallback (car (last branches))
            branches (butlast branches)))
    (let ((defs (cl-loop for (key value) on branches by 'cddr
                         unless (keywordp key)
                         collect (list key value))))
      `'(menu-item
         ,(or docstring "") nil
         :filter (lambda (&optional _)
                   (let (it)
                     (cond ,@(mapcar (lambda (pred-def)
                                       `((setq it ,(car pred-def))
                                         ,(cadr pred-def)))
                                     defs)
                           (t ,fallback))))))))

;;;; Mutation
(defmacro appendq! (sym &rest lists)
  "Append LISTS to SYM in place."
  `(setq ,sym (seq-uniq (append ,sym ,@lists))))

(defmacro setq! (&rest settings)
  "A stripped-down `customize-set-variable' with the syntax of `setq'.

This can be used as a drop-in replacement for `setq'. Particularly when you know
a variable has a custom setter (a :set property in its `defcustom' declaration).
This triggers setters. `setq' does not."
  (macroexp-progn
   (cl-loop for (var val) on settings by 'cddr
            collect `(funcall (or (get ',var 'custom-set) #'set)
                              ',var ,val))))

(defmacro delq! (elt list &optional fetcher)
  "`delq' ELT from LIST in-place.

If FETCHER is a function, ELT is used as the key in LIST (an alist)."
  `(setq ,list
         (delq ,(if fetcher
                    `(funcall ,fetcher ,elt ,list)
                  elt)
               ,list)))

(defmacro pushnew! (place &rest values)
  "Push VALUES sequentially into PLACE, if they aren't already present.
This is a variadic `cl-pushnew'."
  (let ((var (make-symbol "result")))
    `(dolist (,var (list ,@values) (with-no-warnings ,place))
       (cl-pushnew ,var ,place :test #'equal))))

(defmacro prependq! (sym &rest lists)
  "Prepend LISTS to SYM in place."
  `(setq ,sym (seq-uniq (append ,@lists ,sym))))


;;;; loading

(defmacro add-to-load-path! (&rest dirs)
  "Add DIRS to `load-path', relative to the current file.
The current file is the file from which `add-to-load-path!' is used."
  `(let ((default-directory ,(dir!))
         file-name-handler-alist)
     (dolist (dir (list ,@dirs))
       (cl-pushnew (expand-file-name dir) load-path :test #'string=))))

(defmacro after! (package &rest body)
  "Evaluate BODY after PACKAGE have loaded.

PACKAGE is a symbol or list of them. These are package names, not modes,
functions or variables. It can be:

- An unquoted package symbol (the name of a package)
    (after! helm BODY...)
- An unquoted list of package symbols (i.e. BODY is evaluated once both magit
  and git-gutter have loaded)
    (after! (magit git-gutter) BODY...)
- An unquoted, nested list of compound package lists, using any combination of
  :or/:any and :and/:all
    (after! (:or package-a package-b ...)  BODY...)
    (after! (:and package-a package-b ...) BODY...)
    (after! (:and package-a (:or package-b package-c) ...) BODY...)
  Without :or/:any/:and/:all, :and/:all are implied.

This is a wrapper around `eval-after-load' that:

1. Suppresses warnings for disabled packages at compile-time
2. No-ops for package that are disabled by the user (via `package!')
3. Supports compound package statements (see below)
4. Prevents eager expansion pulling in autoloaded macros all at once"
  (declare (indent defun) (debug t))
  (if (symbolp package)
      (when (featurep! package)
        (list (if (or (not (bound-and-true-p byte-compile-current-file))
                      (require package nil 'noerror))
                  #'progn
                #'with-no-warnings)
              ;; We intentionally avoid `with-eval-after-load' to prevent eager
              ;; macro expansion from pulling (or failing to pull) in autoloaded
              ;; macros/packages.
              `(eval-after-load ',package ',(macroexp-progn body))))
    (let ((p (car package)))
      (cond ((memq p '(:or :any))
             (macroexp-progn
              (cl-loop for next in (cdr package)
                       collect `(after! ,next ,@body))))
            ((memq p '(:and :all))
             (dolist (next (reverse (cdr package)))
               (setq body `((after! ,next ,@body))))
             `(after! (:and ,@package) ,@body))))))

(defun radian--handle-load-error (e target path)
  (let* ((source (file-name-sans-extension target))
         (err (cond ((file-in-directory-p
                      source (file-name-directory radian-lib-file))
                     (cons 'radian-error (file-name-directory radian-lib-file)))
                    ((file-in-directory-p
                      source (file-name-directory radian-local-init-file))
                     (cons 'radian-private-error
                           (file-name-directory radian-local-init-file)))
                    (path (cons 'error (file-name-directory path))))))
    (signal (car err)
            (list (file-relative-name
                   (concat source ".el")
                   (cdr err))
                  e))))

(defmacro load! (filename &optional path noerror)
  "Load a file relative to the current executing file (`load-file-name').

FILENAME is either a file path string or a form that should evaluate to such a
string at run time. PATH is where to look for the file (a string representing a
directory path). If omitted, the lookup is relative to either `load-file-name',
`byte-compile-current-file' or `buffer-file-name' (checked in that order).

If NOERROR is non-nil, don't throw an error if the file doesn't exist."
  (let* ((path (or path
                   (dir!)
                   (error "Could not detect path to look for '%s' in"
                          filename)))
         (file (if path
                   `(expand-file-name ,filename ,path)
                 filename)))
    `(condition-case-unless-debug e
         (let (file-name-handler-alist)
           (load ,file ,noerror 'nomessage))
       (radian-error (signal (car e) (cdr e)))
       (error (radian--handle-load-error e ,file ,path)))))

(defmacro defer-until! (condition &rest body)
  "Run BODY when CONDITION is true (checks on `after-load-functions'). Meant to
serve as a predicated alternative to `after!'."
  (declare (indent defun) (debug t))
  `(if ,condition
       (progn ,@body)
     ,(let ((fn (intern (format "radian--delay-form-%s-h" (sxhash (cons condition body))))))
        `(progn
           (fset ',fn (lambda (&rest args)
                        (when ,(or condition t)
                          (remove-hook 'after-load-functions ',fn)
                          (unintern ',fn nil)
                          (ignore args)
                          ,@body)))
           (put ',fn 'permanent-local-hook t)
           (add-hook 'after-load-functions ',fn)))))

(defmacro defer-feature! (feature &rest fns)
  "Pretend FEATURE hasn't been loaded yet, until FEATURE-hook or FN runs.

Some packages (like `elisp-mode' and `lisp-mode') are loaded immediately at
startup, which will prematurely trigger `after!' (and `with-eval-after-load')
blocks. To get around this we make Emacs believe FEATURE hasn't been loaded yet,
then wait until FEATURE-hook (or MODE-hook, if FN is provided) is triggered to
reverse this and trigger `after!' blocks at a more reasonable time."
  (let ((advice-fn (intern (format "radian--defer-feature-%s-a" feature))))
    `(progn
       (delq! ',feature features)
       (defadvice! ,advice-fn (&rest _)
         :before ',fns
         ;; Some plugins (like yasnippet) will invoke a fn early to parse
         ;; code, which would prematurely trigger this. In those cases, well
         ;; behaved plugins will use `delay-mode-hooks', which we can check for:
         (unless delay-mode-hooks
           ;; ...Otherwise, announce to the world this package has been loaded,
           ;; so `after!' handlers can react.
           (provide ',feature)
           (dolist (fn ',fns)
             (advice-remove fn #',advice-fn)))))))


;;;; Hooks
(defvar radian--transient-counter 0)
(defmacro add-transient-hook! (hook-or-function &rest forms)
  "Attaches a self-removing function to HOOK-OR-FUNCTION.

FORMS are evaluated once, when that function/hook is first invoked, then never
again.

HOOK-OR-FUNCTION can be a quoted hook or a sharp-quoted function (which will be
advised)."
  (declare (indent 1))
  (let ((append (if (eq (car forms) :after) (pop forms)))
        ;; Avoid `make-symbol' and `gensym' here because an interned symbol is
        ;; easier to debug in backtraces (and is visible to `describe-function')
        (fn (intern (format "radian--transient-%d-h" (cl-incf radian--transient-counter)))))
    `(let ((sym ,hook-or-function))
       (defun ,fn (&rest _)
         ,(format "Transient hook for %S" (radian-unquote hook-or-function))
         ,@forms
         (let ((sym ,hook-or-function))
           (cond ((functionp sym) (advice-remove sym #',fn))
                 ((symbolp sym)   (remove-hook sym #',fn))))
         (unintern ',fn nil))
       (cond ((functionp sym)
              (advice-add ,hook-or-function ,(if append :after :before) #',fn))
             ((symbolp sym)
              (put ',fn 'permanent-local-hook t)
              (add-hook sym #',fn ,append))))))

(defmacro add-hook! (hooks &rest rest)
  "A convenience macro for adding N functions to M hooks.

This macro accepts, in order:

  1. The mode(s) or hook(s) to add to. This is either an unquoted mode, an
     unquoted list of modes, a quoted hook variable or a quoted list of hook
     variables.
  2. Optional properties :local, :append, and/or :depth [N], which will make the
     hook buffer-local or append to the list of hooks (respectively),
  3. The function(s) to be added: this can be a quoted function, a quoted list
     thereof, a list of `defun' or `cl-defun' forms, or arbitrary forms (will
     implicitly be wrapped in a lambda).

\(fn HOOKS [:append :local [:depth N]] FUNCTIONS-OR-FORMS...)"
  (declare (indent (lambda (indent-point state)
                     (goto-char indent-point)
                     (when (looking-at-p "\\s-*(")
                       (lisp-indent-defform state indent-point))))
           (debug t))
  (let* ((hook-forms (radian--resolve-hook-forms hooks))
         (func-forms ())
         (defn-forms ())
         append-p local-p remove-p depth)
    (while (keywordp (car rest))
      (pcase (pop rest)
        (:append (setq append-p t))
        (:depth  (setq depth (pop rest)))
        (:local  (setq local-p t))
        (:remove (setq remove-p t))))
    (while rest
      (let* ((next (pop rest))
             (first (car-safe next)))
        (push (cond ((memq first '(function nil))
                     next)
                    ((eq first 'quote)
                     (let ((quoted (cadr next)))
                       (if (atom quoted)
                           next
                         (when (cdr quoted)
                           (setq rest (cons (list first (cdr quoted)) rest)))
                         (list first (car quoted)))))
                    ((memq first '(defun cl-defun))
                     (push next defn-forms)
                     (list 'function (cadr next)))
                    ((prog1 `(lambda (&rest _) ,@(cons next rest))
                       (setq rest nil))))
              func-forms)))
    `(progn
       ,@defn-forms
       (dolist (hook (nreverse ',hook-forms))
         (dolist (func (list ,@func-forms))
           ,(if remove-p
                `(remove-hook hook func ,local-p)
              `(add-hook hook func ,(or depth append-p) ,local-p)))))))

(defmacro remove-hook! (hooks &rest rest)
  "A convenience macro for removing N functions from M hooks.

Takes the same arguments as `add-hook!'.

If N and M = 1, there's no benefit to using this macro over `remove-hook'.

\(fn HOOKS [:append :local] FUNCTIONS)"
  (declare (indent defun) (debug t))
  `(add-hook! ,hooks :remove ,@rest))

(defmacro setq-hook! (hooks &rest var-vals)
  "Sets buffer-local variables on HOOKS.

\(fn HOOKS &rest [SYM VAL]...)"
  (declare (indent 1))
  (macroexp-progn
   (cl-loop for (var val hook fn) in (radian--setq-hook-fns hooks var-vals)
            collect `(defun ,fn (&rest _)
                       ,(format "%s = %s" var (pp-to-string val))
                       (setq-local ,var ,val))
            collect `(remove-hook ',hook #',fn) ; ensure set order
            collect `(add-hook ',hook #',fn))))

(defmacro unsetq-hook! (hooks &rest vars)
  "Unbind setq hooks on HOOKS for VARS.

\(fn HOOKS &rest [SYM VAL]...)"
  (declare (indent 1))
  (macroexp-progn
   (cl-loop for (_var _val hook fn)
            in (radian--setq-hook-fns hooks vars 'singles)
            collect `(remove-hook ',hook #',fn))))


;;;; Definers
(defmacro defadvice! (symbol arglist &optional docstring &rest body)
  "Define an advice called SYMBOL and add it to PLACES.

ARGLIST is as in `defun'. WHERE is a keyword as passed to `advice-add', and
PLACE is the function to which to add the advice, like in `advice-add'.
DOCSTRING and BODY are as in `defun'.

\(fn SYMBOL ARGLIST &optional DOCSTRING &rest [WHERE PLACES...] BODY\)"
  (declare (doc-string 3) (indent defun))
  (unless (stringp docstring)
    (push docstring body)
    (setq docstring nil))
  (let (where-alist)
    (while (keywordp (car body))
      (push `(cons ,(pop body) (radian-enlist ,(pop body)))
            where-alist))
    `(progn
       (defun ,symbol ,arglist ,docstring ,@body)
       ;; You'd think I would put an `eval-and-compile' around this. It
       ;; turns out that doing so breaks the ability of
       ;; `elisp-completion-at-point' to complete on function arguments
       ;; to the advice. I know, right? Apparently this is because the
       ;; code that gets the list of lexically bound symbols at point
       ;; tries to `macroexpand-all', and apparently macroexpanding
       ;; `eval-and-compile' goes ahead and evals the thing and returns
       ;; only the function symbol. No good. But the compiler does still
       ;; want to know the function is defined (this is a Gilardi
       ;; scenario), so we pacify it by `eval-when-compile'ing something
       ;; similar (see below).
       (eval-when-compile (declare-function ,symbol nil))
       (dolist (targets (list ,@(nreverse where-alist)))
         (dolist (target (cdr targets))
           (advice-add target (car targets) #',symbol))))))

(defmacro undefadvice! (symbol _arglist &optional docstring &rest body)
  "Undefine an advice called SYMBOL.

This has the same signature as `defadvice!' an exists as an easy undefiner when
testing advice (when combined with `rotate-text').

\(fn SYMBOL ARGLIST &optional DOCSTRING &rest [WHERE PLACES...] BODY\)"
  (declare (doc-string 3) (indent defun))
  (let (where-alist)
    (unless (stringp docstring)
      (push docstring body))
    (while (keywordp (car body))
      (push `(cons ,(pop body) (radian-enlist ,(pop body)))
            where-alist))
    `(dolist (targets (list ,@(nreverse where-alist)))
       (dolist (target (cdr targets))
         (advice-remove target #',symbol)))))

(defun radian--random-string ()
  "Return a random string designed to be globally unique."
  (md5 (format "%s%s%s%s"
               (system-name) (emacs-pid) (current-time) (random))))

(defun radian--list-of-strings-p (obj)
  "Return non-nil if OBJ is a list of strings."
  (and (listp obj)
       (cl-every #'stringp obj)))

(defun radian--path-join (path &rest segments)
  "Join PATH with SEGMENTS using `expand-file-name'.
First `expand-file-name' is called on the first member of
SEGMENTS, with PATH as DEFAULT-DIRECTORY. Then `expand-file-name'
is called on the second member, with the result of the first call
as DEFAULT-DIRECTORY, and so on. If no SEGMENTS are passed, the
return value is just PATH."
  (while segments
    (setq path (expand-file-name (pop segments) path)))
  path)

(defun radian-run-hook (hook)
  "Run HOOK (a hook function) with better error handling.
Meant to be used with `run-hook-wrapped'."
  (condition-case-unless-debug e
      (funcall hook)
    (error
     (signal 'radian-hook-error (list hook e))))
  ;; return nil so `run-hook-wrapped' won't short circuit
  nil)

(defun radian-run-hooks (&rest hooks)
  "Run HOOKS (a list of hook variable symbols) with better error handling.
Is used as advice to replace `run-hooks'."
  (dolist (hook hooks)
    (condition-case-unless-debug e
        (run-hook-wrapped hook #'radian-run-hook)
      (radian-hook-error
       (unless debug-on-error
         (lwarn hook :error "Error running hook %S because: %s"
                (if (symbolp (cadr e))
                    (symbol-name (cadr e))
                  (cadr e))
                (caddr e)))
       (signal 'radian-hook-error (cons hook (cdr e)))))))

(defun radian-run-hook-on (hook-var trigger-hooks)
  "Configure HOOK-VAR to be invoked exactly once when any of the TRIGGER-HOOKS
are invoked *after* Emacs has initialized (to reduce false positives). Once
HOOK-VAR is triggered, it is reset to nil.

HOOK-VAR is a quoted hook.
TRIGGER-HOOK is a list of quoted hooks and/or sharp-quoted functions."
  (dolist (hook trigger-hooks)
    (let ((fn (intern (format "%s-init-on-%s-h" hook-var hook))))
      (fset
       fn (lambda (&rest _)
            ;; Only trigger this after Emacs has initialized.
            (when (and after-init-time
                       (or (daemonp)
                           ;; In some cases, hooks may be lexically unset to
                           ;; inhibit them during expensive batch operations on
                           ;; buffers (such as when processing buffers
                           ;; internally). In these cases we should assume this
                           ;; hook wasn't invoked interactively.
                           (and (boundp hook)
                                (symbol-value hook))))
              (run-hooks hook-var)
              (set hook-var nil))))
      (cond ((daemonp)
             ;; In a daemon session we don't need all these lazy loading
             ;; shenanigans. Just load everything immediately.
             (add-hook 'after-init-hook fn 'append))
            ((eq hook 'find-file-hook)
             ;; Advise `after-find-file' instead of using `find-file-hook'
             ;; because the latter is triggered too late (after the file has
             ;; opened and modes are all set up).
             (advice-add 'after-find-file :before fn '((depth . -101))))
            ((add-hook hook fn -101)))
      fn)))

(defmacro spp (form) "Super pp" `(progn (pp ,form) nil))

;;; Wraps of leaf and straight
(defmacro pow! (name &rest args)
  "Like `leaf' with :disabled `featurep!'"
  (declare (indent 1))
  `(unless mini-p (leaf ,name :disabled (not (featurep! ',name)) ,@args :straight t)))

(defmacro -ow! (name &rest args)
  "Like `pow!' without :straight."
  (declare (indent 1))
  `(unless mini-p (leaf ,name :disabled (not (featurep! ',name)) ,@args)))

(defmacro pow (name &rest args)
  "Same to `pow!', but without `:disabled'."
  (declare (indent 1)) `(leaf ,name ,@args :straight t))

(defalias '-ow #'leaf)
(put '-ow 'lisp-indent-function 1)

(defmacro --w (name &rest args)
  "Like `-ow' except :loading is nil"
  (declare (indent 1)) `(leaf ,name :loading nil ,@args))

(defalias 'sup #'straight-use-package)
(defalias '-key 'leaf-key)
(defalias '-key* 'leaf-key*)
(defalias '-keys 'leaf-keys)
(defalias '-keys* 'leaf-keys*)
(defalias 'mkey 'leaf-key-bind-keymap)
(defalias 'mkey* 'leaf-key-bind-keymap*)
(defalias 'mkeys 'leaf-keys-bind-keymap)
(defalias 'mkeys* 'leaf-keys-bind-keymap*)

(defvar radian-disabled-packages
  '(haskell-mode hl-line ligature)
  "List of packages that Radian should not load.
If the list starts with `:not', packages that are not part of this
list are not loaded instead. This variable should be modified in
`radian-before-straight-hook' to be effective.")

(defun featurep! (package)
  "Return nil if PACKAGE should not be loaded by Radian."
  (declare (indent defun))
  (if (symbolp package)
      (if (eq (car radian-disabled-packages) :not)
          (memq package radian-disabled-packages)
        (not (memq package radian-disabled-packages)))
    (let ((p (car package)))
      (cond ((eq p :or) (cl-some #'featurep! (cdr package)))
            ((eq p :and) (cl-every #'featurep! (cdr package)))
            (t (cl-every #'featurep! (cdr package)))))))

(defsubst radian-disable-feature (feature)
  "Disable Radian's customization of FEATURE.
FEATURE can be the name of any package. No checks are made to
ensure that the name is valid.
Features should be disabled in `radian-before-straight-hook'."
  (if (eq (car radian-disabled-packages) :not)
      (setq radian-disabled-packages
            (delq feature radian-disabled-packages))
    (cl-pushnew feature radian-disabled-packages)))

;;; Define special hooks and load local configuration

;; Reset the value of this variable so that stale functions don't
;; stick around.
(setq radian--finalize-init-hook nil)

;; Allow binding this variable dynamically before straight.el has been
;; loaded.
(defvar straight-current-profile)


;;toggle proxy
(defun radian-toggle-proxy ()
  "Toggle proxy for the url.el library."
  (interactive)
  (cond
   (url-proxy-services
    (message "Turn off URL proxy")
    (setq url-proxy-services nil))
   (t
    (message "Turn on URL proxy")
    (setq url-proxy-services
          '(("no_proxy" . "^\\(localhost\\|10\\..*\\|192\\.168\\..*\\)")
            ("http"     . "localhost:1081")
            ("https"    . "localhost:1081"))))))

;;; font
(setq radian-font-size 98                  ;height
      radian-unicode-font (font-spec :family "Symbola")
      radian-serif-font (font-spec :family "Source Han Serif SC")
      ;; radian-variable-pitch-font (font-spec :family "Futura-Medium")
      radian-variable-pitch-font (font-spec :family "Comic Sans MS" :weight 'bold)
      radian-font (font-spec :family "Cascadia Code"))


;; Disable warnings from legacy advice system. They aren't useful, and what can
;; we do about them, besides changing packages upstream?
(setq ad-redefinition-action 'accept)

;; Reduce debug output, well, unless we've asked for it.
(setq debug-on-error radian-debug-p
      jka-compr-verbose radian-debug-p)

;; After we enabled `load-prefer-newer' in init.el, disable it again
;; for the duration of init. Presumably, it slows things down, and we
;; shouldn't need it for anything but loading radian.el itself.
(setq load-prefer-newer nil)

;; Disable bidirectional text rendering for a modest performance boost. I've set
;; this to `nil' in the past, but the `bidi-display-reordering's docs say that
;; is an undefined state and suggest this to be just as good:
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

;; Disabling the BPA makes redisplay faster, but might produce incorrect display
;; reordering of bidirectional text with embedded parentheses and other bracket
;; characters whose 'paired-bracket' Unicode property is non-nil.
(setq bidi-inhibit-bpa t)  ; Emacs 27 only

;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions
;; in non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate syntax highlighting right after scrolling, which should
;; quickly self-correct.
(setq fast-but-imprecise-scrolling t)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we halve startup times, particularly when we use
;; fonts that are larger than the system default (which would resize the frame).
(setq frame-inhibit-implied-resize t)

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

;; Font compacting can be terribly expensive, especially for rendering icon
;; fonts on Windows. Whether disabling it has a notable affect on Linux and Mac
;; hasn't been determined, but we inhibit it there anyway. This increases memory
;; usage, however!
(setq inhibit-compacting-font-caches t)

;; Remove command line options that aren't relevant to our current OS; means
;; slightly less to process at startup.
(unless *MAC   (setq command-line-ns-option-alist nil))
(unless *LINUX (setq command-line-x-option-alist nil))

;; Contrary to what many Emacs users have in their configs, you really don't
;; need more than this to make UTF-8 the default coding system:
(if *WINDOWS
    (progn
      (set-clipboard-coding-system 'utf-16-le)
      (set-selection-coding-system 'utf-16-le))
  (set-selection-coding-system 'utf-8)
  (set-next-selection-coding-system 'utf-8))
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system 'utf-8-unix)
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(setq locale-coding-system 'utf-8-unix)
;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

;; Performance on Windows is considerably worse than elsewhere. We'll need
;; everything we can get.
(when (boundp 'w32-get-true-file-attributes)
  (setq w32-get-true-file-attributes nil   ; decrease file IO workload
        w32-pipe-read-delay 0              ; faster ipc
        w32-pipe-buffer-size (* 64 1024))) ; read more at a time (was 4K)

;; Adopt a sneaky garbage collection strategy of waiting until idle time to
;; collect; staving off the collector while the user is working.
(setq gcmh-idle-delay 'auto
      gcmh-auto-idle-delay-factor 10
      gcmh-high-cons-threshold (* 16 1024 1024))  ; 16mb

;; HACK `tty-run-terminal-initialization' is *tremendously* slow for some
;;      reason; inexplicably doubling startup time for terminal Emacs. Keeping
;;      it disabled will have nasty side-effects, so we simply delay it until
;;      later in the startup process and, for some reason, it runs much faster
;;      when it does.
(unless (daemonp)
  (advice-add #'tty-run-terminal-initialization :override #'ignore)
  (add-hook! 'window-setup-hook
    (defun radian-init-tty-h ()
      (advice-remove #'tty-run-terminal-initialization #'ignore)
      (tty-run-terminal-initialization (selected-frame) nil t))))

;; Allow doing a command that requires candidate-selection when you
;; are already in the middle of candidate-selection. Sometimes it's
;; handy!
(setq enable-recursive-minibuffers t)

;; Doesn't exist in terminal Emacs, but some Emacs packages (internal and
;; external) use it anyway, leading to a void-function error, so define a no-op
;; substitute to suppress them.
(unless (fboundp 'define-fringe-bitmap) (fset 'define-fringe-bitmap #'ignore))


;;;; EasyPG/EPG(GNU Privacy Guard(GnuPG))
(after! epa
  ;; With GPG 2.1+, this forces gpg-agent to use the Emacs minibuffer to prompt
  ;; for the key passphrase.
  (set 'epg-pinentry-mode 'loopback)
  ;; Default to the first enabled and non-expired key in your keyring.
  (setq-default
   epa-file-encrypt-to
   (or (default-value 'epa-file-encrypt-to)
       (unless (string-empty-p user-full-name)
         (when-let (context (ignore-errors (epg-make-context)))
           (cl-loop for key in (epg-list-keys context user-full-name 'public)
                    for subkey = (car (epg-key-sub-key-list key))
                    if (not (memq 'disabled (epg-sub-key-capability subkey)))
                    if (< (or (epg-sub-key-expiration-time subkey) 0)
                          (time-to-seconds))
                    collect (epg-sub-key-fingerprint subkey))))
       user-mail-address))
   ;; And suppress prompts if epa-file-encrypt-to has a default value (without
   ;; overwriting file-local values).
  (defadvice! +default--dont-prompt-for-keys-a (&rest _)
    :before #'epa-file-write-region
    (unless (local-variable-p 'epa-file-encrypt-to)
      (setq-local epa-file-encrypt-to (default-value 'epa-file-encrypt-to)))))


;;; Networking

;; Emacs is essentially one huge security vulnerability, what with all the
;; dependencies it pulls in from all corners of the globe. Let's try to be at
;; least a little more discerning.
(setq gnutls-verify-error (and (fboundp 'gnutls-available-p)
                               (gnutls-available-p)
                               (not (getenv-internal "INSECURE")))
      gnutls-algorithm-priority
      (when (boundp 'libgnutls-version)
        (concat "SECURE128:+SECURE192:-VERS-ALL"
                (if (and (not *WINDOWS)
                         (>= libgnutls-version 30605))
                    ":+VERS-TLS1.3")
                ":+VERS-TLS1.2"))
      ;; `gnutls-min-prime-bits' is set based on recommendations from
      ;; https://www.keylength.com/en/4/
      gnutls-min-prime-bits 3072
      tls-checktrust gnutls-verify-error
      ;; Emacs is built with `gnutls' by default, so `tls-program' would not be
      ;; used in that case. Otherwise, people have reasons to not go with
      ;; `gnutls', we use `openssl' instead. For more details, see
      ;; https://redd.it/8sykl1
      tls-program '("openssl s_client -connect %h:%p -CAfile %t -nbio -no_ssl3 -no_tls1 -no_tls1_1 -ign_eof"
                    "gnutls-cli -p %p --dh-bits=3072 --ocsp --x509cafile=%t \
--strict-tofu --priority='SECURE192:+SECURE128:-VERS-ALL:+VERS-TLS1.2:+VERS-TLS1.3' %h"
                    ;; compatibility fallbacks
                    "gnutls-cli -p %p %h"))

;; Feature `url-http' is a library for making HTTP requests.
(with-eval-after-load 'url-http
  (eval-when-compile (require 'url-http))

  (defadvice! radian--no-query-on-http-kill
    (buffer)
    "Disable query-on-exit for all network connections.
This prevents Emacs shutdown from being interrupted just because
there is a pending network request."
    :filter-return #'url-http
    (prog1 buffer
      (set-process-query-on-exit-flag
       (get-buffer-process buffer) nil))))

;;; Radian keymaps
;; REVIEW: if `define-key' called above here, shall lead to "Symbol's
;; value as variable is void: \213".

; :doc "Keymap for Radian commands that should be put under a comma prefix.
; This keymap is bound under \\[radian-comma-keymap]."
; :prefix 'radian-comma-keymap
(define-prefix-command 'radian-comma-keymap 'radian-comma-keymap)

; :doc "Keymap for Radian commands that should be put under a zip prefix.
; This keymap is bound under \\[radian-zip-keymap]."
; :prefix 'radian-zip-keymap
(define-prefix-command 'radian-zip-keymap 'radian-zip-keymap)
(define-key radian-zip-keymap "z" #'eval-last-sexp)

(defvar radian-keymap (make-sparse-keymap))
;  :doc "Keymap for Radian commands that should be put under a prefix.
;This keymap is bound under \\[radian-keymap].")

(define-key global-map "\M-P" radian-keymap)

(defmacro radian-bind-key (key-name command)
  "Bind a key in `radian-keymap'.
KEY-NAME, COMMAND, and PREDICATE are as in `-key'."
  `(-key ,key-name ,command 'radian-keymap))

(defun radian-join-keys (&rest keys)
  "Join key sequences KEYS. Empty strings and nils are discarded.
\(radian--join-keys \"\\[radian-keymap] e\" \"e i\")
  => \"\\[radian-keymap] e e i\"
\(radian--join-keys \"\\[radian-keymap]\" \"\" \"e i\")
  => \"\\[radian-keymap] e i\""
  (string-join (remove "" (mapcar #'string-trim (remove nil keys))) " "))

;;;; straight.el

;; Use symlink preferably for building
(setq straight-use-symlinks t)

;; Tell straight.el about the profiles we are going to be using.
(setq straight-profiles
      '(;; Packages registered in this file.
        (radian . "radian.el")
        ;; Packages registered in the local init-file during hooks.
        (radian-local . "radian-local.el")
        ;; Packages registered interactively.
        (nil . "default.el")))

;; Pretend to dynamically bind `straight-current-profile' to `radian'
;; over the init-file. We do this to avoid having straight.el
;; configuration mentioned in the top-level init-file.

(add-hook!  'radian--finalize-init-hook
  (defun radian--reset-straight-current-profile ()
    "Reset `straight-current-profile' to nil.
This function is used on `radian--finalize-init-hook' to emulate
binding the variable dynamically over the entire init-file."
    (setq straight-current-profile nil)))

(setq straight-current-profile 'radian)

(setq straight-repository-branch "develop"
      ;; Since byte-code is rarely compatible across different versions of
      ;; Emacs, it's best we build them in separate directories, per emacs
      ;; version.
      straight-build-dir (format "build-%s" emacs-version)
      ;; Before switching to straight, `user-emacs-directory' would average out at
      ;; around 100mb with half Radian's modules at ~230 packages. Afterwards, at
      ;; around 1gb. With shallow cloning, that is reduced to ~400mb. This has
      ;; no affect on packages that are pinned, however (run 'radian purge' to
      ;; compact those after-the-fact). Some packages break when shallow cloned
      ;; (like magit and org), but we'll deal with that elsewhere.
      straight-vc-git-default-clone-depth 1)

;; If watchexec and Python are installed, use file watchers to detect
;; package modifications. This saves time at startup. Otherwise, use
;; the ever-reliable find(1).
(if (and (executable-find "watchexec") (executable-find "python3"))
    (setq straight-check-for-modifications '(watch-files find-when-checking))
  (setq straight-check-for-modifications '(find-at-startup find-when-checking)))

;; Clear out recipe overrides (in case of re-init).
(setq straight-recipe-overrides nil)

;; Bootstrap the package manager, straight.el.
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Load autoload subdir
(pushnew! straight-default-files-directive "autoload/*.el")

;; Not using `use-package', `leaf' instead.
(straight-use-package-mode -1)

;;;; Leaf

;; Package `leaf' provides a handy macro by the same name which
;; is essentially a wrapper around `eval-after-load' with a lot
;; of handy syntactic sugar and useful features.

(sup 'leaf)
(sup '(leaf-keywords :repo "meziberry/leaf-keywords.el" :branch "noz"))

(-ow leaf-keywords
  :require t
  :bind (radian-comma-keymap ("lf" . leaf-find))
  :config

  ;; 1) Add the :increment :aftercall to leaf
  (dolist (keyword '(:increment :aftercall))
    (cl-pushnew keyword leaf-defer-keywords))

  ;; Accept: 't, symbol and list of these (and nested)
  ;; Return: symbol list.
  ;; Note  : 'nil is just ignored
  ;;         remove duplicate element
  (cl-pushnew '((eq leaf--key :increment)
                (mapcar (lambda (elm) (if (eq t elm) leaf--name elm))
                        (delete-dups (leaf-flatten leaf--value))))
              leaf-normalize)

  ;; :increment t pkg (a b c)
  (setq leaf-keywords-after-conditions
        (plist-put
         leaf-keywords-after-conditions
         :increment
         '`((radian-load-incrementally ',leaf--value) ,@leaf--body)))

  ;; :aftercall a-hook b-functions
  (setq leaf-keywords-after-conditions
        (plist-put
         leaf-keywords-after-conditions
         :aftercall
         '`((radian-load-aftercall ',leaf--name ',leaf--value) ,@leaf--body)))

  ;; 2) Do works for our pow! -ow!
  ;; HACK customize :straight and :disable 's macroexpands
  ;; Let recipe sole. Recipe specified by user take precedence.
  (cl-pushnew '((eq leaf--key :straight)
                (unless (eq (car-safe leaf--value) nil)
                  (mapcar
                   (lambda (elm) (if (eq t elm) leaf--name elm))
                   (if (alist-get leaf--name leaf--value)
                       (delq t leaf--value)
                     leaf--value))))
              leaf-normalize)
  ;; when disable feature, register it's recipe.
  (setq leaf-keywords
        (plist-put
         leaf-keywords
         :disabled
         '(if (eval (car leaf--value))
              `(,@(mapcar
                   (lambda (elm) `(straight-register-package ',elm))
                   (let ((recipes (plist-get leaf--rest :straight)))
                     ;; If have the recipe from :straight.
                     (unless (eq (car-safe recipes) nil)
                       (mapcar
                        (lambda (elm) (if (eq t elm) leaf--name elm))
                        (if (alist-get leaf--name recipes)
                            (delq t recipes)
                          recipes))))))
            `(,@leaf--body))))

  ;; 3) Load package when compiling
  ;; non-nil : require package when compiling.
  ;; nil     : no require.
  ;; And let `radian--current-feature' set in leaf--defaults.

  (setq leaf-keywords-before-protection
        (plist-put
         leaf-keywords-before-protection
         :loading
         '(progn
            (setq radian--current-feature leaf--name)
            (and byte-compile-current-file (car leaf--value)
                 (require leaf--name nil 'noerror))
            `(,@leaf--body))))
  (setq leaf-defaults (plist-put leaf-defaults :loading t))

  ;; Start `leaf-keywords'
  (leaf-keywords-init))

;; el-patch
(pow el-patch :custom (el-patch-enable-use-package-integration . nil))
;; Only needed at compile time, thanks to Jon
;; <https://github.com/raxod502/el-patch/pull/11>.
(eval-when-compile (require 'el-patch))

;; NOTE :bind imply (map @bds) => (map :package name @bds),
;;       Here :package imply `eval-after-load'.
;;      :bind-keymap imply `require' leaf--name.
(--w leaf
  :init
  (plist-put leaf-system-defaults :leaf-defun nil)
  (plist-put leaf-system-defaults :leaf-defvar nil)
  :init/el-patch
  (defmacro leaf-key-bind-keymap (key kmap &optional keymap pkg)
    "Bind KEY to KMAP in KEYMAP (`global-map' if not passed).
If PKG passed, require PKG before binding."
    `(progn
       (el-patch-swap
         ,(when pkg `(require ,pkg))
         (when ,pkg (require ,pkg)))
       (leaf-key ,key ,kmap ,keymap))))

;;;; No-littering
;; Package `no-littering' changes the default paths for lots of
;; different packages, with the net result that the ~/.emacs.d folder
;; is much more clean and organized.
(pow no-littering
  :pre-setq
  (no-littering-etc-directory . *etc/*)
  (no-littering-var-directory . *cache/*)
  :require t)

(pow blackout)

;;;; Meow
(pow meow
  :straight (meow :repo "meow-edit/meow" :branch "master")
  :require t
  :chord (meow-normal-state-keymap (".." . meow-bounds-of-thing))
  :hook (after-init-hook . meow-global-mode)
  :custom
  (meow-use-cursor-position-hack . t)
  (meow-use-enhanced-selection-effect . t)
  (meow-selection-command-fallback . '((meow-save . meow-keypad-start)
                                       (meow-change . meow-keypad-start)
                                       (meow-cancel-selection . meow-keypad-start)
                                       (meow-reverse . meow-open-below)
                                       (meow-replace . meow-replace-char)
                                       (meow-pop-selection . meow-pop-to-mark)
                                       (meow-kill . meow-C-k)))
  :config/el-patch
  (defun meow-change ()
    "Kill current selection and switch to INSERT state.
This command supports `meow-selection-command-fallback'."
    (interactive)
    (el-patch-splice 2
      (when (meow--allow-modify-p)
        (meow--with-selection-fallback
         (el-patch-wrap 2
           (when (meow--allow-modify-p)
             (delete-region (region-beginning) (region-end))
             (meow--switch-state 'insert)
             (setq-local meow--insert-pos (point))))))))

  (defun meow-cancel-selection ()
    "Cancel selection or grab.
This command supports `meow-selection-command-fallback'."
    (interactive)
    (el-patch-wrap 2 3
      (if (not (secondary-selection-exist-p))
          (meow--with-selection-fallback (meow--cancel-selection))
        ;; cancel `meow-grab' selection
        (delete-overlay mouse-secondary-overlay)
        (setq mouse-secondary-start (make-marker))
        (move-marker mouse-secondary-start (point)))))

  :config
  (cl-pushnew '(help-mode . motion) meow-mode-state-list)
  (after! consult
    (defun +consult--line-a (&rest args)
      "after `consult--line' copy the car of `consult--line-history'
into `regexp-search-ring'"
      (ignore args)
      (when-let ((search (car consult--line-history)))
        (unless (string-equal search (car regexp-search-ring))
          (add-to-history 'regexp-search-ring search regexp-search-ring-max))))
    (advice-add 'consult--line :after #'+consult--line-a))

  (radian-bind-key "t" #'meow-temp-normal)
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-dvp)

  (meow-motion-overwrite-define-key
   '("SPC" . meow-motion-origin-command)
   '("/"   . consult-line-or-isearch)
   '("$"   . repeat))

  ;; Drovk programmer keybindings on normal mode.
  (meow-normal-define-key
   '("*" . meow-expand-0)              '("=" . meow-expand-9)
   '("!" . meow-expand-8)              '("[" . meow-expand-7)
   '("]" . meow-expand-6)              '("{" . meow-expand-5)
   '("+" . meow-expand-4)              '("}" . meow-expand-3)
   '(")" . meow-expand-2)              '("(" . meow-expand-1)
   '("1" . digit-argument)             '("2" . digit-argument)
   '("3" . digit-argument)             '("4" . digit-argument)
   '("5" . digit-argument)             '("6" . digit-argument)
   '("7" . digit-argument)             '("8" . digit-argument)
   '("9" . digit-argument)             '("0" . digit-argument)
   '("-" . negative-argument)          '(";" . comment-dwim)
   '("," . radian-comma-keymap)        '("." . meow-inner-of-thing)
   ;'("<" . nil)                        '(">" . nil)
   `("a" . ,(cmds! (region-active-p) #'meow-append #'meow-change-char))
   '("A" . meow-kmacro-matches)
   '("b" . meow-back-word)             '("B" . meow-back-symbol)
   '("c" . meow-change)                '("C" . meow-change-save)
   '("d" . xah-shrink-whitespaces)     '("D" . meow-replace)
   '("e" . meow-line)                  '("E" . radian-simple-scratch-buffer)
   '("f" . meow-find)                  '("F" . meow-find-expand)
   '("g" . meow-cancel-selection)      '("G" . meow-grab)
   '("h" . meow-left)                  '("H" . meow-left-expand)
   '("i" . meow-insert)                '("I" . meow-replace-save)
   '("j" . meow-join)                  '("J" . meow-kmacro-lines)
   '("k" . meow-kill)                 ;'("K" . nil)
   '("l" . meow-till)                  '("L" . meow-till-expand)
   '("m" . meow-mark-word)             '("M" . meow-mark-symbol)
   '("n" . meow-next)                  '("N" . meow-next-expand)
   '("o" . meow-block)                 '("O" . meow-to-block)
   '("p" . meow-prev)                  '("P" . meow-prev-expand)
   '("q" . meow-quit)                  '("Q" . meow-goto-line)
   '("r" . meow-reverse)               '("R" . meow-open-above)
   '("s" . meow-M-x)                   '("S" . meow-swap-grab)
   '("t" . meow-right)                 '("T" . meow-right-expand)
   '("u" . meow-undo)                  '("U" . winner-undo)
   '("v" . meow-search)                '("V" . meow-pop-search)
   '("w" . meow-next-word)             '("W" . meow-next-symbol)
   '("x" . meow-save)                  '("X" . meow-clipboard-save)
   '("y" . meow-yank)                  '("Y" . meow-clipboard-yank)
   '("z" . radian-zip-keymap)          '("Z" . meow-sync-grab)
   '("'" . meow-pop-selection)         '("\"" . meow-pop-grab)
   '("/" . consult-line-or-isearch)    '("?" . meow-keypad-describe-key)
   '("&" . repeat)                     '("%" . radian-reverse-region-characters)
   '("$" . meow-query-replace)         '("~" . meow-query-replace-regexp)
   ;'("#" . nil)                       ;'("`" . nil)
   ;'("@" . nil)                       ;'("^" . nil)
   `("\\" . ,(cmds! (eql '- prefix-arg) #'meow-beginning-of-thing #'meow-end-of-thing))
   `("RET" . ,(cmd! (if (bound-and-true-p delete-selection-mode) (meow--cancel-selection)) (newline-and-indent)))
   '("<escape>" . meow-last-buffer)    '("DEL" . meow-backspace)
   '("<f3>" . meow-start-kmacro-or-insert-counter) '("<f4>" . meow-end-or-call-kmacro))

  :blackout
  (meow-normal-mode meow-motion-mode meow-keypad-mode meow-insert-mode meow-beacon-mode))

;;;; gcmh-mode
(pow gcmh :blackout t)


;; File+dir local variables are initialized after the major mode and its hooks
;; have run. If you want hook functions to be aware of these customizations, add
;; them to MODE-local-vars-hook instead.
(defvar radian-inhibit-major-mode-post-hooks nil)

(defun radian-run-local-var-hooks-h ()
  "Run MODE-local-vars-hook after local variables are initialized."
  (unless radian-inhibit-major-mode-post-hooks
    (setq-local radian-inhibit-major-mode-post-hooks t)
    (radian-run-hooks (intern-soft (format "%s-local-vars-hook" major-mode)))))    ;; (format "after-%s-hook" major-mode)

;; If the user has disabled `enable-local-variables', then
;; `hack-local-variables-hook' is never triggered, so we trigger it at the end
;; of `after-change-major-mode-hook':
(defun radian-run-local-var-hooks-maybe-h ()
  "Run `radian-run-local-var-hooks-h' if `enable-local-variables' is disabled."
  (unless enable-local-variables (radian-run-local-var-hooks-h)))

;;;; Incremental lazy-loading
(--w incremental
  :config
  ;; Incrementally
  (defvar radian-incremental-packages '(t)
    "A list of packages to load incrementally after startup. Any large packages
here may cause noticeable pauses, so it's recommended you break them up into
sub-packages. For example, `org' is comprised of many packages, and can be
broken up into:

  (radian-load-incrementally
   '(calendar find-func format-spec org-macs org-compat
     org-faces org-entities org-list org-pcomplete org-src
     org-footnote org-macro ob org org-clock org-agenda
     org-capture))

This is already done by the lang/org module, however.

If you want to disable incremental loading altogether, either remove
`radian-load-packages-incrementally-h' from `emacs-startup-hook' or set
`radian-incremental-first-idle-timer' to nil. Incremental loading does not occur
in daemon sessions (they are loaded immediately at startup).")

  (defvar radian-incremental-first-idle-timer 2.0
    "How long (in idle seconds) until incremental loading starts.
Set this to nil to disable incremental loading.")

  (defvar radian-incremental-idle-timer 0.75
    "How long (in idle seconds) in between incrementally loading packages.")

  (defvar radian-incremental-load-immediately (daemonp)
    "If non-nil, load all incrementally deferred packages immediately at startup.")

  (defun radian-load-incrementally (packages &optional now)
    "Registers PACKAGES to be loaded incrementally.

If NOW is non-nil, load PACKAGES incrementally, in
`radian-incremental-idle-timer' intervals."
    (if (not now)
        (appendq! radian-incremental-packages packages)
      (while packages
        (let* ((gc-cons-threshold most-positive-fixnum)
               (req (pop packages)))
          (unless (featurep req)
            (radian-log "Incrementally loading %s" req)
            (condition-case-unless-debug e
                (or (while-no-input
                      ;; If `default-directory' is a directory that doesn't exist
                      ;; or is unreadable, Emacs throws up file-missing errors, so
                      ;; we set it to a directory we know exists and is readable.
                      (let ((default-directory *emacsd/*)
                            (inhibit-message t)
                            file-name-handler-alist)
                        (require req nil t))
                      t)
                    (push req packages))
              (error
               (message "Failed to load %S package incrementally, because: %s"
                        req e)))
            (if (not packages)
                (radian-log "Finished incremental loading")
              (run-with-idle-timer radian-incremental-idle-timer
                                   nil #'radian-load-incrementally
                                   packages t)
              (setq packages nil)))))))

  (defun radian-load-packages-incrementally-h ()
    "Begin incrementally loading packages in `radian-incremental-packages'.

If this is a daemon session, load them all immediately instead."
    (if radian-incremental-load-immediately
        (mapc #'require (cdr radian-incremental-packages))
      (when (numberp radian-incremental-first-idle-timer)
        (run-with-idle-timer radian-incremental-first-idle-timer
                             nil #'radian-load-incrementally
                             (cdr radian-incremental-packages) t)))))

;;;; After-call  SYMBOLS | HOOKS
(--w aftercall
  :config
  (defvar radian--deferred-packages-alist '(t))

  (defun radian-load-aftercall (name hooks)
    (let ((fn (make-symbol (format "radian--after-call-%s-h" name))))
      (fset fn
            (lambda (&rest _)
              (radian-log "Loading deferred package %s from %s" name fn)
              (condition-case e
                  ;; If `default-directory' is a directory that doesn't
                  ;; exist or is unreadable, Emacs throws up file-missing
                  ;; errors, so we set it to a directory we know exists and
                  ;; is readable.
                  (let ((default-directory *emacsd/*))
                    (require name))
                ((debug error)
                 (message "Failed to load deferred package %s: %s" name e)))
              (when-let (deferral-list (assq name radian--deferred-packages-alist))
                (dolist (hook (cdr deferral-list))
                  (advice-remove hook fn)
                  (remove-hook hook fn))
                (delq! deferral-list radian--deferred-packages-alist)
                (unintern fn nil))))

      (dolist (hook hooks)
        (if (string-match-p "-\\(?:functions\\|hook\\)$" (symbol-name hook))
            (add-hook hook fn)
          (advice-add hook :before fn)))

      (unless (assq name radian--deferred-packages-alist)
        (push `(,name) radian--deferred-packages-alist))
      (nconc (assq name radian--deferred-packages-alist)
             `(,@hooks)))))

;;;; Switch buffer/windows/frame functions

(defun radian-run-switch-buffer-hooks-h (&optional _)
  (let ((gc-cons-threshold most-positive-fixnum)
        (inhibit-redisplay t))
    (run-hooks 'radian-switch-buffer-hook)))

(defvar radian--last-frame nil)
(defun radian-run-switch-window-or-frame-hooks-h (&optional _)
  (let ((gc-cons-threshold most-positive-fixnum)
        (inhibit-redisplay t))
    (unless (equal (old-selected-frame) (selected-frame))
      (run-hooks 'radian-switch-frame-hook))
    (unless (or (minibufferp)
                (equal (old-selected-window) (minibuffer-window)))
      (run-hooks 'radian-switch-window-hook))))

;;;; Finding files
;; Follow symlinks when opening files. This has the concrete impact,
;; for instance, that when you edit init.el with M-P e e i and then
;; later do C-x C-f, you will be in the Radian repository instead of
;; your home directory.
(setq find-file-visit-truename t)

;; Disable the warning "X and Y are the same file" which normally
;; appears when you visit a symlinked file by the same name. (Doing
;; this isn't dangerous, as it will just redirect you to the existing
;; buffer.)
(setq find-file-suppress-same-file-warnings t)

(defvar radian--dirs-to-delete nil
  "List of directories to try to delete when killing buffer.
This is used to implement the neat feature where if you kill a
new buffer without saving it, then Radian will prompt to see if
you want to also delete the parent directories that were
automatically created.")

(defun radian--advice-find-file-create-directories
    (find-file filename &rest args)
  "Automatically create and delete parent directories of new files.
This advice automatically creates the parent directory (or directories) of
the file being visited, if necessary. It also sets a buffer-local
variable so that the user will be prompted to delete the newly
created directories if they kill the buffer without saving it.

This advice has no effect for remote files.

This is an `:around' advice for `find-file' and similar
functions.

FIND-FILE is the original `find-file'; FILENAME and ARGS are its
arguments."
  (if (file-remote-p filename)
      (apply find-file filename args)
    (let ((orig-filename filename)
          ;; For relative paths where none of the named parent
          ;; directories exist, we might get a nil from
          ;; `file-name-directory' below, which would be bad. Thus we
          ;; expand the path fully.
          (filename (expand-file-name filename))
          ;; The variable `dirs-to-delete' is a list of the
          ;; directories that will be automatically created by
          ;; `make-directory'. We will want to offer to delete these
          ;; directories if the user kills the buffer without saving
          ;; it.
          (dirs-to-delete ()))
      ;; If the file already exists, we don't need to worry about
      ;; creating any directories.
      (unless (file-exists-p filename)
        ;; It's easy to figure out how to invoke `make-directory',
        ;; because it will automatically create all parent
        ;; directories. We just need to ask for the directory
        ;; immediately containing the file to be created.
        (let* ((dir-to-create (file-name-directory filename))
               ;; However, to find the exact set of directories that
               ;; might need to be deleted afterward, we need to
               ;; iterate upward through the directory tree until we
               ;; find a directory that already exists, starting at
               ;; the directory containing the new file.
               (current-dir dir-to-create))
          ;; If the directory containing the new file already exists,
          ;; nothing needs to be created, and therefore nothing needs
          ;; to be destroyed, either.
          (while (not (file-exists-p current-dir))
            ;; Otherwise, we'll add that directory onto the list of
            ;; directories that are going to be created.
            (push current-dir dirs-to-delete)
            ;; Now we iterate upwards one directory. The
            ;; `directory-file-name' function removes the trailing
            ;; slash of the current directory, so that it is viewed as
            ;; a file, and then the `file-name-directory' function
            ;; returns the directory component in that path (which
            ;; means the parent directory).
            (setq current-dir (file-name-directory
                               (directory-file-name current-dir))))
          ;; Only bother trying to create a directory if one does not
          ;; already exist.
          (unless (file-exists-p dir-to-create)
            ;; Make the necessary directory and its parents.
            (make-directory dir-to-create 'parents))))
      ;; Call the original `find-file', now that the directory
      ;; containing the file to found exists. We make sure to preserve
      ;; the return value, so as not to mess up any commands relying
      ;; on it.
      (prog1 (apply find-file orig-filename args)
        ;; If there are directories we want to offer to delete later,
        ;; we have more to do.
        (when dirs-to-delete
          ;; Since we already called `find-file', we're now in the
          ;; buffer for the new file. That means we can transfer the
          ;; list of directories to possibly delete later into a
          ;; buffer-local variable. But we pushed new entries onto the
          ;; beginning of `dirs-to-delete', so now we have to reverse
          ;; it (in order to later offer to delete directories from
          ;; innermost to outermost).
          (setq-local radian--dirs-to-delete (reverse dirs-to-delete))
          ;; Now we add a buffer-local hook to offer to delete those
          ;; directories when the buffer is killed, but only if it's
          ;; appropriate to do so (for instance, only if the
          ;; directories still exist and the file still doesn't
          ;; exist).
          (add-hook 'kill-buffer-hook
                    #'radian--kill-buffer-delete-directory-if-appropriate
                    'append 'local)
          ;; The above hook removes itself when it is run, but that
          ;; will only happen when the buffer is killed (which might
          ;; never happen). Just for cleanliness, we automatically
          ;; remove it when the buffer is saved. This hook also
          ;; removes itself when run, in addition to removing the
          ;; above hook.
          (add-hook 'after-save-hook
                    #'radian--remove-kill-buffer-delete-directory-hook
                    'append 'local))))))

(defun radian--kill-buffer-delete-directory-if-appropriate ()
  "Delete parent directories if appropriate.
This is a function for `kill-buffer-hook'. If
`radian--advice-find-file-create-directories' created the
directory containing the file for the current buffer
automatically, then offer to delete it. Otherwise, do nothing.
Also clean up related hooks."
  (when (and
         ;; Stop if the local variables have been killed.
         (boundp 'radian--dirs-to-delete)
         ;; Stop if there aren't any directories to delete (shouldn't
         ;; happen).
         radian--dirs-to-delete
         ;; Stop if `radian--dirs-to-delete' somehow got set to
         ;; something other than a list (shouldn't happen).
         (listp radian--dirs-to-delete)
         ;; Stop if the current buffer doesn't represent a
         ;; file (shouldn't happen).
         buffer-file-name
         ;; Stop if the buffer has been saved, so that the file
         ;; actually exists now. This might happen if the buffer were
         ;; saved without `after-save-hook' running, or if the
         ;; `find-file'-like function called was `write-file'.
         (not (file-exists-p buffer-file-name)))
    (cl-dolist (dir-to-delete radian--dirs-to-delete)
      ;; Ignore any directories that no longer exist or are malformed.
      ;; We don't return immediately if there's a nonexistent
      ;; directory, because it might still be useful to offer to
      ;; delete other (parent) directories that should be deleted. But
      ;; this is an edge case.
      (when (and (stringp dir-to-delete)
                 (file-exists-p dir-to-delete))
        ;; Only delete a directory if the user is OK with it.
        (if (y-or-n-p (format "Also delete directory `%s'? "
                              ;; The `directory-file-name' function
                              ;; removes the trailing slash.
                              (directory-file-name dir-to-delete)))
            (delete-directory dir-to-delete)
          ;; If the user doesn't want to delete a directory, then they
          ;; obviously don't want to delete any of its parent
          ;; directories, either.
          (cl-return)))))
  ;; It shouldn't be necessary to remove this hook, since the buffer
  ;; is getting killed anyway, but just in case...
  (radian--remove-kill-buffer-delete-directory-hook))

(defun radian--remove-kill-buffer-delete-directory-hook ()
  "Clean up directory-deletion hooks, if necessary.
This is a function for `after-save-hook'. Remove
`radian--kill-buffer-delete-directory-if-appropriate' from
`kill-buffer-hook', and also remove this function from
`after-save-hook'."
  (remove-hook 'kill-buffer-hook
               #'radian--kill-buffer-delete-directory-if-appropriate
               'local)
  (remove-hook 'after-save-hook
               #'radian--remove-kill-buffer-delete-directory-hook
               'local))

(dolist (fun '(find-file           ; C-x C-f
               find-alternate-file ; C-x C-v
               write-file))        ; C-x C-w
  (advice-add fun :around #'radian--advice-find-file-create-directories))


;; Feature `saveplace' provides a minor mode for remembering the
;; location of point in each file you visit, and returning it there
;; when you find the file again.
(-ow saveplace
  :config
  (save-place-mode +1)

  (defadvice! radian--advice-save-place-quickly-and-silently
    (func &rest args)
    "Make `save-place' save more quickly and silently."
    :around #'save-place-alist-to-file
    (letf! ((#'pp #'prin1)
            (defun write-region (start end filename &optional append visit lockname mustbenew)
              (unless visit (setq visit 'no-message))
              (funcall write-region start end filename append visit lockname mustbenew)))
      (apply func args))))

(eval-cond!
  (*MAC
   ;; mac-* variables are used by the special emacs-mac build of Emacs by
   ;; Yamamoto Mitsuharu, while other builds use ns-*.
   (setq mac-command-modifier      'super
         mac-option-modifier       'meta
         ;; Free up the right option for character composition
         mac-right-option-modifier 'none))
  (*WINDOWS
   (setq w32-lwindow-modifier 'super
         w32-rwindow-modifier 'super)))


;; HACK Fixes Emacs' disturbing inability to distinguish C-i from TAB.
(define-key
 key-translation-map [?\C-i]
 (cmd! (if (let ((keys (this-single-command-raw-keys)))
             (and keys
                  (not (cl-position 'tab    keys))
                  (not (cl-position 'kp-tab keys))
                  (display-graphic-p)
                  ;; Fall back if no <C-i> keybind can be found, otherwise
                  ;; we've broken all pre-existing C-i keybinds.
                  (let ((key
                         (radian-lookup-key
                          (vconcat (cl-subseq keys 0 -1) [C-i]))))
                    (not (or (numberp key) (null key))))))
           [C-i] [?\C-i])))

(pow key-chord
  :config
  (fn-quiet! #'key-chord-mode +1)
  (setq key-chord-two-keys-delay 0.25))

(defadvice! radian--quoted-insert-allow-quit (quoted-insert &rest args)
  "Allow quitting out of \\[quoted-insert] with \\[keyboard-quit]."
  :around #'quoted-insert
  (letf! ((defun insert-and-inherit (&rest args)
            (dolist (arg args)
              (when (equal arg ?\C-g)
                (signal 'quit nil)))
            (apply insert-and-inherit args)))
    (apply quoted-insert args)))

(defvar radian-escape-hook nil
  "A hook run when C-g is pressed (or ESC in normal mode).

More specifically, when `radian/escape' is pressed. If any hook returns non-nil,
all hooks after it are ignored.")

(defun radian/escape (&optional interactive)
  "Run `radian-escape-hook'."
  (interactive (list 'interactive))
  (cond ((switch-to-buffer (window-buffer (active-minibuffer-window)))
         ;; quit the minibuffer if open.
         (cond
          ((featurep 'delsel)
           (progn
             (eval-when-compile (require 'delsel))
             (minibuffer-keyboard-quit)))
          ;; Emacs 28 and later
          ((fboundp 'abort-minibuffers)
           (abort-minibuffers))
          ;; Emacs 27 and earlier
          (t (abort-recursive-edit))))
        ;; Run all escape hooks. If any returns non-nil, then stop there.
        ((run-hook-with-args-until-success 'radian-escape-hook))
        ;; don't abort macros
        ((or defining-kbd-macro executing-kbd-macro) nil)
        ;; Back to the default
        ((unwind-protect (keyboard-quit)
           (when interactive
             (setq this-command 'keyboard-quit))))))

(global-set-key [remap keyboard-quit] #'radian/escape)
(with-eval-after-load 'eldoc (eldoc-add-command 'radian/escape))


;;;; tty support
;; Keep window title up-to-date. Should fail gracefully in non-xterm terminals.
;; Only works in Emacs 27+.
(setq xterm-set-window-title t)

;; Some terminals offer two different cursors: a "visible" static cursor and a
;; "very visible" blinking one. By default, Emacs uses the very visible cursor
;; and will switch back to it when Emacs is started or resumed. A nil
;; `visible-cursor' prevents this.
(setq visible-cursor nil)

;; Enable the mouse in terminal Emacs
(add-hook 'tty-setup-hook #'xterm-mouse-mode)


;; Unix tools look for HOME, but this is normally not defined on Windows.
(when-let (realhome
           (and *WINDOWS
                (null (getenv-internal "HOME"))
                (getenv "USERPROFILE")))
  (setenv "HOME" realhome)
  (setq abbreviated-home-dir nil))

(defvar radian--env-setup-p nil
  "Non-nil if `radian-env-setup' has completed at least once.")

(defun radian-env-setup (&optional again)
  "Load ~/.profile and set environment variables exported therein.
Only do this once, unless AGAIN is non-nil."
  (interactive (list 'again))
  ;; No need to worry about race conditions because Elisp isn't
  ;; concurrent (yet).
  (unless (and radian--env-setup-p (not again))
    (let (;; Current directory may not exist in certain horrifying
          ;; circumstances (yes, this has happened in practice).
          (default-directory "/")
          (profile-file "~/.profile")
          (buf-name " *radian-env-output*"))
      (when (and profile-file
                 (file-exists-p profile-file)
                 (executable-find "python3"))
        (ignore-errors (kill-buffer buf-name))
        (with-current-buffer (get-buffer-create buf-name)
          (let* ((python-script
                  (expand-file-name "scripts/print_env.py" *radian-directory*))
                 (delimiter (radian--random-string))
                 (sh-script (format ". %s && %s %s"
                                    (shell-quote-argument
                                     (expand-file-name profile-file))
                                    (shell-quote-argument python-script)
                                    (shell-quote-argument delimiter)))
                 (return (call-process "sh" nil t nil "-c" sh-script))
                 (found-delimiter
                  (progn
                    (goto-char (point-min))
                    (search-forward delimiter nil 'noerror))))
            (if (and (= 0 return) found-delimiter)
                (let* ((results (split-string
                                 (buffer-string) (regexp-quote delimiter)))
                       (results (cl-subseq results 1 (1- (length results)))))
                  (if (cl-evenp (length results))
                      (progn
                        (cl-loop for (var value) on results by #'cddr do
                                 (setenv var value)
                                 (when (string= var "PATH")
                                   (setq exec-path (append
                                                    (parse-colon-path value)
                                                    (list exec-directory)))))
                        (setq radian--env-setup-p t))
                    (message
                     "Loading %s produced malformed result; see buffer %S"
                     profile-file
                     buf-name)))
              (message "Failed to load %s; see buffer %S"
                       profile-file
                       buf-name))))))))

(defvar radian--env-setup-timer (run-at-time 1 nil #'radian-env-setup)
  "Timer used to run `radian-env-setup'.
We (mostly) don't need environment variables to be set correctly
during init, so deferring their processing saves some time at
startup.")

;;;; Clipboard integration
;; On macOS, clipboard integration works out of the box in windowed
;; mode but not terminal mode. The following code to fix it was
;; originally based on [1], and then modified based on [2].
;;
;; [1]: https://gist.github.com/the-kenny/267162
;; [2]: https://emacs.stackexchange.com/q/26471/12534
(when *MAC
  (unless (display-graphic-p)

    (defvar radian--clipboard-last-copy nil
      "The last text that was copied to the system clipboard.
This is used to prevent duplicate entries in the kill ring.")

    (eval-and-compile
      (defun radian--clipboard-paste ()
        "Return the contents of the macOS clipboard, as a string."
        (let* (;; Setting `default-directory' to a directory that is
               ;; sure to exist means that this code won't error out
               ;; when the directory for the current buffer does not
               ;; exist.
               (default-directory "/")
               ;; Command pbpaste returns the clipboard contents as a
               ;; string.
               (text (shell-command-to-string "pbpaste")))
          ;; If this function returns nil then the system clipboard is
          ;; ignored and the first element in the kill ring (which, if
          ;; the system clipboard has not been modified since the last
          ;; kill, will be the same) is used instead. Including this
          ;; `unless' clause prevents you from getting the same text
          ;; yanked the first time you run `yank-pop'.
          (unless (string= text radian--clipboard-last-copy)
            text)))

      (defun radian--clipboard-copy (text)
        "Set the contents of the macOS clipboard to given TEXT string."
        (let* (;; Setting `default-directory' to a directory that is
               ;; sure to exist means that this code won't error out
               ;; when the directory for the current buffer does not
               ;; exist.
               (default-directory "/")
               ;; Setting `process-connection-type' makes Emacs use a pipe to
               ;; communicate with pbcopy, rather than a pty (which is
               ;; overkill).
               (process-connection-type nil)
               ;; The nil argument tells Emacs to discard stdout and
               ;; stderr. Note, we aren't using `call-process' here
               ;; because we want this command to be asynchronous.
               ;;
               ;; Command pbcopy writes stdin to the clipboard until it
               ;; receives EOF.
               (proc (start-process "pbcopy" nil "pbcopy")))
          (process-send-string proc text)
          (process-send-eof proc))
        (setq radian--clipboard-last-copy text)))

    (setq interprogram-paste-function #'radian--clipboard-paste)
    (setq interprogram-cut-function #'radian--clipboard-copy)))

;; Allow UTF or composed text from the clipboard, even in the terminal or on
;; non-X systems (like Windows or macOS), where only `STRING' is used.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; If you have something on the system clipboard, and then kill
;; something in Emacs, then by default whatever you had on the system
;; clipboard is gone and there is no way to get it back. Setting the
;; following option makes it so that when you kill something in Emacs,
;; whatever was previously on the system clipboard is pushed into the
;; kill ring. This way, you can paste it with `yank-pop'.
(setq save-interprogram-paste-before-kill t)

(defadvice! radian--advice-gui-get-selection-quietly (func &rest args)
  "Disable an annoying message emitted when Emacs can't yank something.
In particular, if you have an image on your system clipboard and
you either yank or kill (as `save-interprogram-paste-before-kill'
means Emacs will try to put the system clipboard contents into
the kill ring when you kill something new), you'll get the
message 'gui-get-selection: (error \"Selection owner couldn't
convert\" UTF8_STRING)'. Disable that."
  :around #'gui-selection-value
  (regx-quiet! "Selection owner couldn't convert"
    (apply func args)))

;;;; Window management
;; Prevent accidental usage of `list-buffers'.
(-keys (("C-x C-b" . switch-to-buffer)
        ("C-x b"   . list-buffers)
        ("C-x C-k" . kill-buffer)))

;; Feature `winner' provides an undo/redo stack for window
;; configurations, with undo and redo being C-c left and C-c right,
;; respectively. (Actually "redo" doesn't revert a single undo, but
;; rather a whole sequence of them.) For instance, you can use C-x 1
;; to focus on a particular window, then return to your previous
;; layout with C-c left.
(-ow winner
  ;; undo/redo changes to Emacs' window layout
  :preface (defvar winner-dont-bind-my-keys t) ; I'll bind keys myself
  :hook radian-first-buffer-hook
  :config
  (appendq! winner-boring-buffers
            '("*Compile-Log*" "*inferior-lisp*" "*Fuzzy Completions*"
              "*Apropos*" "*Help*" "*cvs*" "*Buffer List*" "*Ibuffer*"
              "*esh command on file*")))

;; slip window
(defun split-window-func-with-other-buffer (split-function)
  (lambda (&optional arg)
    "Split this window and switch to the new window unless ARG is provided."
    (interactive "P")
    (funcall split-function)
    (let ((target-window (next-window)))
      (set-window-buffer target-window (other-buffer))
      (unless arg
        (select-window target-window)))))

(-keys
 (("C-x C-)" . (split-window-func-with-other-buffer 'split-window-vertically))
  ("C-x C-*" . (split-window-func-with-other-buffer 'split-window-horizontally))))

(defun radian/toggle-delete-other-windows ()
  "Delete other windows in frame if any, or restore previous window config."
  (interactive)
  (if (and winner-mode (equal (selected-window) (next-window)))
      (winner-undo)
    (delete-other-windows)))

(-key "C-x C-(" #'radian/toggle-delete-other-windows)

;; Rearrange split windows

(defun split-window-horizontally-instead ()
  "Kill any other windows and re-split such that the current window is on the top half of the frame."
  (interactive)
  (let ((other-buffer (and (next-window) (window-buffer (next-window)))))
    (delete-other-windows)
    (split-window-horizontally)
    (when other-buffer
      (set-window-buffer (next-window) other-buffer))))

(defun split-window-vertically-instead ()
  "Kill any other windows and re-split such that the current window is on the left half of the frame."
  (interactive)
  (let ((other-buffer (and (next-window) (window-buffer (next-window)))))
    (delete-other-windows)
    (split-window-vertically)
    (when other-buffer
      (set-window-buffer (next-window) other-buffer))))

(-keys (("C-x \\" . split-window-horizontally-instead)
        ("C-x _" . split-window-vertically-instead)))

(defun radian/split-window()
  "Split the window to see the most recent buffer in the other window.
Call a second time to restore the original window configuration."
  (interactive)
  (if (eq last-command 'radian/split-window)
      (progn
        (jump-to-register :radian/split-window)
        (setq this-command 'radian/unsplit-window))
    (window-configuration-to-register :radian/split-window)
    (switch-to-buffer-other-window nil)))

(-key "<f7>" #'radian/split-window)

(defun radian/toggle-current-window-dedication ()
  "Toggle whether the current window is dedicated to its current buffer."
  (interactive)
  (let* ((window (selected-window))
         (was-dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not was-dedicated))
    (message "Window %sdedicated to %s"
             (if was-dedicated "no longer " "")
             (buffer-name))))

(-key "C-c <down>" #'radian/toggle-current-window-dedication)

(declare-function minibuffer-keyboard-quit "delsel")
(defadvice! radian--advice-keyboard-quit-minibuffer-first
  (keyboard-quit)
  "Cause \\[keyboard-quit] to exit the minibuffer, if it is active.
Normally, \\[keyboard-quit] will just act in the current buffer.
This advice modifies the behavior so that it will instead exit an
active minibuffer, even if the minibuffer is not selected."
  :around #'keyboard-quit
  (if-let ((minibuffer (active-minibuffer-window)))
      (with-current-buffer (window-buffer minibuffer)
        (minibuffer-keyboard-quit))
    (funcall keyboard-quit)))

(defadvice! radian--advice-kill-buffer-maybe-kill-window
  (func &optional buffer-or-name kill-window-too)
  "Make it so \\[universal-argument] \\[kill-buffer] kills the window too."
  :around #'kill-buffer
  (interactive
   (lambda (spec)
     (append (or (advice-eval-interactive-spec spec) '(nil))
             current-prefix-arg)))
  (if kill-window-too
      (with-current-buffer buffer-or-name
        (kill-buffer-and-window))
    (funcall func buffer-or-name)))


;; Package `swsw' provides lightway to navigate windows.
(-ow swsw
  :straight (swsw :repo "https://git.sr.ht/~dsemy/swsw")
  :hook (radian-first-input-hook . swsw-mode)
  :chord (",," . swsw-select)
  :bind
  ("C-x C-o" . swsw-select)
  (swsw-command-map ([?c] . swsw-delete))
  :custom
  (swsw-id-chars . '(?a ?o ?e ?u ?h ?t ?n ?s))
  (swsw-id-format . "%s")
  (swsw-display-function . #'ignore)
  :init
  (defvar swsw-char-position 'bottom)
  (defvar swsw--overlays nil)
  (defvar swsw--empty-buffers-list nil)
  (defvar swsw--windows-hscroll nil)
  :config
  (customize-set-variable 'swsw-scope 'visible)
  (defun swsw--point-visible-p ()
    "Return non-nil if point is visible in the selected window.
Return nil when horizontal scrolling has moved it off screen."
    (and (>= (- (current-column) (window-hscroll)) 0)
         (< (- (current-column) (window-hscroll))
            (window-width))))

  (defun swsw--remove-id-overlay ()
    "Remove leading char overlays."
    (mapc #'delete-overlay swsw--overlays)
    (setq swsw--overlays nil)
    (dolist (b swsw--empty-buffers-list)
      (with-current-buffer b
        (when (string= (buffer-string) " ")
          (let ((inhibit-read-only t))
            (delete-region (point-min) (point-max))))))
    (setq swsw--empty-buffers-list nil)
    (let (wnd hscroll)
      (mapc (lambda (wnd-and-hscroll)
              (setq wnd (car wnd-and-hscroll)
                    hscroll (cdr wnd-and-hscroll))
              (when (window-live-p wnd)
                (set-window-hscroll wnd hscroll)))
            swsw--windows-hscroll))
    (setq swsw--windows-hscroll nil))

  (defun swsw--display-id-overlay ()
    "Create an overlay on every window."
    ;; Properly adds overlay in visible region of most windows except for any one
    ;; receiving output while this function is executing, since that moves point,
    ;; potentially shifting the added overlay outside the window's visible region.
    (cl-flet
        ((swsw--id-overlay (wnd)
           ;; Prevent temporary movement of point from scrolling any window.
           (let ((scroll-margin 0))
             (with-selected-window wnd
               (when (= 0 (buffer-size))
                 (push (current-buffer) swsw--empty-buffers-list)
                 (let ((inhibit-read-only t))
                   (insert " ")))
               ;; If point is not visible due to horizontal scrolling of the
               ;; window, this next expression temporarily scrolls the window
               ;; right until point is visible, so that the leading-char can be
               ;; seen when it is inserted.  When ace-window's action finishes,
               ;; the horizontal scroll is restored.
               (while (and (not (swsw--point-visible-p))
                           (not (zerop (window-hscroll)))
                           (progn (push (cons (selected-window) (window-hscroll))
                                        swsw--windows-hscroll) t)
                           (not (zerop (scroll-right)))))
               (let* ((ws (window-start))
                      (prev nil)
                      (vertical-pos (if (eq swsw-char-position 'bottom) -1 0))
                      (horizontal-pos (if (zerop (window-hscroll)) 0 (1+ (window-hscroll))))
                      (old-pt (point))
                      (pt
                       (progn
                         ;; If leading-char is to be displayed at the top-left, move
                         ;; to the first visible line in the window, otherwise, move
                         ;; to the last visible line.
                         (move-to-window-line vertical-pos)
                         (move-to-column horizontal-pos)
                         ;; Find a nearby point that is not at the end-of-line but
                         ;; is visible so have space for the overlay.
                         (setq prev (1- (point)))
                         (while (and (>= prev ws) (/= prev (point)) (eolp))
                           (setq prev (point))
                           (unless (bobp)
                             (line-move -1 t)
                             (move-to-column horizontal-pos)))
                         (recenter vertical-pos)
                         (point)))
                      (ol (make-overlay pt (1+ pt) (window-buffer wnd))))
                 (goto-char old-pt)
                 (overlay-put ol 'display (swsw-format-id wnd))
                 (overlay-put ol 'window wnd)
                 (push ol swsw--overlays))))))
      (walk-windows #'swsw--id-overlay nil (swsw--get-scope))))
  ;; HACK: use overlay to display swsw id.
  (add-hook 'swsw-before-command-hook #'swsw--display-id-overlay)
  (add-hook 'swsw-after-command-hook #'swsw--remove-id-overlay)

  (defadvice! swsw-format-id-a (id)
    "Format an ID string for WINDOW."
    :filter-return #'swsw-format-id
    (propertize
     (upcase id)
     'face
     `(:foreground "deep pink" :weight extra-bold :height ,(+ 30 radian-font-size))))

  :blackout t)

;; Feature `windmove' provides keybindings S-left, S-right, S-up, and
;; S-down to move between windows. This is much more convenient and
;; efficient than using the default binding, C-x o, to cycle through
;; all of them in an essentially unpredictable order.
(-ow windmove
  ;; Avoid using `windmove-default-keybindings' due to
  ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=50430.
  :bind
  ("S-<left>"  . windmove-swap-states-left)
  ("S-<right>" . windmove-swap-states-right)
  ("S-<up>"    . windmove-swap-states-up)
  ("S-<down>"  . windmove-swap-states-down))

;; Feature `ibuffer' provides a more modern replacement for the
;; `list-buffers' command.
(-ow ibuffer
  :bind (([remap list-buffers] . ibuffer))
  :config
  (setq ibuffer-expert t)
  (setq ibuffer-display-summary nil)
  (setq ibuffer-use-other-window nil)
  (setq ibuffer-show-empty-filter-groups nil)
  (setq ibuffer-movement-cycle nil)
  (setq ibuffer-default-sorting-mode 'filename/process)
  (setq ibuffer-use-header-line t)
  (setq ibuffer-default-shrink-to-minimum-size nil)
  (setq ibuffer-formats
        '((mark modified read-only locked " "
                (name 40 40 :left :elide)
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " " filename-and-process)
          (mark " "
                (name 16 -1)
                " " filename)))
  (setq ibuffer-saved-filter-groups nil)
  (setq ibuffer-old-time 48))

(defadvice! radian--advice-eval-expression-save-garbage
  (func prompt &optional initial-contents keymap read &rest args)
  "Save user input in history even if it's not a valid sexp.
We do this by forcing `read-from-minibuffer' to always be called
with a nil value for READ, and then handling the effects of READ
ourselves."
  :around #'read-from-minibuffer
  (let ((input (apply func prompt initial-contents keymap nil args)))
    (when read
      ;; This is based on string_to_object in minibuf.c.
      (let ((result (read-from-string input)))
        (unless (string-match-p
                 "\\`[ \t\n]*\\'" (substring input (cdr result)))
          (signal
           'invalid-read-syntax
           '("Trailing garbage following expression")))
        (setq input (car result))))
    input))

;;;; Complation supported by `vertico'
(-ow vertico
  :straight (vertico :host github :repo "minad/vertico"
                     :files ("*.el" "extensions/*.el"))
  :bind (radian-comma-keymap ("&" . vertico-repeat))
  :hook radian-first-input-hook
  :chord (:vertico-map (".." . vertico-quick-exit))
  :config
  (defvar +vertico-company-completion-styles '(basic partial-completion orderless)
    "Completion styles for company to use.

The completion/vertico module uses the orderless completion style by default,
but this returns too broad a candidate set for company completion. This variable
overrides `completion-styles' during company completion sessions.")

  (setq vertico-resize nil
        vertico-count 17
        vertico-cycle t
        completion-in-region-function
        (lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args)))
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
  (-key "DEL" #'vertico-directory-delete-char 'vertico-map)) ;backspace key

;;;; Orderless
(pow orderless
  :aftercall radian-first-input-hook
  :config
  (defadvice! +vertico--company-capf--candidates-a (fn &rest args)
    "Highlight company matches correctly, and try default completion styles before
orderless."
    :around #'company-capf--candidates
    (let ((orderless-match-faces [completions-common-part])
          (completion-styles +vertico-company-completion-styles))
      (apply fn args)))

  (defun +vertico-orderless-dispatch (pattern _index _total)
    (cond
     ;; Ensure $ works with Consult commands, which add disambiguation suffixes
     ((string-suffix-p "$" pattern)
      `(orderless-regexp . ,(concat (substring pattern 0 -1) "[\x100000-\x10FFFD]*$")))
     ;; Ignore single !
     ((string= "!" pattern) `(orderless-literal . ""))
     ;; Without literal
     ((string-prefix-p "!" pattern) `(orderless-without-literal . ,(substring pattern 1)))
     ;; Character folding
     ((string-prefix-p "%" pattern) `(char-fold-to-regexp . ,(substring pattern 1)))
     ((string-suffix-p "%" pattern) `(char-fold-to-regexp . ,(substring pattern 0 -1)))
     ;; Initialism matching
     ((string-prefix-p "`" pattern) `(orderless-initialism . ,(substring pattern 1)))
     ((string-suffix-p "`" pattern) `(orderless-initialism . ,(substring pattern 0 -1)))
     ;; Literal matching
     ((string-prefix-p "=" pattern) `(orderless-literal . ,(substring pattern 1)))
     ((string-suffix-p "=" pattern) `(orderless-literal . ,(substring pattern 0 -1)))
     ;; Flex matching
     ((string-prefix-p "~" pattern) `(orderless-flex . ,(substring pattern 1)))
     ((string-suffix-p "~" pattern) `(orderless-flex . ,(substring pattern 0 -1)))))

  (setq completion-styles '(orderless)
        completion-category-defaults nil
        ;; note that despite override in the name orderless can still be used in
        ;; find-file etc.
        completion-category-overrides '((file (styles orderless partial-completion)))
        orderless-style-dispatchers '(+vertico-orderless-dispatch)
        orderless-component-separator "[ &]")
  ;; ...otherwise find-file gets different highlighting than other commands
  (set-face-attribute 'completions-first-difference nil :inherit nil))

;;;; isearch
(-ow isearch
  :init
  (defun consult-line-or-visit-p ()
    (<= (buffer-size)
        (/ 300000 (if (eq major-mode 'org-mode) 2 1))))

  (defun consult-line-or-isearch ()
    "Use `consult-line' for little file and `isearch' for larger one"
    (interactive)
    (call-interactively (if (and (fboundp 'consult-line) (consult-line-or-visit-p))
                            #'consult-line
                          #'isearch-forward)))

  ;; use my prefer search workflow.
  :bind ((minibuffer-local-isearch-map
          ([?\t]      . isearch-complete-edit)
          ("\r"       . isearch-forward-exit-minibuffer))
         (isearch-mode-map
          ([remap isearch-delete-char] . isearch-del-char)
          ("M-s -"    . isearch-toggle-symbol)
          ("M-s a"    . isearch-beginning-of-buffer)
          ("M-s e"    . isearch-end-of-buffer)
          ("M-s ."    . isearch-forward-symbol-at-point)
          ("M-s t"    . isearch-forward-thing-at-point)
          ("<escape>" . isearch-abort)
          (","        . isearch-repeat-backward)
          ("."        . isearch-repeat-forward)
          ("/"        . isearch-edit-string)))

  :config
  (setq isearch-allow-motion t
        isearch-repeat-on-direction-change t
        isearch-motion-changes-direction t)

  :blackout t)

;;;; auth-source
;; Feature `auth-source' reads and writes secrets from files like
;; ~/.netrc for TRAMP and related packages, so for example you can
;; avoid having to type in a particular host's password every time.
(-ow auth-source
  :config
  ;; Emacs stores `authinfo' in $HOME and in plain-text. Let's not do that, mkay?
  ;; This file stores usernames, passwords, and other such treasures for the
  ;; aspiring malicious third party.
  ;; (setq auth-sources (list "~/.authinfo.gpg"))

  (defvar radian--auth-source-blacklist-file
    (no-littering-expand-var-file-name "auth-source/blacklist.el")
    "File to store `auth-source' user blacklist.
The contents are a list of MD5 hashes, one for each potential
password that the user has decided not to save.")

  (defadvice! radian--advice-auth-source-persist-blacklist
    (func file add)
    "Allow user to permanently disable prompt to save credentials."
    :around #'auth-source-netrc-saver
    (let* ((key (format "%s %s" file (rfc2104-hash 'md5 64 16 file add)))
           (blacklist
            (ignore-errors
              (with-temp-buffer
                (insert-file-contents radian--auth-source-blacklist-file)
                (read (current-buffer))))))
      (unless (listp blacklist)
        (setq blacklist nil))
      (if (member key blacklist)
          ?n
        (letf! ((defun auth-source-read-char-choice (prompt choices)
                  (let ((choice (funcall auth-source-read-char-choice
                                         prompt choices)))
                    (when (= choice ?N)
                      (push key blacklist)
                      (make-directory
                       (file-name-directory
                        radian--auth-source-blacklist-file)
                       'parents)
                      (with-temp-file radian--auth-source-blacklist-file
                        (print blacklist (current-buffer)))
                      (setq choice ?n))
                    choice)))
          (funcall func file add))))))

(setq create-lockfiles nil
      make-backup-files nil
      ;; But in case the user does enable it, some sensible defaults:
      version-control t     ; number each backup file
      backup-by-copying t   ; instead of renaming current file (clobbers links)
      delete-old-versions t ; clean up after itself
      kept-old-versions 5
      kept-new-versions 5
      tramp-backup-directory-alist backup-directory-alist)

;; But turn on auto-save, so we have a fallback in case of crashes or lost data.
;; Use `recover-file' or `recover-session' to recover them.
(setq auto-save-default t
      ;; Don't auto-disable auto-save after deleting big chunks. This defeats
      ;; the purpose of a failsafe. This adds the risk of losing the data we
      ;; just deleted, but I believe that's VCS's jurisdiction, not ours.
      auto-save-include-big-deletions t
      auto-save-file-name-transforms
      (list (list "\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
                  ;; Prefix tramp autosaves to prevent conflicts with local ones
                  (concat auto-save-list-file-prefix "tramp-\\2") t)
            (list ".*" auto-save-list-file-prefix t)))

(defun radian-set-executable-permission (allowed)
  "Enable or disable executable permission on the current file.
If ALLOWED is non-nil, enable permission; otherwise, disable
permission."
  (interactive (list (not current-prefix-arg)))
  (unless buffer-file-name
    (user-error "This buffer is not visiting a file"))
  (with-demoted-errors "Could not set permissions: %S"
    (set-file-modes buffer-file-name (file-modes-symbolic-to-number
                                      (if allowed
                                          "+x"
                                        "-x")
                                      (file-modes buffer-file-name)))
    (message "Executable permission %s"
             (if allowed "enabled" "disabled"))))

(-key* "s-x" #'radian-set-executable-permission)

;;;; recentf-mode
(-ow recentf
  :hook (radian-first-file-hook . (lambda () (fn-quiet! #'recentf-mode)))
  :increment easymenu tree-widget timer
  :custom
  (recentf-max-saved-items . 100)
  (recentf-exclude . '(".gz" ".xz" ".zip" "/elpa/" "/ssh:" "/sudo:"))
  ;; The most sensible time to clean up your recent files list is when you quit
  ;; Emacs (unless this is a long-running daemon session).
  `(recentf-auto-cleanup . ,(if (daemonp) 300 nil))
  :commands recentf-open-files
  ;; Set history-length longer
  :setq-default (history-length . 100)
  :config (add-hook 'kill-emacs-hook (lambda () (fn-quiet! #'recentf-cleanup))))

;;; MODULE {Editing}
;;;; Text formatting

(add-to-list 'safe-local-variable-values '(auto-fill-function . nil))

(add-to-list 'safe-local-eval-forms '(visual-line-mode +1))

(blackout 'visual-line-mode)

;; When region is active, make `capitalize-word' and friends act on
;; it.
(-key "M-c" #'capitalize-dwim)
(-key "M-l" #'downcase-dwim)
(-key "M-u" #'upcase-dwim)

(defun radian-reverse-region-characters (beg end)
  "Reverse the characters in the region from BEG to END.
Interactively, reverse the characters in the current region."
  (interactive "*r")
  (insert (reverse (delete-and-extract-region beg end))))

;; When filling paragraphs, assume that sentences end with one space
;; rather than two.
(setq sentence-end-double-space nil)

;; Trigger auto-fill after punctutation characters, not just
;; whitespace.
(mapc
 (lambda (c) (set-char-table-range auto-fill-chars c t))
 "!-=+]};:'\",.?")

;; We could maybe use the variable `comment-auto-fill-only-comments'
;; for this, but I wrote this code before I knew about it. Also, I'm
;; not sure how well it handles the edge cases for docstrings and
;; such.
(eval-when! (version<= "26" emacs-version)
  (defadvice! radian--advice-auto-fill-only-text (func &rest args)
    "Only perform auto-fill in text, comments, or docstrings."
    :around #'internal-auto-fill
    (cl-block nil
      ;; Don't auto-fill on the first line of a docstring, since it
      ;; shouldn't be wrapped into the body.
      (when (and (derived-mode-p #'emacs-lisp-mode)
                 (eq (get-text-property (point) 'face) 'font-lock-doc-face)
                 (save-excursion
                   (beginning-of-line)
                   (looking-at-p "[[:space:]]*\"")))
        (cl-return))
      (when (and (derived-mode-p 'text-mode)
                 (not (derived-mode-p 'yaml-mode)))
        (apply func args)
        (cl-return))
      ;; Inspired by <https://emacs.stackexchange.com/a/14716/12534>.
      (when-let ((faces (save-excursion
                          ;; In `web-mode', the end of the line isn't
                          ;; fontified, so we have to step backwards
                          ;; by one character before checking the
                          ;; properties.
                          (ignore-errors
                            (backward-char))
                          (get-text-property (point) 'face))))
        (unless (listp faces)
          (setq faces (list faces)))
        (when (cl-some
               (lambda (face)
                 (memq face '(font-lock-comment-face
                              font-lock-comment-delimiter-face
                              font-lock-doc-face
                              web-mode-javascript-comment-face)))
               faces)
          ;; Fill Elisp docstrings to the appropriate column. Why
          ;; docstrings are filled to a different column, I don't know.
          (let ((fill-column (if (and
                                  (derived-mode-p #'emacs-lisp-mode)
                                  (memq 'font-lock-doc-face faces))
                                 emacs-lisp-docstring-fill-column
                               fill-column)))
            (apply func args)))))))

(blackout 'auto-fill-mode)

(defun radian--do-auto-fill ()
  "Replacement for `do-auto-fill' that respects `normal-auto-fill-function'.
The reason we need this is that in order to enable auto-fill
globally, we are supposed to set the default value of variable
`auto-fill-function'. However, some major modes set
`normal-auto-fill-function' (itself normally set to
`do-auto-fill', which is what we generally set the default value
of variable `auto-fill-function' to), expecting `auto-fill-mode'
to be enabled afterwards (which copies the value of
`normal-auto-fill-function' into variable `auto-fill-function').
However, since we enable auto-fill globally by means of setting
variable `auto-fill-function' directly, this setting gets lost.
The workaround is to set variable `auto-fill-function' globally
to a function which looks up the value of
`normal-auto-fill-function' \(generally just `do-auto-fill') and
calls that. This is a slight inversion of the usual flow of
control and might make you slightly uncomfortable, but we'll just
have to live with it :3"
  (funcall normal-auto-fill-function))

;; https://www.gnu.org/software/emacs/manual/html_node/efaq/Turning-on-auto_002dfill-by-default.html
(setq-default auto-fill-function #'radian--do-auto-fill)

(define-minor-mode radian-fix-whitespace-mode
  "Minor mode to automatically fix whitespace on save.
If enabled, then saving the buffer deletes all trailing
whitespace and ensures that the file ends with exactly one
newline."
  :global t
  (if radian-fix-whitespace-mode
      (progn
        (setq require-final-newline t)
        (add-hook 'before-save-hook #'delete-trailing-whitespace nil 'local))
    (setq require-final-newline nil)
    (remove-hook 'before-save-hook #'delete-trailing-whitespace 'local)))

(define-globalized-minor-mode radian-fix-whitespace-global-mode
  radian-fix-whitespace-mode radian-fix-whitespace-mode)

(radian-fix-whitespace-global-mode +1)

(put 'radian-fix-whitespace-mode 'safe-local-variable #'booleanp)

;; Feature `whitespace' provides a minor mode for highlighting
;; whitespace in various special ways.
;;;; Whitespace
(-ow whitespace
  :init
  (defun radian-highlight-non-default-indentation-h ()
    "Highlight whitespace at odds with `indent-tabs-mode'.
That is, highlight tabs if `indent-tabs-mode' is `nil', and highlight spaces at
the beginnings of lines if `indent-tabs-mode' is `t'. The purpose is to make
incorrect indentation in the current buffer obvious to you.

Does nothing if `whitespace-mode' or `global-whitespace-mode' is already active
or if the current buffer is read-only or not file-visiting."
    (unless (or (eq major-mode 'fundamental-mode)
                (bound-and-true-p global-whitespace-mode)
                (null buffer-file-name))
      (require 'whitespace)
      (set (make-local-variable 'whitespace-style)
           (cl-union (if indent-tabs-mode
                         '(indentation)
                       '(tabs tab-mark))
                     (when whitespace-mode
                       (remq 'face whitespace-active-style))))
      (cl-pushnew 'face whitespace-style) ; must be first
      (whitespace-mode +1)))

  (define-minor-mode radian-highlight-long-lines-mode
    "Minor mode for highlighting long lines."
    :after-hook
    (if radian-highlight-long-lines-mode
        (progn
          (setq-local whitespace-style '(face lines-tail))
          (setq-local whitespace-line-column 79)
          (whitespace-mode +1))
      (whitespace-mode -1)
      (kill-local-variable 'whitespace-style)
      (kill-local-variable 'whitespace-line-column)))

  (add-hook 'prog-mode-hook #'radian-highlight-long-lines-mode)
  (defun toggle-radian-highlight-long-lines-mode ()
    (if radian-highlight-long-lines-mode
        (radian-highlight-long-lines-mode -1)
      (radian-highlight-long-lines-mode +1)))
  (add-hook! '(ediff-prepare-buffer-hook ediff-quit-hook) #'toggle-radian-highlight-long-lines-mode)
  :blackout t)

;;;; Kill and yank
(defadvice! radian--advice-stop-kill-at-whitespace
  (kill-line &rest args)
  "Prevent `kill-line' from killing through whitespace to a newline.
This affects the case where you press \\[kill-line] when point is
followed by some whitespace and then a newline. Without this
advice, \\[kill-line] will kill both the whitespace and the
newline, which is inconsistent with its behavior when the
whitespace is replaced with non-whitespace. With this advice,
\\[kill-line] will kill just the whitespace, and another
invocation will kill the newline."
  :around #'kill-line
  (let ((show-trailing-whitespace t))
    (apply kill-line args)))

;; Eliminate duplicates in the kill ring. That is, if you kill the
;; same thing twice, you won't have to use M-y twice to get past it to
;; older entries in the kill ring.
(setq kill-do-not-save-duplicates t)

(defadvice! radian--advice-disallow-password-copying (func &rest args)
  "Don't allow copying a password to the kill ring."
  :around #'read-passwd
  (cl-letf (((symbol-function #'kill-new) #'ignore)
            ((symbol-function #'kill-append) #'ignore))
    (apply func args)))

;; Feature `delsel' provides an alternative behavior for certain
;; actions when you have a selection active. Namely: if you start
;; typing when you have something selected, then the selection will be
;; deleted; and if you press DEL while you have something selected, it
;; will be deleted rather than killed. (Otherwise, in both cases the
;; selection is deselected and the normal function of the key is
;; performed.)
(-ow delsel :config (delete-selection-mode -1))

;;;; Undo/redo

(-ow repeat
  :init
  (defvar radian-repeat-exclude-commands
    '(meow-next meow-prev meow-left meow-right meow-block meow-line)
    "Dont repeat commands")

  :config
  (defadvice! radian-repeat-a (_)
    "Don't repeat some command"
    :before #'repeat
    (if (memql last-repeatable-command radian-repeat-exclude-commands)
        (setq last-repeatable-command repeat-previous-repeated-command))))

;; Feature `warnings' allows us to enable and disable warnings.
(-ow warnings
  :require t
  :config
  ;; Ignore the warning we get when a huge buffer is reverted and the
  ;; undo information is too large to be recorded.
  (add-to-list 'warning-suppress-log-types '(undo discard-info)))

;; Feature `bookmark' provides a way to mark places in a buffer. I
;; don't use it, but some other packages do.
(-ow bookmark
  :config

  (dolist (func '(bookmark-load bookmark-write-file))
    (advice-add func :around #'fn-quiet!)))


;;;; `cua' rectangle edit
(-ow cua-base
  :init (cua-selection-mode t)
  ;; disable `delete-selection-mode'
  :custom (cua-delete-selection . nil))

;;;; ediff
(after! ediff
  (setq ediff-diff-options "-w" ; turn off whitespace checking
        ediff-split-window-function #'split-window-horizontally
        ediff-window-setup-function #'ediff-setup-windows-plain)
  (defvar radian--ediff-saved-wconf nil)
  ;; Restore window config after quitting ediff
  (add-hook! 'ediff-before-setup-hook
    (defun radian-ediff-save-wconf-h ()
      (setq radian--ediff-saved-wconf (current-window-configuration))))
  (add-hook! '(ediff-quit-hook ediff-suspend-hook) :append
    (defun radian-ediff-restore-wconf-h ()
      (when (window-configuration-p radian--ediff-saved-wconf)
        (set-window-configuration radian--ediff-saved-wconf)))))

;;;; server
(-ow server
  :when (display-graphic-p)
  :aftercall radian-first-input-hook radian-first-file-hook focus-out-hook
  :init
  (when-let (name (getenv "EMACS_SERVER_NAME"))
    (setq server-name name))
  :defer-config
  (unless (server-running-p)
    (server-start)))

;;;; tramp
(-ow tramp
  :init
  (unless *WINDOWS
    (setq tramp-default-method "ssh")) ; faster than the default scp
  :defer-config
  (setq remote-file-name-inhibit-cache 60
        tramp-verbose 1
        vc-ignore-dir-regexp (format "%s\\|%s\\|%s"
                                     vc-ignore-dir-regexp
                                     tramp-file-name-regexp
                                     "[/\\\\]node_modules")))

;;;; so-long
(-ow so-long
  :hook (radian-first-file-hook . global-so-long-mode)
  :config
  ;; Emacs 29 introduced faster long-line detection, so they can afford a much
  ;; larger `so-long-threshold' and its default `so-long-predicate'.
  (if (fboundp 'buffer-line-statistics)
      (unless *NATIVECOMP (setq so-long-threshold 5000))
    ;; reduce false positives w/ larger threshold
    (setq so-long-threshold 400)

    (defun radian-buffer-has-long-lines-p ()
      (unless (bound-and-true-p visual-line-mode)
        (let ((so-long-skip-leading-comments
               ;; HACK Fix #2183: `so-long-detected-long-line-p' calls
               ;;   `comment-forward' which tries to use comment syntax, which
               ;;   throws an error if comment state isn't initialized, leading
               ;;   to a wrong-type-argument: stringp error.
               ;; DEPRECATED Fixed in Emacs 28.
               (bound-and-true-p comment-use-syntax)))
          (so-long-detected-long-line-p))))
    (setq so-long-predicate 'radian-buffer-has-long-lines-p))
  ;; Don't disable syntax highlighting and line numbers, or make the buffer
  ;; read-only, in `so-long-minor-mode', so we can have a basic editing
  ;; experience in them, at least. It will remain off in `so-long-mode',
  ;; however, because long files have a far bigger impact on Emacs performance.
  (delq! 'font-lock-mode so-long-minor-modes)
  (delq! 'display-line-numbers-mode so-long-minor-modes)
  (delq! 'buffer-read-only so-long-variable-overrides 'assq)
  ;; ...but at least reduce the level of syntax highlighting
  (add-to-list 'so-long-variable-overrides '(font-lock-maximum-decoration . 1))
  ;; ...and insist that save-place not operate in large/long files
  (add-to-list 'so-long-variable-overrides '(save-place-alist . nil))
  ;; But disable everything else that may be unnecessary/expensive for large or
  ;; wide buffers.
  (appendq! so-long-minor-modes
            '(spell-fu-mode
              eldoc-mode
              highlight-numbers-mode
              better-jumper-local-mode
              ws-butler-mode
              auto-composition-mode
              undo-tree-mode
              highlight-indent-guides-mode
              hl-fill-column-mode
              ;; These are redundant on Emacs 29+
              flycheck-mode
              smartparens-mode
              smartparens-strict-mode)))

;;;; prettify
(-ow prog-mode
  :init
  (defvar +ligatures-extras-in-modes t
    "List of major modes where extra ligatures should be enabled.

Extra ligatures are mode-specific substituions, defined in
`+ligatures-extra-symbols' and assigned with `set-ligatures!'. This variable
controls where these are enabled.

  If t, enable it everywhere (except `fundamental-mode').
  If the first element is 'not, enable it in any mode besides what is listed.
  If nil, don't enable these extra ligatures anywhere (though it's more
efficient to remove the `+extra' flag from the :ui ligatures module instead).")

  (defvar +ligatures-extra-alist '((t))
    "A map of major modes to symbol lists (for `prettify-symbols-alist').")

  (defun +ligatures--enable-p (modes)
    "Return t if ligatures should be enabled in this buffer depending on MODES."
    (unless (eq major-mode 'fundamental-mode)
      (or (eq modes t)
          (if (eq (car modes) 'not)
              (not (apply #'derived-mode-p (cdr modes)))
            (apply #'derived-mode-p modes)))))

  (setq prettify-symbols-alist '(("lambda" . "𝝀")))

  :defer-config
  (defun +ligatures-init-buffer-h ()
    "Set up ligatures for the current buffer.

Extra ligatures are mode-specific substituions, defined in
`+ligatures-extra-symbols', assigned with `set-ligatures!', and made possible
with `prettify-symbols-mode'. This variable controls where these are enabled.
See `+ligatures-extras-in-modes' to control what major modes this function can
and cannot run in."
    (when after-init-time
      (when (+ligatures--enable-p +ligatures-extras-in-modes)
        (prependq! prettify-symbols-alist
                   (alist-get major-mode +ligatures-extra-alist)))
      (when prettify-symbols-alist
        (if prettify-symbols-mode (prettify-symbols-mode -1))
        (prettify-symbols-mode +1))))

  ;; Ligatures Bootstrap
  (add-hook 'after-change-major-mode-hook #'+ligatures-init-buffer-h)
  ;; When you get to the right edge, it goes back to how it normally prints
  ;; (setq prettify-symbols-unprettify-at-point 'right-edge)
  (global-prettify-symbols-mode +1))

(-ow autorevert
  :init
  (defun radian--autorevert-silence ()
    "Silence messages from `auto-revert-mode' in the current buffer."
    (setq-local auto-revert-verbose nil))

  :hook (radian-first-file-hook . global-auto-revert-mode)
  :config

  ;; Turn the delay on auto-reloading from 5 seconds down to 1 second.
  ;; We have to do this before turning on `auto-revert-mode' for the
  ;; change to take effect. (Note that if we set this variable using
  ;; `customize-set-variable', all it does is toggle the mode off and
  ;; on again to make the change take effect, so that way is dumb.)
  (setq auto-revert-interval 1)

  (global-auto-revert-mode +1)

  ;; Auto-revert all buffers, not only file-visiting buffers. The
  ;; docstring warns about potential performance problems but this
  ;; should not be an issue since we only revert visible buffers.
  (setq global-auto-revert-non-file-buffers t)

  ;; Since we automatically revert all visible buffers after one
  ;; second, there's no point in asking the user whether or not they
  ;; want to do it when they find a file. This disables that prompt.
  (setq revert-without-query '(".*"))

  (defun radian-autorevert-inhibit-p (buffer)
    "Return non-nil if autorevert should be inhibited for BUFFER."
    (or (null (get-buffer-window))
        (with-current-buffer buffer
          (or (null buffer-file-name)
              (file-remote-p buffer-file-name)))))

  (eval-if! (version< emacs-version "27")
      (defadvice! radian--autorevert-only-visible
        (auto-revert-buffers &rest args)
        "Inhibit `autorevert' for buffers not displayed in any window."
        :around #'auto-revert-buffers
        (letf! ((defun buffer-list (&rest args)
                  (cl-remove-if
                   #'radian-autorevert-inhibit-p
                   (apply buffer-list args))))
          (apply auto-revert-buffers args)))
    (defadvice! radian--autorevert-only-visible (bufs)
      "Inhibit `autorevert' for buffers not displayed in any window."
      :filter-return #'auto-revert--polled-buffers
      (cl-remove-if #'radian-autorevert-inhibit-p bufs)))

  :blackout auto-revert-mode)

(-ow paren
  ;; highlight matching delimiters
  :hook (radian-first-buffer-hook . show-paren-mode)
  :config
  (setq show-paren-delay 0.1
        show-paren-highlight-openparen t
        show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t))

(-ow apheleia
  :straight (apheleia :host github :repo "raxod502/apheleia")
  :init

  (apheleia-global-mode +1)

  (defadvice! radian--save-buffer-reformat-maybe (func &optional arg)
    "Make it so \\[save-buffer] with prefix arg inhibits reformatting."
    :around #'save-buffer
    (let ((apheleia-mode (and apheleia-mode (member arg '(nil 1)))))
      (funcall func)))

  ;; We need to do this both before and after Apheleia is loaded
  ;; because the autoloading is set up such that the minor mode
  ;; definition is evaluated twice.
  (blackout 'apheleia-mode)

  :blackout t)

(-ow abbrev :blackout t)

(-ow xref
  :custom
  (xref-search-program . 'ripgrep)
  (xref-show-xrefs-function . #'xref-show-definitions-completing-read)
  (xref-show-definitions-function . #'xref-show-definitions-completing-read))

(setq-default indent-tabs-mode nil)

(defun radian-indent-defun ()
  "Indent the surrounding defun."
  (interactive)
  (save-excursion
    (when (beginning-of-defun)
      (let ((beginning (point)))
        (end-of-defun)
        (let ((end (point)))
          (let ((inhibit-message t)
                (message-log-max nil))
            (indent-region beginning end)))))))

(-key* "C-x TAB" #'radian-indent-defun)

(defadvice! radian--advice-indent-region-quietly (func &rest args)
  "Make `indent-region' shut up about its progress."
  :around #'indent-region
  (regx-quiet! "Indenting region" (apply func args)))

;;;; Autocompletion

;; Package `company' provides an in-buffer autocompletion framework.
;; It allows for packages to define backends that supply completion
;; candidates, as well as optional documentation and source code. Then
;; Company allows for multiple frontends to display the candidates,
;; such as a tooltip menu. Company stands for "Complete Anything".

(-ow eldoc
  :require t
  :config

  ;; For Emacs 26 and below, `eldoc--message' is not defined. For
  ;; Emacs 27 and above, `eldoc-message' is obsolete.
  (with-no-warnings
    (defadvice! radian--advice-eldoc-no-trample (func &rest args)
      "Prevent `eldoc' from trampling on existing messages."
      :around #'eldoc-print-current-symbol-info
      (letf! ((defun eldoc-message (&optional string)
                (if string
                    (funcall eldoc-message string)
                  (setq eldoc-last-message nil))
                (defun eldoc--message (&optional string)
                  (if string
                      (funcall eldoc--message string)
                    (setq eldoc-last-message nil)))))
        (apply func args))))

  ;; Always truncate ElDoc messages to one line. This prevents the
  ;; echo area from resizing itself unexpectedly when point is on a
  ;; variable with a multiline docstring.
  (setq eldoc-echo-area-use-multiline-p nil)

  ;; Original code from
  ;; https://github.com/PythonNut/emacs-config/blob/1a92a1ff1d563fa6a9d7281bbcaf85059c0c40d4/modules/config-intel.el#L130-L137,
  ;; thanks!

  (defadvice! radian--advice-eldoc-better-display-message-p (&rest _)
    "Make ElDoc smarter about when to display its messages.
By default ElDoc has a customizable whitelist of commands that it
will display its messages after. The idea of this is to not
trample on messages that other commands may have printed.
However, this is a hopeless endeavour because there are a
virtually unlimited number of commands that don't conflict with
ElDoc. A better approach is to simply check to see if a message
was printed, and only have ElDoc display if one wasn't."
    :override #'eldoc--message-command-p
    (member (current-message) (list nil eldoc-last-message)))

  :blackout t)

(-ow lisp-mode
  :bind
  ([remap eval-expression] . pp-eval-expression)
  :init
  (add-to-list 'safe-local-variable-values
               '(lisp-indent-function . common-lisp-indent-function)))

(-ow cc-mode

  :defer-config

  (defadvice! radian--advice-inhibit-c-submode-indicators (&rest _)
    "Unconditionally inhibit CC submode indicators in the mode lighter."
    :override #'c-update-modeline)

  ;; Switch to a better indentation-and-braces style. This turns the
  ;; following code:
  ;;
  ;; if (condition)
  ;;   {
  ;;     statement;
  ;;   }
  ;;
  ;; Into this:
  ;;
  ;; if (condition)
  ;; {
  ;;   statement;
  ;; }
  ;;
  ;; We do this by defining a custom style that is based on BSD, and
  ;; then overriding the indentation (which is set to 8 spaces by
  ;; default). This style is only used for languages which do not have
  ;; a more specific style set in `c-default-style'.
  (c-add-style "radian-bsd"
               '("bsd"
                 (c-basic-offset . 2)))
  (setf (map-elt c-default-style 'other) "radian-bsd")

  (put 'c-default-style 'safe-local-variable #'stringp))


;; Extra file extensions to support
(nconc auto-mode-alist
       '(("/LICENSE\\'" . text-mode)
         ("\\.log\\'" . text-mode)
         ("rc\\'" . conf-mode)
         ("\\.\\(?:hex\\|nes\\)\\'" . hexl-mode)))

(-ow help
  :bind
  ;; Aviod visiting HELLO file accidentally.
  ("C-h h" . nil)
  (help-map ("M-k" . describe-keymap))

  :config
  ;; Aviod visiting HELLO file accidentally.
  (put 'view-hello-file 'disabled t)

  (defadvice! radian--advice-help-inhibit-hints (&rest _)
    "Inhibit the \"Type q in help window to delete it\" hints.
Normally these are printed in the echo area whenever you open a
help buffer."
    :override #'help-window-display-message)


  (defadvice! radian--advice-help-disable-revert-prompt
    (help-mode-revert-buffer ignore-auto _noconfirm)
    "Don't ask for confirmation before reverting help buffers.
\(Reverting is done by pressing \\<help-mode-map>\\[revert-buffer].)"
    :around #'help-mode-revert-buffer
    (funcall help-mode-revert-buffer ignore-auto 'noconfirm))

  (add-hook! 'help-mode-hook
    (defun radian--xref-help-setup ()
      "Make xref look up Elisp symbols in help buffers.
Otherwise, it will try to find a TAGS file using etags, which is
unhelpful."
      (add-hook 'xref-backend-functions #'elisp--xref-backend nil 'local))))

(-ow elisp-mode
  :config
  (defun radian/headerise-elisp ()
    "Add minimal header and footer to an elisp buffer in order to placate flycheck."
    (interactive)
    (let ((fname (if (buffer-file-name)
                     (file-name-nondirectory (buffer-file-name))
                   (error "This buffer is not visiting a file"))))
      (save-excursion
        (goto-char (point-min))
        (insert ";;; " fname " --- Insert description here -*- lexical-binding: t -*-\n"
                ";;; Commentary:\n"
                ";;; Code:\n\n")
        (goto-char (point-max))
        (insert ";;; " fname " ends here\n"))))
  (-key "ie" #'radian/headerise-elisp 'radian-comma-keymap)

  ;; <https://github.com/magnars/dash.el#fontification-of-special-variables>
  ;; Integrate dash.el
  (eval-after-load 'dash '(global-dash-fontify-mode))

  (defadvice! radian--advice-fill-elisp-docstrings-correctly (&rest _)
    "Prevent `auto-fill-mode' from adding indentation to Elisp docstrings."
    :before-until #'fill-context-prefix
    (when (and (derived-mode-p #'emacs-lisp-mode)
               (eq (get-text-property (point) 'face) 'font-lock-doc-face))
      ""))

  ;; The default mode lighter has a space instead of a hyphen.
  ;; Disgusting!
  :blackout ((lisp-interaction-mode . "Lisp-Interaction")
             (emacs-lisp-mode . `("ELisp"
                                  (lexical-binding
                                   ""
                                   (:propertize
                                    "/d" face warning))))))

(-keys (("C-h f"   . find-function)
        ("C-h v"   . find-variable)
        ("C-h C-l" . find-library)
        ("C-h C-f" . describe-function)
        ("C-h C-v" . describe-variable)
        ("C-h C-o" . describe-symbol)
        ("C-h C-e" . view-echo-area-messages)))

(-ow checkdoc
  :init
  ;; Not sure why this isn't included by default.
  (put 'checkdoc-package-keywords-flag 'safe-local-variable #'booleanp))

;; Package `elisp-lint', not installed, provides a linting framework
;; for Elisp code. We use `with-eval-after-load' because `leaf'
;; is configured to try to `require' features during byte-compilation.
(with-eval-after-load 'elisp-lint
  ;; From the package. We need this because some packages set this as
  ;; a file-local variable, but we don't install the package so Emacs
  ;; doesn't know the variable is safe.
  (put 'elisp-lint-indent-specs 'safe-local-variable #'listp))

(-ow dired
  ;; This binding is way nicer than ^. It's inspired by
  ;; Sunrise Commander.
  :bind (dired-mode-map ("J" . dired-up-directory))
  :bind* ("C-x w" . radian-rename-current-file)
  :defer-config

  (defun radian-rename-current-file (newname)
    "Rename file visited by current buffer to NEWNAME.
Interactively, prompt the user for the target filename, with
completion.

If NEWNAME is a directory then extend it with the basename of
`buffer-file-name'. Make parent directories automatically."
    (interactive
     (progn
       (unless buffer-file-name
         (user-error "Current buffer is not visiting a file"))
       (let ((newname (read-file-name "Rename to: " nil buffer-file-name)))
         (when (equal (file-truename newname)
                      (file-truename buffer-file-name))
           (user-error "%s" "Can't rename a file to itself"))
         (list newname))))
    (unless buffer-file-name
      (error "Current buffer is not visiting a file"))
    (when (equal (file-truename newname)
                 (file-truename buffer-file-name))
      (error "%s: %s" "Can't rename a file to itself" newname))
    (when (equal newname (file-name-as-directory newname))
      (setq newname
            (concat newname (file-name-nondirectory buffer-file-name))))
    (make-directory (file-name-directory newname) 'parents)
    ;; Passing integer as OK-IF-ALREADY-EXISTS means prompt for
    ;; confirmation before overwriting. Why? Who can say...
    (dired-rename-file buffer-file-name newname 0))

  (defadvice! radian--advice-dired-check-for-ls-dired (&rest _)
    "Check if ls --dired is supported ahead of time, and silently.

This advice prevents Dired from printing a message if your ls
does not support the --dired option. (We do this by performing
the check ourselves, and refraining from printing a message in
the problematic case.)"
    :before #'dired-insert-directory
    (when (eq dired-use-ls-dired 'unspecified)
      (setq dired-use-ls-dired
            (eq 0 (call-process insert-directory-program
                                nil nil nil "--dired")))))

  (add-hook 'dired-mode-hook #'radian--autorevert-silence)

  ;; Disable the prompt about whether I want to kill the Dired buffer
  ;; for a deleted directory. Of course I do! It's just a Dired
  ;; buffer, after all. Note that this variable, for reasons unknown
  ;; to me, is defined in `dired-x', but only affects the behavior of
  ;; functions defined in `dired'.
  (setq dired-clean-confirm-killing-deleted-buffers nil)

  ;; Instantly revert Dired buffers on re-visiting them, with no
  ;; message. (A message is shown if insta-revert is either disabled
  ;; or determined dynamically by setting this variable to a
  ;; function.)
  (setq dired-auto-revert-buffer #'dired-buffer-stale-p))

(-ow dired-x
  :bind (;; Bindings for jumping to the current directory in Dired.
         ("C-x C-j" . dired-jump)
         ("C-x 4 C-j" . dired-jump-other-window))
  :config

  ;; Prevent annoying "Omitted N lines" messages when auto-reverting.
  (setq dired-omit-verbose nil)

  (when *MAC
    (defadvice! radian--advice-dired-guess-open-on-macos
      (&rest _)
      "Cause Dired's '!' command to use open(1).
This advice is only activated on macOS, where it is helpful since
most of the Linux utilities in `dired-guess-shell-alist-default'
are probably not going to be installed."
      :override #'dired-guess-default)))
(-ow find-dired :setq (find-ls-option . '("-print0 | xargs -0 ls -ld" . "-ld")))

(-ow smerge-mode :blackout t)
(-ow pixel-scroll :emacs> 29 :init (pixel-scroll-precision-mode +1))

(appendq! initial-frame-alist
          '((tool-bar-lines . 0)
            ;;;; only on unix
            ;; (icon-type . nil)
            ;; (alpha-background . 80)

            ;; (fullscreen . maximized)
            ;;(undecorated . t)
            (internal-border-width . 0)
            (alpha . (95 . 80))))
(appendq! default-frame-alist initial-frame-alist)

(setq frame-title-format
      '(""
        (:eval
         (if (eq major-mode 'org-mode)
             (replace-regexp-in-string
              ".*/[0-9]*-?" "☰ "
              (subst-char-in-string ?_ ?  (or buffer-file-name "Null")))
           "%b")))
      icon-title-format frame-title-format)

;; Allow you to resize frames however you want, not just in whole
;; columns. "The 80s called, they want their user interface back"
(setq-default frame-resize-pixelwise t
              window-resize-pixelwise t)

(setq minibuffer-message-properties '(face minibuffer-prompt))

;; Disable the contextual menu that pops up when you right-click.
(global-set-key (kbd "<C-down-mouse-1>") nil)

;; The menu bar appears in both graphical and tty frames. Kill it.
(menu-bar-mode -1)

(when (display-graphic-p)

  ;; Disable unnecessary graphical elements.
  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))
  (tool-bar-mode -1)

  (eval-when! *MAC
    (add-hook! 'after-make-frame-functions
      (defun radian--disable-menu-bar-again-on-macos (_)
        "Disable the menu bar again, because macOS is dumb.
On macOS, for some reason you can't disable the menu bar once it
appears, and also `menu-bar-mode' doesn't prevent the menu bar
from appearing when run during early init. So we do a hack and
turn it off again after creating the first frame."
        (menu-bar-mode -1))))

  ;; Prevent the cursor from blinking. Do it two ways: using the minor
  ;; mode only works during regular init, while using the variable
  ;; only works during early init.
  (blink-cursor-mode -1)
  (setq no-blinking-cursor t)

  ;; Don't stretch the cursor to fit wide characters, it is disorienting,
  ;; especially for tabs.
  (setq x-stretch-cursor nil)

  ;; On macOS, set the title bar to match the frame background.
  (when *MAC
    (add-to-list 'default-frame-alist '(ns-appearance . dark))
    (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))))

;; Scrolling
(setq hscroll-margin 2
      hscroll-step 1
      ;; Emacs spends too much effort recentering the screen if you scroll the
      ;; cursor more than N lines past window edges (where N is the settings of
      ;; `scroll-conservatively'). This is especially slow in larger files
      ;; during large-scale scrolling commands. If kept over 100, the window is
      ;; never automatically recentered.
      scroll-conservatively 101
      scroll-margin 0
      scroll-preserve-screen-position t
      ;; Reduce cursor lag by a tiny bit by not auto-adjusting `window-vscroll'
      ;; for tall lines.
      auto-window-vscroll nil
      ;; mouse
      mouse-wheel-scroll-amount '(1 ((shift) . hscroll))
      mouse-wheel-scroll-amount-horizontal 2)

;; Mouse integration works out of the box in windowed mode but not
;; terminal mode. The following code to fix it was based on
;; <https://stackoverflow.com/a/8859057/3538165>.
(unless (display-graphic-p)
  ;; Enable basic mouse support (click and drag).
  (xterm-mouse-mode t)
  ;; Note that the reason for the next two functions is that
  ;; `scroll-down' and `scroll-up' scroll by a "near full screen"
  ;; by default, whereas we want a single line.
  (eval-and-compile
    (defun radian-scroll-down ()
      "Scroll down one line."
      (interactive)
      (scroll-down 1))
    (defun radian-scroll-up ()
      "Scroll up one line."
      (interactive)
      (scroll-up 1)))

  ;; Enable scrolling with the mouse wheel.
  (-key "<mouse-4>" #'radian-scroll-down)
  (-key "<mouse-5>" #'radian-scroll-up))

;; Remove hscroll-margin in shells, otherwise it causes jumpiness
(setq-hook! '(eshell-mode-hook term-mode-hook) hscroll-margin 0)

;; The native border "consumes" a pixel of the fringe on righter-most splits,
;; `window-divider' does not. Available since Emacs 25.1.
(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)
(add-hook 'radian-init-ui-hook #'window-divider-mode)

;; GUIs are inconsistent across systems and themes (and will rarely match our
;; active Emacs theme). They impose inconsistent shortcut key paradigms too.
;; It's best to avoid GUIs altogether and have Emacs handle the prompting, since
;; its promtps are governed by the same rules and keybinds as the rest of Emacs.
(setq use-dialog-box nil)
(when (bound-and-true-p tooltip-mode) (tooltip-mode -1))
(eval-when! *LINUX (setq x-gtk-use-system-tooltips nil))

;; Expand the minibuffer to fit multi-line text displayed in the echo-area. This
;; doesn't look too great with direnv, however...
(setq resize-mini-windows 'grow-only)

;; Try to keep the cursor out of the read-only portions of the minibuffer.
(setq minibuffer-prompt-properties '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(setq-default mode-line-buffer-identification
              (propertized-buffer-identification "%b"))

;; Make `mode-line-position' show the column, not just the row.
(column-number-mode +1)

;; https://emacs.stackexchange.com/a/7542/12534
(defun radian--mode-line-align (left right)
  "Render a left/right aligned string for the mode line.
LEFT and RIGHT are strings, and the return value is a string that
displays them left- and right-aligned respectively, separated by
spaces."
  (let ((width (- (window-total-width) (length left))))
    (format (format "%%s%%%ds" width) left right)))

;; <https://github.com/minad/recursion-indicator>
;; remove the default recursion indicator
(let ((index 0))
  (dolist (e mode-line-modes)
    (cond
     ((and (stringp e) (string-match-p "^\\(%\\[\\|%\\]\\)$" e))
      (setf (nth index mode-line-modes) ""))
     ((equal "(" e) (setf (nth index mode-line-modes) "⟿"))
     ((equal ")" e) (setf (nth index mode-line-modes) ""))
     (t))
    (setq index (1+ index))))

(defcustom radian-mode-line-left
  '(" "
    mode-line-mule-info
    "%*" "%@"
    "  "
    ;; Show the name of the current buffer.
    mode-line-buffer-identification
    "  "
    ;; Show the row and column of point.
    mode-line-position
    ;; Show the active major and minor modes.
    " "
    mode-line-modes
    mode-line-misc-info)
  "Composite mode line construct to be shown left-aligned."
  :type 'sexp)

(defcustom radian-mode-line-right          ;⊙_⚆⚈☭
  '(""
    (vc-mode vc-mode)
    " "
    (:eval (when (featurep 'meow) (meow-indicator)))
    " ")
  "Composite mode line construct to be shown right-aligned."
  :type 'sexp)

;; Actually reset the mode line format to show all the things we just
;; defined.
(setq-default mode-line-format
              '(:eval (replace-regexp-in-string
                       "%" "%%"
                       (radian--mode-line-align
                        (format-mode-line radian-mode-line-left)
                        (format-mode-line radian-mode-line-right))
                       'fixedcase 'literal)))

;;;; Formfeed display.
(defun xah-insert-formfeed ()
  "Insert a form feed char (codepoint 12)"
  (interactive)
  (insert "\u000c\n"))

(defun xah-show-formfeed-as-line (&optional frame)
  "Display the formfeed ^L char as line."
  (interactive)
  (letf! (defun pretty-formfeed-line (window)
           (with-current-buffer (window-buffer window)
             (with-selected-window window
               (when (not buffer-display-table)
                 (setq buffer-display-table (make-display-table)))
               (aset buffer-display-table ?\^L
                     (vconcat (make-list 70 (make-glyph-code ?─ 'font-lock-comment-face))))
               (redraw-frame))))
    (unless (minibufferp)
      (mapc 'pretty-formfeed-line (window-list frame 'no-minibuffer)))))

(dolist (hook '(window-configuration-change-hook
                window-size-change-functions
                after-setting-font-hook
                display-line-numbers-mode-hook))
  (add-hook hook #'xah-show-formfeed-as-line))

;;; Shutdown
;; Package `restart-emacs' provides an easy way to restart Emacs from
;; inside of Emacs, both in the terminal and in windowed mode.
(pow restart-emacs

  :init
  (defun radian-really-kill-emacs ()
    "Kill Emacs immediately, bypassing `kill-emacs-hook'."
    (interactive)
    (let ((kill-emacs-hook nil))
      (kill-emacs)))

  (defvar radian--restart-in-progress nil
    "Used to prevent infinite recursion.
This is non-nil if `radian--advice-kill-emacs-dispatch' has called
`restart-emacs'.")

  (defvar radian--restart-emacs-eager-hook-functions
    ;; This list contains hooks that I determined via profiling to be
    ;; slow (double-digit milliseconds).
    '(prescient--save
      radian--org-clock-save
      save-place-kill-emacs-hook)
    "List of functions on `kill-emacs-hook' which can be run eagerly.
If actually present on `kill-emacs-hook', then these functions
are run immediately on `save-buffers-kill-emacs'. This means that
Emacs shutdown appears to be slightly faster.

Functions can only be added here if it is okay to run them even
when shutting down Emacs is canceled. However, it is fine to put
functions here that aren't actually present on `kill-emacs-hook'.")

  (defvar radian--restart-emacs-eager-hook-functions-run nil
    "List of functions on `kill-emacs-hook' which have been run eagerly.
The global value of this variable is irrelevant; it is always
bound dynamically before being used.")

  (autoload #'restart-emacs--translate-prefix-to-args "restart-emacs")

  (defadvice! radian--advice-kill-emacs-dispatch
    (save-buffers-kill-emacs &optional arg)
    "Allow restarting Emacs or starting a new session on shutdown."
    :around #'save-buffers-kill-emacs
    (if radian--restart-in-progress
        (funcall save-buffers-kill-emacs arg)
      (let ((radian--restart-in-progress t)
            ;; Don't mutate the global value.
            (radian--restart-emacs-eager-hook-functions-run nil)
            (prompt (concat "Really exit (or restart, or start new, or kill) "
                            "Emacs? (y/n/r/e/k) "))
            (key nil))
        (dolist (func radian--restart-emacs-eager-hook-functions)
          ;; Run eager hook functions asynchronously while waiting for
          ;; user input. Use a separate idle timer for each function
          ;; because the order shouldn't be important, and because
          ;; that way if we don't actually restart then we can cancel
          ;; out faster (we don't have to wait for all the eager hook
          ;; functions to run).
          (run-with-idle-timer
           0 nil
           (lambda ()
             (when (and radian--restart-in-progress
                        (memq func kill-emacs-hook))
               (funcall func)
               ;; Thank goodness Elisp is single-threaded.
               (push func radian--restart-emacs-eager-hook-functions-run)))))
        (while (null key)
          (let ((cursor-in-echo-area t))
            (when minibuffer-auto-raise
              (raise-frame (window-frame (minibuffer-window))))
            (setq key
                  (read-key (propertize prompt
                                        'face 'minibuffer-prompt)))
            ;; No need to re-run the hooks that we already ran
            ;; eagerly. (This is the whole point of those
            ;; shenanigans.)
            (let ((kill-emacs-hook
                   (cl-remove-if
                    (lambda (func)
                      (memq
                       func
                       radian--restart-emacs-eager-hook-functions-run))
                    kill-emacs-hook)))
              (pcase key
                ((or ?y ?Y) (funcall save-buffers-kill-emacs arg))
                ((or ?n ?N))
                ((or ?r ?R)
                 (restart-emacs arg))
                ((or ?e ?E)
                 (restart-emacs-start-new-emacs
                  (restart-emacs--translate-prefix-to-args arg)))
                ((or ?k ?K) (radian-really-kill-emacs))
                (?\C-g (signal 'quit nil))
                (_ (setq key nil))))))
        (message "%s%c" prompt key)))))

(defun radian/dos2unix ()
  "Convert the current buffer to a Unix file encoding."
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))

(defun radian/unix2dos ()
  "Convert the current buffer to a DOS file encoding."
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos nil))

(when *WINDOWS
  (defun save-buffer-as-unix (&rest _)
    (if (eq major-mode 'emacs-lisp-mode) (radian/dos2unix)))
  (add-hook 'before-save-hook #'save-buffer-as-unix))

;; 2021-10-21 / Thursday, 21. October 2021 / 21.10.2021
(-key "id" #'insert-date 'radian-comma-keymap)

;;; Startup
;; Disable the *About GNU Emacs* buffer at startup, and go straight
;; for the scratch buffer.
(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      ;; Shave seconds off startup time by starting the scratch buffer in
      ;; `fundamental-mode', rather than, say, `org-mode' or `text-mode', which
      ;; pull in a ton of packages. `radian/open-scratch-buffer' provides a better
      ;; scratch buffer anyway.
      initial-major-mode 'fundamental-mode
      initial-buffer-choice t
      initial-scratch-message nil)

;; Make `apropos' et co search more extensively. They're more useful this way.
(setq apropos-do-all t)

;; A second, case-insensitive pass over `auto-mode-alist' is time wasted, and
;; indicates misconfiguration (or that the user needs to stop relying on case
;; insensitivity).
(setq auto-mode-case-fold nil)

;; EN date format
(setq system-time-locale "C")
(setq calendar-chinese-all-holidays-flag t)

;; Get rid of "For information about GNU Emacs..." message at startup, unless
;; we're in a daemon session where it'll say "Starting Emacs daemon." instead,
;; which isn't so bad.
(unless (daemonp)
  (advice-add #'display-startup-echo-area-message :override #'ignore))

;; Emacs "updates" its ui more often than it needs to, so we slow it down
;; slightly from 0.5s:
(setq idle-update-delay 1.0)

;; Don't prompt for confirmation when we create a new file or buffer (assume the
;; user knows what they're doing).
(setq confirm-nonexistent-file-or-buffer nil)

(setq uniquify-buffer-name-style 'forward
      ;; no beeping or blinking please
      ring-bell-function #'ignore
      visible-bell nil)

;; Larger column width for function name in profiler reports
(after! profiler
  (setf (caar profiler-report-cpu-line-format) 80
        (caar profiler-report-memory-line-format) 80))

;; Display keystrokes in the echo area immediately, not after one
;; second. We can't set the delay to zero because somebody thought it
;; would be a good idea to have that value suppress keystroke display
;; entirely.
(setq echo-keystrokes 1e-6)

(after! comint
  (setq comint-prompt-read-only t
        comint-buffer-maximum-size 2048)) ; double the default


(after! compile
  (setq compilation-always-kill t       ; kill compilation process before starting another
        compilation-ask-about-save nil  ; save all buffers on `compile'
        compilation-scroll-output 'first-error)
  ;; Handle ansi codes in compilation buffer
  (add-hook 'compilation-filter-hook #'radian-apply-ansi-color-to-compilation-buffer-h)
  ;; Automatically truncate compilation buffers so they don't accumulate too
  ;; much data and bog down the rest of Emacs.
  (autoload 'comint-truncate-buffer "comint" nil t)
  (add-hook 'compilation-filter-hook #'comint-truncate-buffer))

;; Don't blink the cursor on the opening paren when you insert a
;; closing paren, as we already have superior handling of that from
;; Smartparens.
(setq blink-matching-paren nil)

;; Typing yes/no is obnoxious when y/n will do
;; (advice-add #'yes-or-no-p :override #'y-or-n-p)
(setq use-short-answers t)

;; Enable all disabled commands.
(setq disabled-command-function nil)

;; HACK Stop sessions from littering the user directory
(defadvice! radian--use-cache-dir-a (session-id)
  :override #'emacs-session-filename
  (concat *cache/* "emacs-session." session-id))

(defun radian-display-benchmark-h (&optional return-p)
  "Display a benchmark including number of packages and modules loaded.

If RETURN-P, return the message as a string instead of displaying it."
  (funcall
   (if return-p #'format #'message)
   "Radian loaded %d packages in %.03fs"
   (- (length load-path) (length (get 'load-path 'initial-value)))
   (or radian-init-time
       (setq radian-init-time
             (float-time (time-subtract (current-time) before-init-time))))))

;;;; simple
(-ow simple
  :bind
  ("C-x C-M-t" . transpose-regions)
  (([remap default-indent-new-line] . radian-continue-comment))
  :config

  (defun radian-continue-comment ()
    "Continue current comment, preserving trailing whitespace.
This differs from `default-indent-new-line' in the following way:

If you have a comment like \";; Some text\" with point at the end
of the line, then running `default-indent-new-line' will get you
a new line with \";; \", but running it again will get you a line
with only \";;\" (no trailing whitespace). This is annoying for
inserting a new paragraph in a comment. With this command, the
two inserted lines are the same."
    (interactive)
    ;; `default-indent-new-line' uses `delete-horizontal-space'
    ;; because in auto-filling we want to avoid the space character at
    ;; the end of the line from being put at the beginning of the next
    ;; line. But when continuing a comment it's not desired.
    (cl-letf (((symbol-function #'delete-horizontal-space) #'ignore))
      (default-indent-new-line))))

;;;; savehist for session
(-ow savehist
  ;; persist variables across sessions
  :increment custom
  :hook radian-first-input-hook
  :config
  (setq savehist-save-minibuffer-history t
        savehist-autosave-interval nil     ; save on kill only
        savehist-additional-variables
        '(kill-ring                        ; persist clipboard
          register-alist                   ; persist macros
          mark-ring global-mark-ring       ; persist marks
          search-ring regexp-search-ring)) ; persist searches
  (add-hook! 'savehist-save-hook
    (defun radian-savehist-unpropertize-variables-h ()
      "Remove text properties from `kill-ring' to reduce savehist cache size."
      (setq kill-ring
            (mapcar #'substring-no-properties
                    (cl-remove-if-not #'stringp kill-ring))
            register-alist
            (cl-loop for (reg . item) in register-alist
                     if (stringp item)
                     collect (cons reg (substring-no-properties item))
                     else collect (cons reg item))))
    (defun radian-savehist-remove-unprintable-registers-h ()
      "Remove unwriteable registers (e.g. containing window configurations).
Otherwise, `savehist' would discard `register-alist' entirely if we don't omit
the unwritable tidbits."
      ;; Save new value in the temp buffer savehist is running
      ;; `savehist-save-hook' in. We don't want to actually remove the
      ;; unserializable registers in the current session!
      (setq-local register-alist
                  (cl-remove-if-not #'savehist-printable register-alist)))))

(defvar radian-font (font-spec :family "Microsoft Yahei")
  "The default font to use.
Must be a `font-spec', a font object, an XFT font string, or an XLFD string.

This affects the `default' and `fixed-pitch' faces.

The properties except :family are all ignored.

Examples:
  (setq radian-font (font-spec :family \"Fira Mono\"))
  (setq radian-font \"Terminus (TTF):antialias=off\")")

(defvar radian-font-size 98 "The `height' property of radian fontset ")

(defvar radian-variable-pitch-font nil
  "The default font to use for variable-pitch text.
Must be a `font-spec', a font object, an XFT font string, or an XLFD string. See
`radian-font' for examples.

An omitted font size means to inherit `radian-font''s size.")

(defvar radian-serif-font nil
  "The default font to use for the `fixed-pitch-serif' face.
Must be a `font-spec', a font object, an XFT font string, or an XLFD string. See
`radian-font' for examples.

An omitted font size means to inherit `radian-font''s size.")

(defvar radian-unicode-font nil
  "Fallback font for Unicode glyphs.
Must be a `font-spec', a font object, an XFT font string, or an XLFD string. See
`radian-font' for examples.

The defaults on macOS and Linux are Apple Color Emoji and Symbola, respectively.

WARNING: if you specify a size for this font it will hard-lock any usage of this
font to that size. It's rarely a good idea to do so!")

(defvar radian-emoji-fallback-font-families
  '("Apple Color Emoji"
    "Segoe UI Emoji"
    "Noto Color Emoji"
    "Noto Emoji")
  "A list of fallback font families to use for emojis.")

(defvar radian-symbol-fallback-font-families
  '("Symbola"
    "Segoe UI Symbol"
    "Apple Symbols")
  "A list of fallback font families for general symbol glyphs.")

(defvar radian-cjk-fallback-font-families
  '("WenQuanYi Micro Hei Mono"
    "Microsoft YaHei")
  "A list of fallback font families to use for emojis.")

(defun radian-init-font-h (&optional reload)
  "Loads `fonts'"
  (when (fboundp 'set-fontset-font)
    (let ((fn (apply-partially (lambda (font) (find-font (font-spec :name font))))))
      (when-let (font (cl-find-if fn radian-symbol-fallback-font-families))
        (set-fontset-font t 'symbol font))
      (when-let (font (cl-find-if fn radian-emoji-fallback-font-families))
        (eval-when! *EMACS28+ (set-fontset-font t 'emoji font nil 'append)))
      (when radian-unicode-font
        (set-fontset-font t 'unicode radian-unicode-font nil 'append))
      (when-let (font (cl-find-if fn radian-cjk-fallback-font-families))
        ;; Set CJK font.
        ;; (dolist (script '(kana han cjk-misc bopomofo))
        ;;   (set-fontset-font (frame-parameter nil 'font) script font))
        (set-fontset-font t '(#x4e00 . #x9fff) font nil 'prepend))))

  (apply #'custom-set-faces
         (let ((attrs '(:weight unspecified :slant unspecified :width unspecified)))
           (append (when radian-font
                     `((fixed-pitch ((t (:font ,radian-font ,@attrs))))))
                   (when radian-serif-font
                     `((fixed-pitch-serif ((t (:font ,radian-serif-font ,@attrs))))))
                   (when radian-variable-pitch-font
                     `((variable-pitch ((t (:font ,radian-variable-pitch-font ,@attrs)))))))))
  ;; Never save these settings to `custom-file'
  (dolist (sym '(fixed-pitch fixed-pitch-serif variable-pitch))
    (put sym 'saved-face nil))

  ;;  FIXME: During initialize. this way cannot chang the font
  ;; height. I don't know why.
  (set-face-attribute 'default nil :height radian-font-size)

  (if (or reload (daemonp)) (set-frame-font radian-font t t t))
  ;; I avoid `set-frame-font' at startup because it is expensive; doing extra,
  ;; unnecessary work we can avoid by setting the frame parameter directly.
  (setf (alist-get 'font default-frame-alist)
        (cond ((stringp radian-font)
               (format "-*-%s-*-*-*-*-*-%s-*-*-*-*-*-*" radian-font radian-font-size))
              ((fontp radian-font)
               (format "-*-%s-*-*-*-*-*-%s-*-*-*-*-*-*"
                       (font-get radian-font :family) radian-font-size))
              ((signal 'wrong-type-argument (list '(fontp stringp) radian-font))))))


(defcustom theme-light/dark 'light
  "Nano theme uses light theme or dark theme?"
  :type 'symbol
  :group 'radian)

(defvar radian-theme-list '((modus-operandi . 'light) (modus-vivendi . 'dark))
  "Theme sequence of changing. `(THEME-NAME . *LIGHT-THEME)'")

(defun --l?d (light dark)
  "Determine using the LIGHT or the DARK color of theme."
  (if (eq theme-light/dark 'light) light dark))

(-ow modus-themes
  :init
  (setq modus-themes-vivendi-color-overrides
        '((bg-main . "#2E3440") (fg-unfocused . "#ECEFF4")))
  (setq modus-themes-operandi-color-overrides
        '((bg-main . "#FFFFFF") (fg-unfocused . "#37474F")))

  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t
        modus-themes-mixed-fonts nil
        modus-themes-subtle-line-numbers t
        modus-themes-deuteranopia nil
        modus-themes-tabs-accented nil
        modus-themes-variable-pitch-ui nil
        modus-themes-inhibit-reload t ; only applies to `customize-set-variable' and related

        modus-themes-fringes nil ; {nil,'subtle,'intense}

        ;; Options for `modus-themes-lang-checkers' are either nil (the
        ;; default), or a list of properties that may include any of those
        ;; symbols: `straight-underline', `text-also', `background',
        ;; `intense' OR `faint'.
        modus-themes-lang-checkers '(text-also intense)

        ;; Options for `modus-themes-mode-line' are either nil, or a list
        ;; that can combine any of `3d' OR `moody', `borderless',
        ;; `accented', and a natural number for extra padding
        modus-themes-mode-line '(4 accented borderless)

        ;; Options for `modus-themes-markup' are either nil, or a list
        ;; that can combine any of `bold', `italic', `background',
        ;; `intense'.
        modus-themes-markup '(background intense)

        ;; Options for `modus-themes-syntax' are either nil (the default),
        ;; or a list of properties that may include any of those symbols:
        ;; `faint', `yellow-comments', `green-strings', `alt-syntax'
        modus-themes-syntax '(faint alt-syntax)

        ;; Options for `modus-themes-hl-line' are either nil (the default),
        ;; or a list of properties that may include any of those symbols:
        ;; `accented', `underline', `intense'
        modus-themes-hl-line '(accented)

        ;; Options for `modus-themes-paren-match' are either nil (the
        ;; default), or a list of properties that may include any of those
        ;; symbols: `bold', `intense', `underline'
        modus-themes-paren-match nil

        ;; Options for `modus-themes-links' are either nil (the default),
        ;; or a list of properties that may include any of those symbols:
        ;; `neutral-underline' OR `no-underline', `faint' OR `no-color',
        ;; `bold', `italic', `background'
        modus-themes-links '(neutral-underline faint)

        ;; Options for `modus-themes-box-buttons' are either nil (the
        ;; default), or a list that can combine any of `flat',
        ;; `accented', `faint', `variable-pitch', `underline', the
        ;; symbol of any font weight as listed in
        ;; `modus-themes-weights', and a floating point number
        ;; (e.g. 0.9) for the height of the button's text.
        modus-themes-box-buttons '(variable-pitch flat faint 0.9)

        ;; Options for `modus-themes-prompts' are either nil (the
        ;; default), or a list of properties that may include any of those
        ;; symbols: `background', `bold', `gray', `intense', `italic'
        modus-themes-prompts '(intense bold)

        ;; The `modus-themes-completions' is an alist that reads three
        ;; keys: `matches', `selection', `popup'.  Each accepts a nil
        ;; value (or empty list) or a list of properties that can include
        ;; any of the following (for WEIGHT read further below):
        ;;
        ;; `key' - `background', `intense', `underline', `italic', WEIGHT
        ;; `selection' - `accented', `intense', `underline', `italic', WEIGHT
        ;; `popup' - same as `selected'
        ;; `t' - applies to any key not explicitly referenced (check docs)
        ;;
        ;; WEIGHT is a symbol such as `semibold', `light', or anything
        ;; covered in `modus-themes-weights'.  Bold is used in the absence
        ;; of an explicit WEIGHT.
        modus-themes-completions '((matches . (extrabold))
                                   (selection . (semibold accented))
                                   (popup . (accented intense)))

        modus-themes-mail-citations 'faint ; {nil,'intense,'faint,'monochrome}

        ;; Options for `modus-themes-region' are either nil (the default),
        ;; or a list of properties that may include any of those symbols:
        ;; `no-extend', `bg-only', `accented'
        modus-themes-region '(no-extend bg-only accented)

        ;; Options for `modus-themes-diffs': nil, 'desaturated, 'bg-only
        modus-themes-diffs 'desaturated

        modus-themes-org-blocks nil ; {nil,'gray-background,'tinted-background}

        modus-themes-org-agenda ; this is an alist: read the manual or its doc string
        '((header-block . (variable-pitch regular 1.4))
          (header-date . (bold-today grayscale underline-today 1.2))
          (event . (accented varied))
          (scheduled . uniform)
          (habit . nil))

        modus-themes-headings  ; this is an alist: read the manual or its doc string
        '((1 . (variable-pitch light 1.13))
          (2 . (variable-pitch regular 1.1))
          (3 . (variable-pitch regular 1.07))
          (4 . (monochrome 1.04))
          (5 . (1))
          (t . (rainbow 1)))

        ;; ;; For example:
        ;; modus-themes-headings
        ;; '((1 . (variable-pitch light 1.8))
        ;;   (2 . (variable-pitch regular 1.6))
        ;;   (3 . (variable-pitch regular 1.3))
        ;;   (4 . (monochrome 1.2))
        ;;   (5 . (1.1))
        ;;   (t . (rainbow 1.05)))
        ))

(with-no-warnings
  (defun tinker-theme (theme)
    "Tinker the THEME theme"
    (let ((class '((class color) (min-colors 89)))
          (fg         (--l?d "#37474F" "#ECEFF4"))
          (bg         (--l?d "#FFFFFF" "#2E3440"))
          (prompt     (--l?d "#F00056" "#FF2D51"))
          (match      (--l?d "#057748" "#BCE672"))
          (highlight  (--l?d "#FAFAFA" "#3B4252"))
          (critical   (--l?d "#FF6F00" "#EBCB8B"))
          (salient    (--l?d "#673AB7" "#81A1C1"))
          (strong     (--l?d "#000000" "#ECEFF4"))
          (popout     (--l?d "#FE8FA2" "#FFAB91"))
          (subtle     (--l?d "#ECEFF1" "#434C5E"))
          (faded      (--l?d "#B0BEC5" "#677691")))

      (custom-theme-set-faces
       theme

       ;; whitespace-line
       `(whitespace-line        ((,class :background "yellow" :foreground "purple")))

       ;; vertico
       `(vertico-current        ((,class :background ,faded)))

       ;; git-gutter
       `(git-gutter:added       ((,class :background "green")))
       `(git-gutter:deleted     ((,class :background "red")))
       `(git-gutter:modified    ((,class :background ,popout)))
       `(git-gutter:separator   ((,class :background ,salient)))
       `(git-gutter:unchanged   ((,class :background "purple")))
       ;; git-gutter-fr
       `(git-gutter-fr:added    ((,class :background "green")))
       `(git-gutter-fr:deleted  ((,class :background "red")))
       `(git-gutter-fr:modified ((,class :background ,popout)))

       ;; M-x prompt-face
       `(minibuffer-prompt      ((,class (:foreground ,prompt))))
       `(comint-highlight-input ((,class (:foreground "green" :bold t))))))
    (enable-theme theme))

  (defun radian/change-theme ()
    (interactive)
    (let* ((econf (car radian-theme-list))
           (theme (car econf))
           (theme-light/dark (cdr econf)))
      (setq radian-theme-list (append (cdr radian-theme-list) (list econf)))
      (disable-theme theme)
      (load-theme theme t)
      (tinker-theme theme))
    (radian-run-hooks 'radian-load-theme-hook)))

(-key "M-h" #'radian/change-theme)

(defun radian-init-ui-h (&optional _)
  "Inintialize user interface by applying all its advice and hooks"
  (advice-add #'run-hooks :override #'radian-run-hooks)
  (radian-run-hooks 'radian-init-ui-hook)

  (add-hook 'after-change-major-mode-hook #'radian-highlight-non-default-indentation-h 'append)

  ;; Initialize `radian-switch-window-hook' and `radian-switch-frame-hook'
  (add-hook 'window-selection-change-functions #'radian-run-switch-window-or-frame-hooks-h)
  ;; Initialize `radian-switch-buffer-hook'
  (add-hook 'window-buffer-change-functions #'radian-run-switch-buffer-hooks-h)
  ;; `window-buffer-change-functions' doesn't trigger for files visited via the server.
  (add-hook 'server-visit-hook #'radian-run-switch-buffer-hooks-h)

  ;; Only execute this function once.
  (remove-hook 'window-buffer-change-functions #'radian-init-ui-h))

;; Bootstrap UI FONT THEME config
(let ((hook (if (daemonp) 'server-after-make-frame-hook 'after-init-hook)))
  (add-hook hook #'radian-init-font-h -100)
  (add-hook hook #'radian/change-theme -90))

(unless meow-mode
  (message "START MEOW")
  (meow-global-mode +1)
  (radian-init-font-h t)
  (radian/change-theme))

;; Initialize UI as late as possible. `window-buffer-change-functions' runs
;; once, when the scratch/dashboard buffer is first displayed.
(add-hook 'window-buffer-change-functions #'radian-init-ui-h -100)

(unless noninteractive
  (add-hook 'after-change-major-mode-hook #'radian-run-local-var-hooks-maybe-h 100)
  (add-hook 'hack-local-variables-hook #'radian-run-local-var-hooks-h)
  (add-hook 'emacs-startup-hook #'radian-load-packages-incrementally-h)
  (add-hook 'window-setup-hook #'radian-display-benchmark-h 'append)
  (radian-run-hook-on 'radian-first-buffer-hook '(find-file-hook radian-switch-buffer-hook))
  (radian-run-hook-on 'radian-first-file-hook   '(find-file-hook dired-initial-position-hook))
  (radian-run-hook-on 'radian-first-input-hook  '(pre-command-hook))
  (add-hook 'radian-first-buffer-hook #'gcmh-mode))


(run-hooks 'radian--finalize-init-hook)

;; Local Variables:
;; no-native-compile: t
;; End:
