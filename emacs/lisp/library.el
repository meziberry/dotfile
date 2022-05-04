;; library.el --- -*- lexical-binding: t -*-

;;
;;; Some tiny function
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
     ,@(cl-loop for (var val) in envvars
                collect `(setenv ,var ,val))
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

(defmacro eval-unless! (cond &optional doc &rest args)
  "Exclude exps according to COND"
  (declare (doc-string 2)
           (indent (lambda (_ s) (goto-char (elt s 1)) (current-column))))
  (unless (stringp doc) (push doc args))
  (unless (eval cond) (macroexp-progn args)))

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

(defun radian--fn-crawl (data args)
  (cond ((symbolp data)
         (when-let*
             ((lookup '(_ _ %2 %3 %4 %5 %6 %7 %8 %9))
              (pos (cond ((eq data '%*) 0)
                         ((memq data '(% %1)) 1)
                         ((cdr (assq data (seq-map-indexed #'cons lookup)))))))
           (when (and (= pos 1)
                      (aref args 1)
                      (not (eq data (aref args 1))))
             (error "%% and %%1 are mutually exclusive"))
           (aset args pos data)))
        ((and (not (eq (car-safe data) '!))
              (or (listp data)
                  (vectorp data)))
         (seq-doseq (elt data)
           (radian--fn-crawl elt args)))))

(defmacro fn!! (&rest args)
  "Return an lambda with implicit, positional arguments.
The function's arguments are determined recursively from ARGS.  Each symbol from
`%1' through `%9' that appears in ARGS is treated as a positional argument.
Missing arguments are named `_%N', which keeps the byte-compiler quiet.  `%' is
a shorthand for `%1'; only one of these can appear in ARGS.  `%*' represents
extra `&rest' arguments.
Instead of:
  (lambda (a _ c &rest d)
    (if a c (cadr d)))
you can use this macro and write:
  (fn!! (if %1 %3 (cadr %*)))
which expands to:
  (lambda (%1 _%2 %3 &rest %*)
    (if %1 %3 (cadr %*)))
This macro was adapted from llama.el (see https://git.sr.ht/~tarsius/llama),
minus font-locking, the outer function call, and minor optimizations."
  `(lambda ,(let ((argv (make-vector 10 nil)))
         (radian--fn-crawl args argv)
         `(,@(let ((n 0))
               (mapcar (lambda (sym)
                         (cl-incf n)
                         (or sym (intern (format "_%%%s" n))))
                       (reverse (seq-drop-while
                                 #'null (reverse (seq-subseq argv 1))))))
           ,@(and (aref argv 0) '(&rest %*))))
     ,@args))

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
  "A more sensible `setopt' for setting customizable variables.

This can be used as a drop-in replacement for `setq' and *should* be used
instead of `setopt'. Unlike `setq', this triggers custom setters on variables.
Unlike `setopt', this won't needlessly pull in dependencies."
  (macroexp-progn
   (cl-loop for (var val) on settings by 'cddr
            collect `(funcall (or (get ',var 'custom-set) #'set)
                              ',var ,val))))

(defmacro delq! (elt list &optional fetcher)
  "`delq' ELT from LIST in-place.

If FETCHER is a function, ELT is used as the key in LIST (an alist)."
  `(setq ,list (delq ,(if fetcher
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
(defmacro lpp (lst)
  "pp list"
  `(cl-loop for i in ,lst
            do (princ i) (terpri)
            finally (return
                     (let ((v [:length nil :items nil]))
                       (aset v 1 (length ,lst))
                       (aset v 3 ,lst)
                       v))))

;;; Helper & Reference & Log
(defmacro defhelper (name &rest doc-strings)
  "Make a helper function of NAME-helper to show the DOC-STRINGS."
  (let ((fun-name (intern (concat "h/" (symbol-name name))))
        (doc-string (mapconcat 'identity doc-strings "\n")))
    `(defun ,fun-name () ,doc-string
            (interactive)
            (describe-function ',fun-name))))

(cl-defmacro myc-make-action ((&rest args) &body body)
  (declare (indent 1))
  `(lambda ()
     (interactive)
     (put 'quit 'error-message "")
     (run-at-time nil nil
                  (lambda (,@args)
                    (put 'quit 'error-message "Quit")
                    (with-demoted-errors "Error: %S"
                      ,@body))
                  ,@(seq-take
                     `((consult-vertico--candidate)
                       (minibuffer-contents))
                     (length args)))
     (abort-recursive-edit)))

(defmacro defref (name &rest refs)
  "Make a function of NAME to browse the REFS."
  (declare (indent defun))
  (let* ((flatten (cl-loop for item in refs
                           if (stringp item) collect item
                           if (listp item) append
                           (if (stringp (car item))
                               (cl-loop for r in item collect (eval r))
                             (list (eval item)))))
         (consed (if (null flatten) (user-error "No suitable reference.")
                   (cl-loop for item in flatten
                            if (string-match "^\\(.*\\): \\(.*\\)$" item) collect
                            (cons (match-string 2 item) (match-string 1 item))
                            else collect (cons item nil))))
         (formatted (cl-loop for item in consed
                             for ref = (car item)
                             if (not (string-prefix-p "http" ref)) do
                             (setq ref (format "https://github.com/%s" ref))
                             collect (cons ref (cdr item))))
         (propertized (cl-loop with face = 'font-lock-doc-face
                               for item in formatted
                               for label = (cdr item)
                               if label do
                               (let ((len (cl-loop for i in formatted if (cdr i) maximize (1+ (length (car i))))))
                                 (setq label (format (format "%%-%ds (%%s)" len) (car item) label))
                                 (add-face-text-property (length (car item)) (length label) face nil label))
                               else do
                               (setq label (car item))
                               collect label))
         (fun (intern (format (concat (unless (string-match-p "/" (symbol-name name)) "r/") (if (cdr flatten) "%s*" "%s")) name))))
    `(defun ,fun ()
       ,(format "%s\n\nBrowser/Yank it." propertized)
       (interactive)
       (let* ((refs ',propertized)
              (ref (car (split-string
                         (minibuffer-with-setup-hook
                             (lambda ()
                               (let ((map (make-composed-keymap nil (current-local-map))))
                                 (define-key map (kbd "M-w")
                                             (myc-make-action (ref)
                                               (setq ref (car (split-string ref " ")))
                                               (kill-new ref)
                                               (message "Copy REF: %s" ref)))
                                 (use-local-map map)))
                           (completing-read "REF: " refs nil t))
                         " "))))
         (when (string-match "\\[\\(.+\\)\\]" ref)
           (setq ref
                 (string-replace
                  (match-string 0 ref)
                  (read-string (format "%s: " (match-string 1 ref)) nil nil (match-string 1 ref))
                  ref)))
         (browse-url ref)))))


;;; Wraps of leaf and straight
(defmacro x (NAME &rest args)
  "Flags: e/demand s/straight b/blackout i/increment n/loading-nil d/disabled -/ignore."
  (declare (indent defun))
  (pcase-let* ((`(,name ,flags) (split-string (symbol-name NAME) "/"))
               (name (intern name))
               (doc-strings (cl-loop for i in args until (keywordp i) collect i))
               (options (cl-set-difference args doc-strings))
               (refs (prog1 (plist-get options :url) (cl-remf options :url)))
               (fopts (cl-loop
                       for (c . p) in
                       '((?e . (:require t))
                         (?s . (:straight t))
                         (?b . (:blackout t))
                         (?i . (:increment t))
                         (?n . (:loading nil))
                         (?d . (:disabled t))
                         (?m . (:disabled mini-p)))
                       when (cl-find c flags) append p))
               (inactive (if (cl-find ?- flags) t nil)))
    (delq nil
          `(progn
             ,(if doc-strings `(defhelp ,name ,@doc-strings))
             ,(if refs `(defref ,name ,refs))
             ,(if inactive
                  ;; still run the first sexp of :init when inactive
                  ;; `,@(plist-get options :init)
                  `(leaf ,name :straight
                     ,(or (plist-get options :straight) (plist-get fopts :straight)))
                `(leaf ,name ,@options ,@fopts))))))

(defmacro w (name &rest args)
  "Like `x' with :straight t. =(x pkg/s)"
  (declare (indent defun))
  (let ((n (symbol-name name)))
    `(x ,(intern (concat `,n (if (cl-find ?/ `,n) "s" "/s"))) ,@args)))

(defalias 'sup #'straight-use-package)
(defalias '-key 'leaf-key)
(defalias '-key* 'leaf-key*)
(defalias '-keys 'leaf-keys)
(defalias '-keys* 'leaf-keys*)
(defalias 'mkey 'leaf-key-bind-keymap)
(defalias 'mkey* 'leaf-key-bind-keymap*)
(defalias 'mkeys 'leaf-keys-bind-keymap)
(defalias 'mkeys* 'leaf-keys-bind-keymap*)

;; Local Variables:
;; indent-tabs-mode: nil
;; no-native-compile: t
;; sentence-end-double-space: nil
;; End:
