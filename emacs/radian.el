;; -*- coding: utf-8; lexical-binding: t; -*-

;; To see the outline of this file, run M-x outline-minor-mode and
;; then press C-c @ C-t. To also show the top-level functions and
;; variable declarations in each section, run M-x occur with the
;; following query: ^;;;;* \|^(

;;; Detect stale bytecode
;; If Emacs version changed, the bytecode is no longer valid and we
;; must recompile. Also, if the location of Radian changed, our
;; dotfile-finding functions are defined incorrectly and we must
;; recompile.
(eval
 `(unless (equal (list (emacs-version) radian-lib-file)
                 ',(eval-when-compile
                     (list (emacs-version) radian-lib-file)))
    (throw 'stale-bytecode nil)))

;;; Comp
(defconst IS-NATIVECOMP (if (fboundp 'native-comp-available-p) (native-comp-available-p)))
;; (if (fboundp 'startup-redirect-eln-cache) (startup-redirect-eln-cache "cache/eln"))

(and
 IS-NATIVECOMP
 (with-eval-after-load 'comp
   ;; NOTE: Some variable is defined in `init.el', load it when
   ;; native-comp-async. `defalias' will cause `native--compile-async'
   ;; to compiling the file located.
   (customize-set-variable
    'native-comp-async-env-modifier-form
    '(load (expand-file-name "init.el" user-emacs-directory) t t t))
   (customize-set-variable 'native-comp-compiler-options '("-O2" "-mtune=native"))
   ;; Disable byte-compilation warnings from native-compiled
   ;; packages from being reported asynchronously into the UI.
   (customize-set-variable 'native-comp-async-report-warnings-errors nil)))

;;; Radian Variables/Hooks
(defvar radian--current-feature 'core "The feature loading")

(defconst IS-EMACS29+   (> emacs-major-version 28))
(defconst IS-EMACS28+   (> emacs-major-version 27))
(defconst IS-MAC        (eq system-type 'darwin))
(defconst IS-LINUX      (eq system-type 'gnu/linux))
(defconst IS-WINDOWS    (memq system-type ' (cygwin windows-nt ms-dos)))
(defconst IS-BSD        (or IS-MAC (eq system-type 'berkeley-unix)))

(defvar radian-debug-p (or (getenv-internal "DEBUG") init-file-debug)
  "If non-nil, Radian will log more.

Use `radian-debug-mode' to toggle it. The --debug-init flag and
setting the DEBUG envvar will enable this at startup.")

(defconst radian-interactive-p (not noninteractive)
  "If non-nil, Emacs is in interactive mode.")

(defvar radian-init-time nil
  "The time it took, in seconds, for Doom Emacs to initialize.")

;; Directories/files
(defconst *radian-directory* (file-name-directory
                              (directory-file-name
                               (file-name-directory
                                radian-lib-file)))
  "Path to the Radian Git repository.")
(defconst *local-directory* (file-name-directory
                             (directory-file-name
                              (file-name-directory
                               (file-truename radian-local-init-file))))
  "Path to the emacs local configuration Git repository.")

(defconst *radian-contrib/* (concat *radian-directory* "emacs/contrib/")
  "Radian contrib directory for manual third-party packages")
(defsubst -contrib/ (&rest string) (apply #'concat *radian-contrib/* string))

(defconst *radian-lisp/* (concat *radian-directory* "emacs/lisp/")
  "Radian lisp directory for cunstom config")
(defsubst -lisp/ (&rest string) (apply #'concat *radian-lisp/* string))

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

;; Remember these variables' initial values, so we can safely reset them at a
;; later time, or consult them without fear of contamination.
(dolist (var '(exec-path load-path process-environment))
  (unless (get var 'initial-value)
    (put var 'initial-value (default-value var))))

;; Custom error types
(define-error 'radian-error "Error in Radian")
(define-error 'radian-hook-error "Error in a Radian hook" 'radian-error)
(define-error 'radian-local-error "Error in local config" 'radian-error)

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

(defvar radian-theme-list '((modus-vivendi . t) (modus-operandi . nil))
  "Theme sequence of changing. `(THEME-NAME . IS-DARK-THEME)'")

;;; Radian-require
(defmacro req! (name &optional path)
  "Load Radian sub configuration file (in *lisp/*). NAME is filename."
  (let ((file (concat (or path *radian-lisp/*) (symbol-name name) ".el")))
    (if byte-compile-current-file
        (let ((forms nil))
          (with-temp-buffer
            (ignore-errors
              ;; Can't do this literally because it breaks Unicode
              ;; characters.
              (insert-file-contents file))
            (condition-case _
                (while t
                  (let ((form (read (current-buffer))))
                    (push form forms)))
              (end-of-file)))
          (setq forms (nreverse forms))
          `(progn ,@forms))
      `(load ,file 'noerror 'nomessage 'nosuffix))))

;;; Load some libraries.
;; Load utility libraries
(require 'cl-lib)
(require 'map)
(require 'subr-x)
(req! library)

;;; MODULE {option-packages}
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

(defcustom radian-before-straight-hook nil
  "Hook run just before Radian bootstraps straight.el.
For use with `radian-local-on-hook' in init.local.el."
  :group 'radian-hooks
  :type 'hook)

(defcustom radian-after-init-hook nil
  "Hook run after at the very end of init.
For use with `radian-local-on-hook' in init.local.el."
  :group 'radian-hooks
  :type 'hook)

(defvar radian--hook-contents nil
  "Alist mapping local init hooks to lists of forms.
This is used to embed local init hook code directly into the
init-file at the appropriate places during byte-compilation,
without breaking macro-expansion.")
;; Idempotency.
(setq radian--hook-contents nil)

;; Allow binding this variable dynamically before straight.el has been
;; loaded.
(defvar straight-current-profile)

(defmacro radian--load-local-init-file ()
  "Load local init-file, with crazy hacks for byte-compilation.
In particular, if we are byte-compiling, actually macroexpand to
the entire contents of the local init-file, except that the
bodies of invocations to `radian-local-on-hook' are recorded in
`radian--hook-contents'. Otherwise just load the file like
usual."
  (if byte-compile-current-file
      (let ((forms nil))
        (with-temp-buffer
          (ignore-errors
            ;; Can't do this literally because it breaks Unicode
            ;; characters.
            (insert-file-contents radian-local-init-file))
          (condition-case _
              (while t
                (let ((form (read (current-buffer))))
                  (if (and (listp form)
                           (eq (nth 0 form) #'radian-local-on-hook)
                           (nth 1 form)
                           (symbolp (nth 1 form))
                           (nthcdr 2 form))
                      (let* ((name (nth 1 form))
                             (body (nthcdr 2 form))
                             (hook (intern (format "radian-%S-hook" name)))
                             (link (assq hook radian--hook-contents)))
                        (unless link
                          (setq link (cons hook nil))
                          (push link radian--hook-contents))
                        (dolist (subform body)
                          (push subform (cdr link))))
                    (push form forms))))
            (end-of-file)))
        (setq forms (nreverse forms))
        (dolist (link radian--hook-contents)
          (setf (cdr link)
                (nreverse (cdr link))))
        `(progn ,@forms))
    `(load radian-local-init-file 'noerror 'nomessage)))

(defmacro radian-local-on-hook (name &rest body)
  "Register some code to be run on one of Radian's hooks.
The hook to be used is `radian-NAME-hook', with NAME an unquoted
symbol, and the code which is added is BODY wrapped in a `progn'.
See \\[customize-group] RET radian-hooks RET for a list of hooks
which you can use with this macro in your local init-file.

Using this macro instead of defining functions and adding them to
Radian's hooks manually means that a lot of magic happens which
allows Radian to embed your entire local init-file into Radian
during byte-compilation without breaking macroexpansion in
unexpected ways."
  (declare (indent (lambda (_ s) (goto-char (elt s 1)) (current-column))))
  (let ((func-name (intern (format "radian-local--%S" name)))
        (hook (intern (format "radian-%S-hook" name))))
    `(add-hook! ',hook
       (defun ,func-name ()
         "Automatically-generated local hook function."
         ,@body))))

(defmacro radian--run-hook (name)
  "Run the given local init HOOK.
The hook to be used is `radian-NAME-hook', with NAME an unquoted
symbol. This binds `straight-current-profile', and also has some
gnarly hacks to allow Radian to embed the entire contents of the
hook directly into the init-file during byte-compilation."
  (declare (indent 0))
  (let ((hook (intern (format "radian-%S-hook" name))))
    `(let ((straight-current-profile 'radian-local))
       (run-hooks ',hook)
       ,@(when byte-compile-current-file
           (alist-get hook radian--hook-contents)))))

;;;;;;; -> Load <local init file>
;; Allow to disable local customizations with a
;; command-line argument.
(if (member "--no-local" command-line-args)
    ;; Make sure to delete --no-local from the list, because
    ;; otherwise Emacs will issue a warning about the unknown
    ;; argument.
    (setq command-line-args (delete "--no-local" command-line-args))
  ;; Load local customizations.
  (radian--load-local-init-file))

;;; Default optimizations

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
(unless IS-MAC   (setq command-line-ns-option-alist nil))
(unless IS-LINUX (setq command-line-x-option-alist nil))

;; Contrary to what many Emacs users have in their configs, you really don't
;; need more than this to make UTF-8 the default coding system:
(if IS-WINDOWS
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
                (if (and (not IS-WINDOWS)
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
(defvar radian-comma-keymap (make-sparse-keymap)
  "Keymap for Radian commands that should be put under a comma prefix.
This keymap is bound under \\[radian-comma-keymap].")

(defvar radian-keymap (make-sparse-keymap)
  "Keymap for Radian commands that should be put under a prefix.
This keymap is bound under \\[radian-keymap].")

(define-key global-map (kbd "M-P") radian-keymap)

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


;;;;; --> Mini init-file.
;; We need the minimum init-file for some emergency.
(defvar mini-p nil)
(if (member "--mini" command-line-args)
    (setq mini-p t command-line-args (delete "--mini" command-line-args)))

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

;;;;;;; --> Expose <before-straight-hook> contents
(radian--run-hook before-straight)

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
(cl-pushnew "autoload/*.el" straight-default-files-directive)

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
(-ow leaf
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
(-ow meow
  :straight (meow :repo "meziberry/meow" :branch "develop")
  :custom
  (meow-use-cursor-position-hack . t)
  (meow-use-enhanced-selection-effect . t)
  (meow-selection-command-fallback . '((meow-save . meow-keypad-start)
                                       (meow-change . meow-keypad-start)
                                       (meow-cancel-selection . meow-keypad-start)
                                       (meow-reverse . meow-open-below)
                                       (meow-replace . meow-replace-char)
                                       (meow-pop-selection . meow-pop-grab)
                                       (meow-kill . meow-C-k)))

  :hook (after-init-hook . meow-global-mode)
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

  :blackout
  (meow-normal-mode meow-motion-mode meow-keypad-mode meow-insert-mode))

;;; MODULE {Radian-foundation}
;; autoloads
(sup `(radian-autoload :local-repo ,(concat *radian-directory* "emacs/autoload")
                       :type nil :build (:not compile)))
(sup `(local-autoload :local-repo ,(concat *local-directory* "emacs/autoload")
                      :type nil :build (:not compile)))

;;;; gcmh-mode
(pow gcmh :blackout t)

;;;; Hide-mode-line
(pow hide-mode-line
  :hook ((completion-list-mode-hook Man-mode-hook). hide-mode-line-mode))

;;;; MODE-local-vars-hook

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

(defun radian-protect-fallback-buffer-h ()
  "Don't kill the scratch buffer. Meant for `kill-buffer-query-functions'."
  (not (eq (current-buffer) (radian-fallback-buffer))))
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

(defmacro radian-register-dotfile
    (filename &optional keybinding pretty-filename)
  "Establish functions and keybindings to open a dotfile.

The FILENAME should be a path relative to the user's home
directory. Two interactive functions are created: one to find the
file in the current window, and one to find it in another window.

If KEYBINDING is non-nil, the first function is bound to that key
sequence after it is prefixed by \"\\[radian-keymap] e\", and the
second function is bound to the same key sequence, but prefixed
instead by \"\\[radian-keymap] o\".

This is best demonstrated by example. Suppose FILENAME is
\".emacs.d/init.el\" and KEYBINDING is \"e i\". Then
`radian-register-dotfile' will create the interactive functions
`radian-find-init-el' and `radian-find-init-el-other-window', and
it will bind them to the key sequences \"\\[radian-keymap] e e
i\" and \"\\[radian-keymap] o e i\" respectively.

If PRETTY-FILENAME, a string, is non-nil, then it will be used in
place of \"init-el\" in this example. Otherwise, that string will
be generated automatically from the basename of FILENAME.

To pass something other than a literal string as FILENAME,
unquote it using a comma."
  (when (and (listp filename) (eq (car filename) '\,))
    (setq filename (eval (cadr filename))))
  (let* ((bare-filename (replace-regexp-in-string ".*/" "" filename))
         (full-filename (expand-file-name filename "~"))
         (defun-name (intern
                      (replace-regexp-in-string
                       "-+"
                       "-"
                       (concat
                        "radian-find-"
                        (or pretty-filename
                            (replace-regexp-in-string
                             "[^a-z0-9]" "-"
                             (downcase
                              bare-filename)))))))
         (defun-other-window-name
          (intern
           (concat (symbol-name defun-name)
                   "-other-window")))
         (docstring (format "Edit file %s." full-filename))
         (docstring-other-window
          (format "Edit file %s, in another window."
                  full-filename))
         (defun-form `(defun ,defun-name ()
                        ,docstring
                        (interactive)
                        (when (or (file-exists-p ,full-filename)
                                  (yes-or-no-p
                                   ,(format
                                     "Does not exist, really visit %s? "
                                     (file-name-nondirectory
                                      full-filename))))
                          (find-file ,full-filename))))
         (defun-other-window-form
          `(defun ,defun-other-window-name ()
             ,docstring-other-window
             (interactive)
             (when (or (file-exists-p ,full-filename)
                       (yes-or-no-p
                        ,(format
                          "Does not exist, really visit %s? "
                          (file-name-nondirectory
                           full-filename))))
               (find-file-other-window ,full-filename))))
         (full-keybinding
          (when keybinding
            (radian-join-keys "e" keybinding)))
         (full-other-window-keybinding
          (radian-join-keys "o" keybinding)))
    `(progn
       ,defun-form
       ,defun-other-window-form
       ,@(when full-keybinding
           `((radian-bind-key ,full-keybinding #',defun-name)))
       ,@(when full-other-window-keybinding
           `((radian-bind-key
              ,full-other-window-keybinding
              #',defun-other-window-name)))
       ;; Return the symbols for the two functions defined.
       (list ',defun-name ',defun-other-window-name))))

;; Now we register shortcuts to files relevant to Radian.

(radian-register-dotfile ,*radian-directory* "r a" "radian-repo")

;; Emacs
(radian-register-dotfile
 ,(expand-file-name "init.el" user-emacs-directory)
 "e i")
(radian-register-dotfile
 ,(expand-file-name "early-init.el" user-emacs-directory)
 "e e")
(radian-register-dotfile
 ,(expand-file-name "emacs/radian.el" *radian-directory*)
 "e r")
(radian-register-dotfile ,(-lisp/ "library.el") "e b")
(radian-register-dotfile
 ,(expand-file-name "straight/versions/radian.el" user-emacs-directory)
 "e v" "radian-versions-el")
(radian-register-dotfile
 ,(expand-file-name "init.local.el" user-emacs-directory) "e l")
(radian-register-dotfile
 ,(expand-file-name "straight/versions/radian-local.el" user-emacs-directory)
 "e V" "radian-local-versions-el")

;; Git
(radian-register-dotfile ".gitconfig" "g c")
(radian-register-dotfile ".gitexclude" "g e")
(radian-register-dotfile ".gitconfig.local" "g l")

;; Shell
(radian-register-dotfile ".profile" "p r")
(radian-register-dotfile ".profile.local" "p l")

;; Tmux
(radian-register-dotfile ".tmux.conf" "t c")
(radian-register-dotfile ".tmux.local.conf" "t l")

;; Zsh
(radian-register-dotfile ".zshrc" "z r")
(radian-register-dotfile ".zshrc.local" "z l")

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

;;;; Prevent Emacs-provided Org from being loaded

;; Our real configuration for Org comes much later. Doing this now
;; means that if any packages that are installed in the meantime
;; depend on Org, they will not accidentally cause the Emacs-provided
;; (outdated and duplicated) version of Org to be loaded before the
;; real one is registered.
;;
;; Use my mirror of Org because the upstream has *shockingly*
;; atrocious uptime (namely, the entire service will just go down for
;; more than a day at a time on a regular basis). Unacceptable because
;; it keeps breaking Radian CI.
(unless mini-p
  (sup '(org :host github :repo "emacs-straight/org-mode" :files (:defaults "etc"))))

;;
;;;; Keybinds
;; Emacs Keybings search precedence
;;(or (if overriding-terminal-local-map
;;        (find-in overriding-terminal-local-map))
;;    (if overriding-local-map
;;        (find-in overriding-local-map)
;;      (or (find-in (get-char-property (point) 'keymap))
;;          (find-in-any emulation-mode-map-alists)
;;          -------->Evil/Meow keymaps<--------
;;          (find-in-any minor-mode-overriding-map-alist)
;;          (find-in-any minor-mode-map-alist)
;;          (if (get-text-property (point) 'local-map)
;;              (find-in (get-char-property (point) 'local-map))
;;            (find-in (current-local-map)))))
;;    (find-in (current-global-map)))
;; ----------------------------------------------------------------
;; Evil keymap's order. Evil map locate in emulation-mode-map-alist
;;        Intercept keymaps - evil-make-intercept-map
;;        Local state keymap - evil-local-set-key
;;        Minor-mode keymaps - evil-define-minor-mode-key
;;        Auxiliary keymaps - evil-define-key
;;        Overriding keymaps - evil-make-overriding-map
;;        Global state keymap - evil-global-set-key

;; General keybindings
(-keys ((radian-comma-keymap ("tf" . radian/toggle-profiler))))

(eval-cond!
  (IS-MAC
   ;; mac-* variables are used by the special emacs-mac build of Emacs by
   ;; Yamamoto Mitsuharu, while other builds use ns-*.
   (setq mac-command-modifier      'super
         mac-option-modifier       'meta
         ;; Free up the right option for character composition
         mac-right-option-modifier 'none))
  (IS-WINDOWS
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

;; Package `which-key' displays the key bindings and associated
;; commands following the currently-entered key prefix in a popup.
(pow! which-key
  :config

  ;; We configure it so that `which-key' is triggered by typing C-h
  ;; during a key sequence (the usual way to show bindings). See
  ;; <https://github.com/justbur/emacs-which-key#manual-activation>.
  ;; (setq which-key-show-early-on-C-h t)
  ;; (setq which-key-idle-delay most-positive-fixnum)
  ;; (setq which-key-idle-secondary-delay 1e-100)

  (which-key-mode +1)

  :blackout t)

;;;; Universal, non-nuclear escape

;; `keyboard-quit' is too much of a nuclear option. I wanted an ESC/C-g to
;; do-what-I-mean. It serves four purposes (in order):
;;
;; 1. Quit active states; e.g. highlights, searches, snippets, iedit,
;;    multiple-cursors, recording macros, etc.
;; 2. Close popup windows remotely (if it is allowed to)
;; 3. Refresh buffer indicators, like git-gutter and flycheck
;; 4. Or fall back to `keyboard-quit'
;;
;; And it should do these things incrementally, rather than all at once. And it
;; shouldn't interfere with recording macros or the minibuffer. This may require
;; you press ESC/C-g two or three times on some occasions to reach
;; `keyboard-quit', but this is much more intuitive.

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

;;;; all-the-icons
(pow! all-the-icons
  :commands (all-the-icons-octicon
             all-the-icons-faicon
             all-the-icons-fileicon
             all-the-icons-wicon
             all-the-icons-material
             all-the-icons-alltheicon)
  :preface
  (add-hook! 'after-setting-font-hook
    (defun radian-init-all-the-icons-fonts-h ()
      (when (fboundp 'set-fontset-font)
        (dolist (font (list "Weather Icons"
                            "github-octicons"
                            "FontAwesome"
                            "all-the-icons"
                            "file-icons"
                            "Material Icons"))
          (set-fontset-font t 'unicode font nil 'append)))))
  :config
  (cond ((daemonp)
         (defadvice! radian--disable-all-the-icons-in-tty-a (fn &rest args)
           "Return a blank string in tty Emacs, which doesn't support multiple fonts."
           :around '(all-the-icons-octicon
                     all-the-icons-material
                     all-the-icons-faicon all-the-icons-fileicon
                     all-the-icons-wicon all-the-icons-alltheicon)
           (if (or (not after-init-time) (display-multi-font-p))
               (apply fn args)
             "")))
        ((not (display-graphic-p))
         (defadvice! radian--disable-all-the-icons-in-tty-a (&rest _)
           "Return a blank string for tty users."
           :override '(all-the-icons-octicon
                       all-the-icons-material
                       all-the-icons-faicon all-the-icons-fileicon
                       all-the-icons-wicon all-the-icons-alltheicon)
           ""))))


;;; MODULE {Environment}

;;;; Environment variables

;; Unix tools look for HOME, but this is normally not defined on Windows.
(when-let (realhome
           (and IS-WINDOWS
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
(when IS-MAC
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

(-keys (("C-x |" . split-window-horizontally-instead)
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

(-ow! popup
  :straight
  `(popup :local-repo ,(-contrib/ "popup/") :type nil
          :files ("*.el" "autoload/*")
          :build (:not compile))
  :require t)

;; (pow! shackle
;;   :hook (radian-first-buffer-hook . shackle-mode)
;;   :init
;;   ;; This function is derived from split-window-sensibly
;;   (defun shackle-align (&optional window)
;;     (let ((window (or window (selected-window))))
;;       (or (and (window-splittable-p window t) 'right)
;;           (and (window-splittable-p window) 'below)
;;           (and
;;            ;; If WINDOW is the only usable window on its frame (it is
;;            ;; the only one or, not being the only one, all the other
;;            ;; ones are dedicated) and is not the minibuffer window, try
;;            ;; to split it vertically disregarding the value of
;;            ;; `split-height-threshold'.
;;            (let ((frame (window-frame window)))
;;              (or (eq window (frame-root-window frame))
;;                  (catch 'done
;;                    (walk-window-tree
;;                     (lambda (w) (unless (or (eq w window) (window-dedicated-p w))
;;                                   (throw 'done nil)))
;;                     frame)
;;                    t)))
;;            (not (window-minibuffer-p window))
;;            (let ((split-height-threshold 0))
;;              (when (window-splittable-p window) 'below))))))
;;   :setq
;;   (shackle-rules
;;    .
;;    '((compilation-mode :select nil)
;;      ("*help.*" :regexp t :modeline nil :other t :select nil :same nil :align shackle-align :size 0.5)))
;;   :config/el-patch
;;   (defun shackle--display-buffer (buffer alist plist)
;;     "Internal function for `shackle-display-buffer'.
;; Displays BUFFER according to ALIST and PLIST."
;;     ;; HACK: Add the :modeline keyword for shackle-rules.
;;     (unless (plist-get plist :modeline)
;;       (push
;;        `(window-parameters
;;          .
;;          ,(append (alist-get 'window-parameters alist) '((mode-line-format . none))))
;;        alist))
;;     (cond
;;      ((plist-get plist :custom)
;;       (let* ((action (plist-get plist :custom))
;;              (window (funcall action buffer alist plist)))
;;         (when (and window (not (windowp window)))
;;           (user-error "Custom action didn't return window: %S %S" window action))
;;         window))
;;      ((plist-get plist :ignore) 'fail)
;;      ((shackle--display-buffer-reuse buffer alist))
;;      ((or (plist-get plist :same)
;;           ;; there is `display-buffer--same-window-action' which things
;;           ;; like `info' use to reuse the currently selected window, it
;;           ;; happens to be of the (inhibit-same-window . nil) form and
;;           ;; should be permitted unless a popup is requested
;;           (and (not (plist-get plist :popup))
;;                (and (assq 'inhibit-same-window alist)
;;                     (not (cdr (assq 'inhibit-same-window alist))))))
;;       (shackle--display-buffer-same buffer alist))
;;      ((plist-get plist :frame)
;;       (funcall shackle-display-buffer-frame-function buffer alist plist))
;;      ((plist-get plist :align)
;;       (shackle--display-buffer-aligned-window buffer alist plist))
;;      (t
;;       (shackle--display-buffer-popup-window buffer alist plist)))))

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

;;;; HEAD-CORE 

;;; MODULE {Vertico}

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
  (add-to-list
   'completion-styles-alist
   '(+vertico-basic-remote
     +vertico-basic-remote-try-completion
     +vertico-basic-remote-all-completions
     "Use basic completion on remote files only"))
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        ;; note that despite override in the name orderless can still be used in
        ;; find-file etc.
        completion-category-overrides '((file (styles +vertico-basic-remote orderless partial-completion)))
        orderless-style-dispatchers '(+vertico-orderless-dispatch)
        orderless-component-separator "[ &]")
  ;; ...otherwise find-file gets different highlighting than other commands
  (set-face-attribute 'completions-first-difference nil :inherit nil))

;;;; Consult
(pow consult

  :preface
  (pow! consult-dir
    :after vertico consult
    :bind (([remap list-directory] . consult-dir)
           (vertico-map
            ("C-x C-d" . consult-dir)
            ("C-x C-j" . consult-dir-jump-file))))
  :init
  (-keys
   (([remap apropos]                      . consult-apropos)
    ([remap bookmark-jump]                . consult-bookmark)
    ([remap meow-pop-to-mark]             . consult-mark)
    ([remap goto-line]                    . consult-goto-line)
    ([remap imenu]                        . consult-imenu)
    ([remap locate]                       . consult-locate)
    ([remap load-theme]                   . consult-theme)
    ([remap man]                          . consult-man)
    ([remap recentf-open-files]           . consult-recent-file)
    ([remap switch-to-buffer]             . consult-buffer)
    ([remap switch-to-buffer-other-window]. consult-buffer-other-window)
    ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
    ([remap yank-pop]                     . consult-yank-pop)
    ([remap persp-switch-to-buffer]       . +vertico/switch-workspace-buffer)
    (radian-comma-keymap
     ("g"  . project-or-external-find-regexp)
     ("/"  . +vertico/project-search)
     ("fr" . consult-recent-file)
     ("ff" . +vertico/consult-fd))))

  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)
  (advice-add #'multi-occur :override #'consult-multi-occur)

  :defer-config

  (defadvice! +vertico--consult-recent-file-a (&rest _args)
    "`consult-recent-file' needs to have `recentf-mode' on to work correctly"
    :before #'consult-recent-file
    (recentf-mode +1))

  (setq consult-project-root-function #'radian-project-root
        consult-narrow-key "<"
        consult-line-numbers-widen t
        consult-async-min-input 2
        consult-async-refresh-delay  0.15
        consult-async-input-throttle 0.2
        consult-async-input-debounce 0.1)

  (setq +vertico-consult-fd-args
        (if radian--fd-binary
            (format "%s --color=never -i -H -E .git --regex %s"
                    radian--fd-binary
                    (if IS-WINDOWS "--path-separator=/" ""))
          consult-find-args))

  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file
   ;; +default/search-project +default/search-other-project
   ;; +default/search-cwd +default/search-other-cwd
   ;; +default/search-notes-for-symbol-at-point
   ;; consult--source-file consult--source-project-file consult--source-bookmark
   :preview-key (kbd "C-SPC"))

  (consult-customize
   consult-theme
   :preview-key
   (list (kbd "C-SPC") :debounce 0.5 'any))

  (after! org
    (defvar +vertico--consult-org-source
      `(:name "Org"
              :narrow   ?o
              :hidden   t
              :category buffer
              :state    ,#'consult--buffer-state
              :items    ,(lambda () (mapcar #'buffer-name (org-buffer-list)))))
    (add-to-list 'consult-buffer-sources '+vertico--consult-org-source 'append))

  (-keys (consult-crm-map
          ([tab]     . +vertico/crm-select)
          ([backtab] . +vertico/crm-select-keep-input)
          ([return]  . +vertico/crm-exit)
          ("RET"     . +vertico/crm-exit))))

;;;; Embark
(-ow embark
  :straight (embark :type git :host github :repo "oantolin/embark"
                    :files ("embark-consult.el" "embark.el" "embark.texi"
                            "avy-embark-collect.el"))
  :init
  (-keys (minibuffer-local-map
          ("C-;" . embark-export)
          ("C-c C-s" . embark-collect-snapshot)
          ("C-c C-e" . +vertico/embark-export-write)))

  :bind
  ([remap describe-bindings] . embark-bindings)
  ("M-;" . embark-act)
  ("M-." . embark-dwim)
  ("C-x g" . (cmds! (featurep 'magit-status) #'magit-status #'+vertico/embark-magit-status))
  :config
  (setq which-key-use-C-h-commands nil
        prefix-help-command #'embark-prefix-help-command)

  (defadvice! +vertico--embark-which-key-prompt-a (fn &rest args)
    "Hide the which-key indicator immediately when using the
completing-read prompter."
    :around #'embark-completing-read-prompter
    (which-key--hide-popup-ignore-command)
    (let ((embark-indicators
           (remq 'embark-which-key-indicator embark-indicators)))
      (apply fn args)))

  (cl-nsubstitute #'+vertico-embark-which-key-indicator #'embark-mixed-indicator embark-indicators))


(-ow embark-consult
  :after embark consult
  :require t
  :config (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode))

(pow marginalia

  :hook radian-first-input-hook
  :bind (minibuffer-local-map ("M-A" . marginalia-cycle))
  :config
  (pushnew! marginalia-command-categories
            '(flycheck-error-list-set-filter . builtin)
            '(project-find-file . project-file)
            '(project-switch-to-buffer . buffer)
            '(project-switch-project . project-file)))

(pow wgrep
  :commands wgrep-change-to-wgrep-mode
  :config (setq wgrep-auto-save-buffer t))

;;;; isearch
(-ow isearch
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

;;; MODULE {files}
;;;; Project
(-ow project
  :init

  (setq project-switch-commands
        '((project-find-file "File" ?f)
          (project-find-regexp "Grep" ?g)
          (project-dired "Dired" ?d)
          (project-switch-to-buffer "Buffer" ?b)
          (project-query-replace-regexp "Query replace" ?r)
          (project-vc-dir "VC dir" ?v)
          (project-eshell "Eshell" ?e)
          (+project/retrieve-tag "Tag switch" ?t)
          (+project/magit-status "Magit" ?m)
          (+project/commit-log "Log VC" ?l)
          (project-find-dir "Subdir" ?s)))

  :config
  (setq +project-commit-log-limit 25)

  (-keys (project-prefix-map
          ("s" . project-find-dir)
          ("l" . +project/commit-log)
          ("t" . +project/retrieve-tag)))

  (cl-defmethod project-root ((project (head local)))
    "Project root for PROJECT with HEAD and LOCAL."
    (cdr project))

  (defun +project--files-in-directory (dir)
    "Use `fd' to list files in DIR."
    (unless (executable-find "fd")
      (error "Cannot find 'fd' command is shell environment $PATH"))
    (let* ((default-directory dir)
           (localdir (file-local-name (expand-file-name dir)))
           (command (format "fd -t f -0 . %s" localdir)))
      (project--remote-file-names
       (split-string (shell-command-to-string command) "\0" t))))

  ;; Copied from Manuel Uberti:
  ;; <https://www.manueluberti.eu/emacs/2020/11/14/extending-project/>.
  (cl-defmethod project-files ((project (head local)) &optional dirs)
    "Override `project-files' to use `fd' in local projects.

Project root for PROJECT with HEAD and LOCAL, plus optional
DIRS."
    (mapcan #'+project--files-in-directory
            (or dirs (list (project-root project)))))

  (defun +project--try-local (dir)
    "Determine if DIR is a non-Git project.
DIR must include a .project file to be considered a project."
    (let ((root (locate-dominating-file dir ".project")))
      (and root (cons 'local root))))

  :defer-config
  (add-hook 'project-find-functions #'+project--try-local)

  :blackout t)

;; Feature `auth-source' reads and writes secrets from files like
;; ~/.netrc for TRAMP and related packages, so for example you can
;; avoid having to type in a particular host's password every time.
(-ow! auth-source
  :config

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

;;;; Saving files

;; Don't generate backups or lockfiles. While auto-save maintains a copy so long
;; as a buffer is unsaved, backups create copies once, when the file is first
;; written, and never again until it is killed and reopened. This is better
;; suited to version control, and I don't want world-readable copies of
;; potentially sensitive material floating around our filesystem.
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
 (lambda (c)
   (set-char-table-range auto-fill-chars c t))
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

;; Feature `outline' provides major and minor modes for collapsing
;; sections of a buffer into an outline-like format.
;;;; Outline
(-ow outline
  :config
  (setq-local outline-heading-alist
              '((";;; " . 1) (";;;; " . 2) (";;;;; " . 3)
                (";;;;;; " . 4) (";;;;;;; " . 5)))

  (define-globalized-minor-mode global-outline-minor-mode
    outline-minor-mode outline-minor-mode)

  (global-outline-minor-mode +1)

  :blackout outline-minor-mode)

;;;; hide-show
(-ow! hideshow
  :hook (prog-mode-hook . hs-minor-mode)
  :config

  ;; Let hided block show the count of line in box.
  (defconst hideshow-folded-face '((t (:inherit 'font-lock-comment-face :box t))))

  (defun hideshow-folded-overlay-fn (ov)
    (when (eq 'code (overlay-get ov 'hs))
      (let* ((nlines (count-lines (overlay-start ov) (overlay-end ov)))
             (info (format " ... #%d " nlines)))
        (overlay-put ov 'display (propertize info 'face hideshow-folded-face)))))

  (setq hs-set-up-overlay 'hideshow-folded-overlay-fn)

  :blackout hs-minor-mode)

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

;; Package `undo-fu' replaces the default Emacs undo system, which
;; is poorly designed and hard to use, with a much more powerful
;; tree-based system. In basic usage, you don't even have to think
;; about the tree, because it acts like a conventional undo/redo
;; system. Bindings are C-/, M-/, and C-x u.
(pow! undo-fu
  :hook (radian-first-buffer-hook . undo-fu-mode)
  :preface
  (pow! undo-fu-session
    :hook (undo-fu-mode-hook . global-undo-fu-session-mode)
    :config
    (setq undo-fu-session-incompatible-files '("\\.gpg$" "/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))

    ;; HACK Fix #4993: we've advised `make-backup-file-name-1' to produced SHA1'ed
    ;;      filenames to prevent file paths that are too long, so we force
    ;;      `undo-fu-session--make-file-name' to use it instead of its own
    ;;      home-grown overly-long-filename generator.
    ;; TODO PR this upstream; should be a universal issue
    (defadvice! +undo-fu-make-hashed-session-file-name-a (file)
      :override #'undo-fu-session--make-file-name
      (let ((backup-directory-alist `(("." . ,undo-fu-session-directory))))
        (concat (make-backup-file-name-1 file)
                (if undo-fu-session-compression ".gz" ".el"))))

    ;; HACK Use the faster zstd to compress undo files instead of gzip
    (when (executable-find "zstd")
      (defadvice! +undo--append-zst-extension-to-file-name-a (filename)
        :filter-return #'undo-fu-session--make-file-name
        (if undo-fu-session-compression
            (concat (file-name-sans-extension filename) ".zst")
          filename))))
  :config
  ;; Increase undo history limits to reduce likelihood of data loss
  (setq undo-limit 400000           ; 400kb (default is 160kb)
        undo-strong-limit 3000000   ; 3mb   (default is 240kb)
        undo-outer-limit 48000000)  ; 48mb  (default is 24mb)

  (define-minor-mode undo-fu-mode
    "Enables `undo-fu' for the current session."
    :keymap (let ((map (make-sparse-keymap)))
              (define-key map [remap undo] #'undo-fu-only-undo)
              (define-key map [remap redo] #'undo-fu-only-redo)
              (define-key map (kbd "C-/")     #'undo-fu-only-undo)
              (define-key map (kbd "M-/")     #'undo-fu-only-redo)
              (define-key map (kbd "C-M-/")   #'undo-fu-only-redo-all)
              (define-key map (kbd "C-x r u") #'undo-fu-session-save)
              (define-key map (kbd "C-x r U") #'undo-fu-session-recover)
              map)
    :init-value nil
    :global t))

;; Feature `bookmark' provides a way to mark places in a buffer. I
;; don't use it, but some other packages do.
(-ow bookmark
  :config

  (dolist (func '(bookmark-load bookmark-write-file))
    (advice-add func :around #'fn-quiet!)))

;; Feature `fileloop' provides the underlying machinery used to do
;; operations on multiple files, such as find-and-replace.
(-ow! fileloop
    :when (version<= "27" emacs-version)
    :config

    (defadvice! radian--advice-fileloop-find-all-matches
      (func &rest args)
      "Fix a bug in `fileloop' that causes it to miss matches.
In particular, without this advice, doing a find-and-replace in
multiple files will miss any match that occurs earlier in a
visited file than point happens to be currently in that
buffer."
      :around #'fileloop-initialize-replace
      (letf! ((defun perform-replace (&rest args)
                (apply perform-replace
                       (append args (list (point-min) (point-max))))))
        (apply func args))))

;; Package `visual-regexp' provides an alternate version of
;; `query-replace' which highlights matches and replacements as you
;; type.
(pow! visual-regexp
  :bind (([remap query-replace] . vr/query-replace)))

;; Package `visual-regexp-steroids' allows `visual-regexp' to use
;; regexp engines other than Emacs'; for example, Python or Perl
;; regexps.
(pow! visual-regexp-steroids
  :require t
  :after visual-regexp
  :bind (([remap query-replace-regexp] . radian-query-replace-literal))

  :config

  ;; Use Emacs-style regular expressions by default, instead of
  ;; Python-style.
  (setq vr/engine 'emacs)

  (defun radian-query-replace-literal ()
    "Do a literal query-replace using `visual-regexp'."
    (interactive)
    (let ((vr/engine 'emacs-plain))
      (call-interactively #'vr/query-replace))))

;; Package `imenu-list' use the side buffer to display imenu
(pow! imenu-list

  :bind ("C-c i" . imenu-list-smart-toggle)
  :config
  (setq imenu-list-focus-after-activation t)
  (setq imenu-list-auto-resize t))

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
(-ow! server
  :when (display-graphic-p)
  :aftercall radian-first-input-hook radian-first-file-hook focus-out-hook
  :init
  (when-let (name (getenv "EMACS_SERVER_NAME"))
    (setq server-name name))
  :defer-config
  (unless (server-running-p)
    (server-start)))

;;;; tramp
(after! tramp
  (setq remote-file-name-inhibit-cache 60
        tramp-verbose 1
        vc-ignore-dir-regexp (format "%s\\|%s\\|%s"
                                     vc-ignore-dir-regexp
                                     tramp-file-name-regexp
                                     "[/\\\\]node_modules")))

;;;; so-long
(-ow! so-long
  :hook (radian-first-file-hook . global-so-long-mode)
  :config
  (setq so-long-threshold 400) ; reduce false positives w/ larger threshold
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
            '(flycheck-mode
              spell-fu-mode
              eldoc-mode
              smartparens-mode
              highlight-numbers-mode
              better-jumper-local-mode
              ws-butler-mode
              auto-composition-mode
              undo-tree-mode
              highlight-indent-guides-mode
              hl-fill-column-mode))
  (defun radian-buffer-has-long-lines-p ()
    (unless (bound-and-true-p visual-line-mode)
      (let ((so-long-skip-leading-comments
             ;; HACK Fix #2183: `so-long-detected-long-line-p' tries to parse
             ;;      comment syntax, but comment state may not be initialized,
             ;;      leading to a wrong-type-argument: stringp error.
             (bound-and-true-p comment-use-syntax)))
        (so-long-detected-long-line-p))))
  (setq so-long-predicate #'radian-buffer-has-long-lines-p))

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

  :defer-config
  (defun +ligatures-prettify-symbols-h ()
    (setq prettify-symbols-alist '(("lambda" . "𝝀"))))
  (add-hook 'prog-mode-hook #'+ligatures-prettify-symbols-h)

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

;;;; ligature
(-ow! ligature
  :unless IS-WINDOWS          ; `ligature' make emacs slow on windows.
  :straight (ligature :host github :repo "mickeynp/ligature.el")

  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures
   'prog-mode
   '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
     ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
     "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
     "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
     "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
     "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
     "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
     "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
     ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
     "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
     "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
     "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
     "\\\\" "://"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

;;; MODULE {Electricity}: automatic things
;;;; Autorevert

;; On macOS, Emacs has a nice keybinding to revert the current buffer.
;; On other platforms such a binding is missing; we re-add it here.


;; Feature `autorevert' allows the use of file-watchers or polling in
;; order to detect when the file visited by a buffer has changed, and
;; optionally reverting the buffer to match the file (unless it has
;; unsaved changes).
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

;;;; Automatic delimiter pairing

(-ow paren
  ;; highlight matching delimiters
  :hook (radian-first-buffer-hook . show-paren-mode)
  :config
  (setq show-paren-delay 0.1
        show-paren-highlight-openparen t
        show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t))

;; Package `smartparens' provides an API for manipulating paired
;; delimiters of many different types, as well as interactive commands
;; and keybindings for operating on paired delimiters at the
;; s-expression level. It provides a Paredit compatibility layer.
(pow! smartparens
  :hook (radian-first-buffer-hook . smartparens-global-mode)
  :defun sp-message sp-region-ok-p
  :commands sp-pair sp-local-pair sp-with-modes sp-point-in-comment sp-point-in-string
  ;; markdown-mode lsp
  ;; radian--advice-lsp-mode-silence company-explicit-action-p company--should-begin
  ;; company--should-continue global-company-mode
  :config

  ;; Load the default pair definitions for Smartparens.
  (require 'smartparens-config)

  ;; When in Paredit emulation mode, Smartparens binds M-( to wrap the
  ;; following s-expression in round parentheses. By analogy, we
  ;; should bind M-[ to wrap the following s-expression in square
  ;; brackets. However, this breaks escape sequences in the terminal,
  ;; so it may be controversial upstream. We only enable the
  ;; keybinding in windowed mode.
  (when (display-graphic-p)
    (setf (map-elt sp-paredit-bindings "M-[") #'sp-wrap-square))

  ;; Set up keybindings for s-expression navigation and manipulation
  ;; in the style of Paredit.
  (sp-use-paredit-bindings)

  ;; Highlight matching delimiters.
  (show-smartparens-global-mode +1)

  ;; Prevent all transient highlighting of inserted pairs.
  (setq sp-highlight-pair-overlay nil)
  (setq sp-highlight-wrap-overlay nil)
  (setq sp-highlight-wrap-tag-overlay nil)

  ;; Don't disable autoskip when point moves backwards. (This lets you
  ;; open a sexp, type some things, delete some things, etc., and then
  ;; type over the closing delimiter as long as you didn't leave the
  ;; sexp entirely.)
  (setq sp-cancel-autoskip-on-backward-movement nil)

  ;; Disable Smartparens in Org-related modes, since the keybindings
  ;; conflict.

  (-ow! org
    :config

    (add-to-list 'sp-ignore-modes-list #'org-mode))

  (-ow! org-agenda
    :config

    (add-to-list 'sp-ignore-modes-list #'org-agenda-mode))

  ;; Make C-k kill the sexp following point in Lisp modes, instead of
  ;; just the current line.
  (-key [remap kill-line] #'sp-kill-hybrid-sexp 'smartparens-mode-map)

  (defun radian--smartparens-indent-new-pair (&rest _)
    "Insert an extra newline after point, and reindent."
    (newline)
    (indent-according-to-mode)
    (forward-line -1)
    (indent-according-to-mode))

  ;; The following is a really absurdly stupid hack that I can barely
  ;; stand to look at. It needs to be fixed.
  ;;
  ;; Nevertheless, I can't live without the feature it provides (which
  ;; should really come out of the box IMO): when pressing RET after
  ;; inserting a pair, add an extra newline and indent. See
  ;; <https://github.com/Fuco1/smartparens/issues/80#issuecomment-18910312>.

  (defun radian--smartparens-pair-setup (mode delim)
    "In major mode MODE, set up DELIM with newline-and-indent."
    (sp-local-pair mode delim nil :post-handlers
                   '((radian--smartparens-indent-new-pair "RET")
                     (radian--smartparens-indent-new-pair "<return>"))))

  (dolist (delim '("(" "[" "{"))
    (dolist (mode '(
                    fundamental-mode
                    javascript-mode
                    protobuf-mode
                    prog-mode
                    text-mode
                    ))
      (radian--smartparens-pair-setup mode delim)))
  (radian--smartparens-pair-setup #'python-mode "\"\"\"")
  (radian--smartparens-pair-setup #'markdown-mode "```")

  ;; Deal with `protobuf-mode' not using `define-minor-mode'.
  (radian--smartparens-pair-setup #'protobuf-mode "{")

  ;; Work around https://github.com/Fuco1/smartparens/issues/783.
  (setq sp-escape-quotes-after-insert nil)

  ;; Quiet some silly messages.
  (dolist (key '(:unmatched-expression :no-matching-tag))
    (setf (cdr (assq key sp-message-alist)) nil))

  :blackout t)

;;;; Code reformatting

;; Package `apheleia' implements a sophisticated algorithm for
;; applying code formatters asynchronously on save without moving
;; point or modifying the scroll position.
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

;;;; Snippet expansion

;; Feature `abbrev' provides functionality for expanding user-defined
;; abbreviations. We prefer to use `yasnippet' instead, though.
(-ow abbrev :blackout t)

;; Package `yasnippet' allows the expansion of user-defined
;; abbreviations into fillable templates. The only reason we have it
;; here is because it gets pulled in by LSP, and we need to unbreak
;; some stuff.
(pow! yasnippet

  :bind (yas-minor-mode-map
         ;; Disable TAB from expanding snippets, as I don't use it and
         ;; it's annoying.
         ("TAB" . nil)
         ("<tab>" . nil))
  :after company
  :config

  ;; Reduce verbosity. The default value is 3. Bumping it down to 2
  ;; eliminates a message about successful snippet lazy-loading setup
  ;; on every(!) Emacs init. Errors should still be shown.
  (setq yas-verbosity 2)

  ;; Make it so that Company's keymap overrides Yasnippet's keymap
  ;; when a snippet is active. This way, you can TAB to complete a
  ;; suggestion for the current field in a snippet, and then TAB to
  ;; move to the next field. Plus, C-g will dismiss the Company
  ;; completions menu rather than cancelling the snippet and moving
  ;; the cursor while leaving the completions menu on-screen in the
  ;; same location.
  (-ow company

    :config

    ;; This function translates the "event types" I get from
    ;; `map-keymap' into things that I can pass to `lookup-key' and
    ;; `define-key'. It's a hack, and I'd like to find a built-in
    ;; function that accomplishes the same thing while taking care of
    ;; any edge cases I might have missed in this ad-hoc solution.
    (defun radian--yasnippet-normalize-event (event)
      "This function is a complete hack, do not use.
But in principle, it translates what we get from `map-keymap'
into what `lookup-key' and `define-key' want."
      (if (vectorp event)
          event
        (vector event)))

    ;; Here we define a hybrid keymap that delegates first to
    ;; `company-active-map' and then to `yas-keymap'.
    (defvar radian--yasnippet-then-company-keymap
      ;; It starts out as a copy of `yas-keymap', and then we
      ;; merge in all of the bindings from `company-active-map'.
      (let ((keymap (copy-keymap yas-keymap)))
        (map-keymap
         (lambda (event company-cmd)
           (let* ((event (radian--yasnippet-normalize-event event))
                  (yas-cmd (lookup-key yas-keymap event)))
             ;; Here we use an extended menu item with the
             ;; `:filter' option, which allows us to dynamically
             ;; decide which command we want to run when a key is
             ;; pressed.
             (define-key keymap event
                         `(menu-item
                           nil ,company-cmd :filter
                           (lambda (cmd)
                             ;; There doesn't seem to be any obvious
                             ;; function from Company to tell whether or not
                             ;; a completion is in progress (à la
                             ;; `company-explicit-action-p'), so I just
                             ;; check whether or not `company-my-keymap' is
                             ;; defined, which seems to be good enough.
                             (if company-my-keymap
                                 ',company-cmd
                               ',yas-cmd))))))
         company-active-map)
        keymap)
      "Keymap which delegates to both `company-active-map' and `yas-keymap'.
The bindings in `company-active-map' only apply if Company is
currently active.")

    (defadvice! radian--advice-company-overrides-yasnippet
      (yas--make-control-overlay &rest args)
      "Allow `company' keybindings to override those of `yasnippet'."
      :around #'yas--make-control-overlay
      ;; The function `yas--make-control-overlay' uses the current
      ;; value of `yas-keymap' to build the Yasnippet overlay, so to
      ;; override the Yasnippet keymap we only need to dynamically
      ;; rebind `yas-keymap' for the duration of that function.
      (let ((yas-keymap radian--yasnippet-then-company-keymap))
        (apply yas--make-control-overlay args))))

  :blackout yas-minor-mode)

;;; MODULE {IDE features}
;;;; Virtual environments
;;;; xref
(-ow xref
  :custom
  (xref-search-program . 'ripgrep)
  (xref-show-xrefs-function . #'xref-show-definitions-completing-read)
  (xref-show-definitions-function . #'xref-show-definitions-completing-read))

;;;; rainbow colorize delimiter.
(pow! rainbow-delimiters

  :hook ((prog-mode-hook text-mode-hook) . rainbow-delimiters-mode)
  :init
  (-ow cc-mode :hook (c-mode-common-hook . rainbow-delimiters-mode))
  :config
  (setq rainbow-delimiters-max-face-count 3))

;;;; hl-todo
(pow! hl-todo
  :hook ((prog-mode-hook yaml-mode-hook) . hl-todo-mode)

  :custom
  (hl-todo-highlight-punctuation . ":")
  (hl-todo-keyword-faces
   .
   `(;; For things that need to be done, just not today.
     ("TODO" warning bold)
     ;; For problems that will become bigger problems later if not
     ;; fixed ASAP.
     ("FIXME" error bold)
     ;; For tidbits that are unconventional and not intended uses of the
     ;; constituent parts, and may break in a future update.
     ("HACK" font-lock-constant-face bold)
     ;; For things that were done hastily and/or hasn't been thoroughly
     ;; tested. It may not even be necessary!
     ("REVIEW" font-lock-keyword-face bold)
     ;; For especially important gotchas with a given implementation,
     ;; directed at another user other than the author.
     ("NOTE" success bold)
     ;; For things that just gotta go and will soon be gone.
     ("DEPRECATED" font-lock-doc-face bold)
     ;; For a known bug that needs a workaround
     ("BUG" error bold)
     ;; For working in progress
     ("WIP" elisp-shorthand-font-lock-face bold)
     ;; For warning about a problematic or misguiding code
     ("XXX" font-lock-constant-face bold)))

  :config
  (defadvice! +hl-todo-clamp-font-lock-fontify-region-a (fn &rest args)
    "Fix an `args-out-of-range' error in some modes."
    :around #'hl-todo-mode
    (letf! (defun font-lock-fontify-region (beg end &optional loudly)
             (funcall font-lock-fontify-region (max beg 1) end loudly))
      (apply fn args)))

  ;; Use a more primitive todo-keyword detection method in major modes that
  ;; don't use/have a valid syntax table entry for comments.
  (add-hook! '(pug-mode-hook haml-mode-hook)
    (defun +hl-todo--use-face-detection-h ()
      "Use a different, more primitive method of locating todo keywords."
      (set (make-local-variable 'hl-todo-keywords)
           '(((lambda (limit)
                (let (case-fold-search)
                  (and (re-search-forward hl-todo-regexp limit t)
                       (memq 'font-lock-comment-face (radian-enlist (get-text-property (point) 'face))))))
              (1 (hl-todo-get-face) t t))))
      (when hl-todo-mode
        (hl-todo-mode -1)
        (hl-todo-mode +1)))))

;;;; Language servers

;; Package `lsp-mode' is an Emacs client for the Language Server
;; Protocol <https://langserver.org/>. It is where we get all of our
;; information for completions, definition location, documentation,
;; and so on.
(pow! lsp-mode
  :preface

  (pow! consult-lsp :if (featurep! 'consult) :commands consult-lsp-symbols)

  (defcustom radian-lsp-disable nil
    "If non-nil, then LSP is not allowed to be enabled.
For use in file-local variables."
    :type 'boolean
    :safe #'booleanp)

  (defun radian--lsp-enable (&optional _)
    "Enable `lsp-mode' for most programming modes.
Do this on `after-change-major-mode-hook' instead of
`prog-mode-hook' and `text-mode-hook' because we want to make
sure regular mode hooks get a chance to run first, for example to
set LSP configuration (see `lsp-python-ms')."
    (when (derived-mode-p #'prog-mode #'text-mode)
      (unless (or radian-lsp-disable
                  (null buffer-file-name)
                  (derived-mode-p
                   ;; `lsp-mode' doesn't support Elisp, so let's avoid
                   ;; triggering the autoload just for checking that, yes,
                   ;; there's nothing to do for the *scratch* buffer.
                   #'emacs-lisp-mode
                   ;; Disable for modes that we currently use a specialized
                   ;; framework for, until they are phased out in favor of
                   ;; LSP.
                   #'clojure-mode
                   #'ruby-mode))
        (lsp))))
  ;; (setq xref-backend-functions (remq 'lsp--xref-backend xref-backend-functions))

  :bind (lsp-mode-map ("C-c C-a" . consult-lsp-symbols))
  :hook (after-change-major-mode-hook . radian--lsp-enable)

  :init
  (setq lsp-modeline-code-actions-enable nil
        lsp-headerline-breadcrumb-enable nil
        ;; Reduce unexpected modifications to code
        lsp-enable-on-type-formatting nil
        ;; Disable features that have great potential to be slow.
        lsp-enable-folding nil
        lsp-enable-text-document-color nil)

  :config
  ;; We want to make sure the PATH is set up correctly by now, since
  ;; otherwise we might not be able to find the LSP server binaries.
  (radian-env-setup)

  ;; As per <https://github.com/emacs-lsp/lsp-mode#performance>.
  (setq read-process-output-max (* 1024 1024))

  (defun radian--advice-lsp-mode-silence (format &rest args)
    "Silence needless diagnostic messages from `lsp-mode'.

This is a `:before-until' advice for several `lsp-mode' logging
functions."
    (or
     (string-match-p "No LSP server for %s" format)
     (string-match-p "Connected to %s" format)
     (string-match-p "Unable to calculate the languageId" format)
     (string-match-p
      "There are no language servers supporting current mode" format)
     ;; Errors we get from gopls for no good reason (I can't figure
     ;; out why). They don't impair functionality.
     (and (stringp (car args))
          (or (string-match-p "^no object for ident .+$" (car args))
              (string-match-p "^no identifier found$" (car args))))))

  (dolist (fun '(lsp-warn lsp--warn lsp--info lsp--error))
    (advice-add fun :before-until #'radian--advice-lsp-mode-silence))

  ;; If we don't disable this, we get a warning about YASnippet not
  ;; being available, even though it is. I don't use YASnippet anyway,
  ;; so don't bother with it.
  (setq lsp-enable-snippet nil)

  (defadvice! radian--lsp-run-from-node-modules (command)
    "Find LSP executables inside node_modules/.bin if present."
    :filter-return #'lsp-resolve-final-function
    (cl-block nil
      (prog1 command
        (when-let ((project-dir
                    (locate-dominating-file default-directory "node_modules"))
                   (binary
                    (radian--path-join
                     project-dir "node_modules" ".bin" (car command))))
          (when (file-executable-p binary)
            (cl-return (cons binary (cdr command))))))))

  (add-hook! 'kill-emacs-hook
    (defun radian--lsp-teardown ()
      "Ignore the LSP server getting killed.
If we don't do this, then when killing Emacs we may be prompted
with whether we want to restart the LSP server that has just been
killed (which happens during Emacs shutdown)."
      (setq lsp-restart nil)))

  ;; Looks like `lsp-mode' doesn't know about LaTeX yet.
  (add-to-list 'lsp-language-id-configuration '(latex-mode . "latex"))

  ;; Also, it has a bunch of regexps which are completely wrong.
  (setq lsp-language-id-configuration
        (mapcar
         (lambda (link)
           (if (and (stringp (car link))
                    (string-match "\\`\\.\\*\\.\\(.+\\)\\'" (car link)))
               (cons
                (format "\\.%s\\'" (match-string 1 (car link))) (cdr link))
             link))
         lsp-language-id-configuration))

  ;; Disable LSP reformatting your code as you type. We use Apheleia
  ;; for that instead.
  (setq lsp-enable-on-type-formatting nil)

  :blackout " LSP")

;;;; Indentation

;; Don't use tabs for indentation. Use only spaces. Otherwise,
;; whenever the indent level does not equal the tab width (e.g. in
;; Emacs Lisp code, the indent level is 2 and the tab width is 8),
;; *both* tabs and spaces will be used for indentation. Disgusting.
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
(-ow! company
  :straight
  (company :type git :flavor melpa :host github
           :repo "company-mode/company-mode" :files (:defaults "icons"))
  :hook (after-init-hook . global-company-mode)
  :bind (;; Remap the standard Emacs keybindings for invoking
         ;; completion to instead use Company. You might think this
         ;; could be put in the `:bind*' declaration below, but it
         ;; seems that `bind-key*' does not work with remappings.
         ([remap completion-at-point] . company-manual-begin)
         ([remap complete-symbol] . company-manual-begin)

         ;; The following are keybindings that take effect whenever
         ;; the completions menu is visible, even if the user has not
         ;; explicitly interacted with Company.

         (company-active-map

          ;; Make TAB always complete the current selection, instead of
          ;; only completing a common prefix.
          ("<tab>" . company-complete-selection)
          ("TAB" . company-complete-selection)

          ;; When was the last time you used the C-s binding for
          ;; searching candidates? It conflicts with buffer search,
          ;; anyway. Same for the scroll commands.
          ("C-s" . nil)
          ([remap scroll-down-command] . nil)
          ([remap scroll-up-command] . nil)

          ;; The following are keybindings that only take effect if the
          ;; user has explicitly interacted with Company. Note that
          ;; `:map' from above is "sticky", and applies also below: see
          ;; https://github.com/jwiegley/use-package/issues/334#issuecomment-349473819.
          ;; Make RET don't trigger a completion
          ("<return>" . nil)
          ("RET" . nil)

          ;; We then make <up> and <down> abort the completions menu
          ;; unless the user has interacted explicitly. Note that we
          ;; use `company-select-previous' instead of
          ;; `company-select-previous-or-abort'. I think the former
          ;; makes more sense since the general idea of this `company'
          ;; configuration is to decide whether or not to steal
          ;; keypresses based on whether the user has explicitly
          ;; interacted with `company', not based on the number of
          ;; candidates.
          ;;
          ;; Note that M-p and M-n work regardless of whether explicit
          ;; interaction has happened yet, and note also that M-TAB
          ;; when the completions menu is open counts as an
          ;; interaction.
          ("<up>" . company-select-previous)
          ("<down>" . company-select-next)))

  :bind* (;; The default keybinding for `completion-at-point' and
          ;; `complete-symbol' is M-TAB or equivalently C-M-i. We
          ;; already remapped those bindings to `company-manual-begin'
          ;; above. Here we make sure that they definitely invoke
          ;; `company-manual-begin' even if a minor mode binds M-TAB
          ;; directly.
          ("M-TAB" . company-manual-begin))

  :config
  ;; Make RET trigger a completion if and only if the user has
  ;; explicitly interacted with Company, instead of always
  ;; doing so.
  (-keys
   (company-active-map
    ("<return>" . (cmds! (company-explicit-action-p)
                         #'company-complete-selection #'newline-and-indent))
    ("RET" . (cmds! (company-explicit-action-p)
                    #'company-complete-selection #'newline-and-indent))))

  (customize-set-variable
   'company-quick-access-keys '("a" "o" "e" "u" "i" "d" "h" "t" "s" "."))

  ;; Make completions display twice as soon.
  (setq company-idle-delay 0.15)

  ;; Make completions display when you have only typed one character,
  ;; instead of three.
  (setq company-minimum-prefix-length 2)

  ;; Always display the entire suggestion list onscreen, placing it
  ;; above the cursor if necessary.
  (setq company-tooltip-minimum company-tooltip-limit)

  ;; Always display suggestions in the tooltip, even if there is only
  ;; one. Also, don't display metadata in the echo area. (This
  ;; conflicts with ElDoc.)
  (setq company-frontends '(company-pseudo-tooltip-frontend))

  ;; Show quick-reference numbers in the tooltip. (Select a completion
  ;; with M-1 through M-0.)
  (setq company-show-quick-access t)

  ;; Prevent non-matching input (which will dismiss the completions
  ;; menu), but only if the user interacts explicitly with Company.
  (setq company-require-match #'company-explicit-action-p)

  ;; Only search the current buffer to get suggestions for
  ;; `company-dabbrev' (a backend that creates suggestions from text
  ;; found in your buffers). This prevents Company from causing lag
  ;; once you have a lot of buffers open.
  (setq company-dabbrev-other-buffers nil)

  ;; Make the `company-dabbrev' backend fully case-sensitive, to
  ;; improve the UX when working with domain-specific words that have
  ;; particular casing.
  (setq company-dabbrev-ignore-case nil)
  (setq company-dabbrev-downcase nil)

  ;; When candidates in the autocompletion tooltip have additional
  ;; metadata, like a type signature, align that information to the
  ;; right-hand side. This usually makes it look neater.
  (setq company-tooltip-align-annotations t)

  (defvar-local radian--company-buffer-modified-counter nil
    "Last return value of `buffer-chars-modified-tick'.
Used to ensure that Company only initiates a completion when the
buffer is modified.")

  (defadvice! radian--advice-company-complete-on-change ()
    "Make Company trigger a completion when the buffer is modified.
This is in contrast to the default behavior, which is to trigger
a completion when one of a whitelisted set of commands is used.
One specific improvement this brings about is that you get
completions automatically when backspacing into a symbol."
    :override #'company--should-begin
    (let ((tick (buffer-chars-modified-tick)))
      (unless (equal tick radian--company-buffer-modified-counter)
        ;; Only trigger completion if previous counter value was
        ;; non-nil (i.e., don't trigger completion just as we're
        ;; jumping to a buffer for the first time).
        (prog1 (and radian--company-buffer-modified-counter
                    (not (and (symbolp this-command)
                              (string-match-p
                               "^\\(company-\\|undo-\\|undo$\\)"
                               (symbol-name this-command)))))
          (setq radian--company-buffer-modified-counter tick)))))

  (defadvice! radian--advice-company-update-buffer-modified-counter ()
    "Make sure `radian--company-buffer-modified-counter' is up to date.
If we don't do this on `company--should-continue' as well as
`company--should-begin', then we may end up in a situation where
autocomplete triggers when it shouldn't. Specifically suppose we
delete a char from a symbol, triggering autocompletion, then type
it back, but there is more than one candidate so the menu stays
onscreen. Without this advice, saving the buffer will cause the
menu to disappear and then come back after `company-idle-delay'."
    :after #'company--should-continue
    (setq radian--company-buffer-modified-counter
          (buffer-chars-modified-tick)))

  (add-hook! '(shell-mode-hook) (company-mode -1))

  :blackout t)

;;;; TabNine
;; (leaf company-tabnine
;;   :after company
;;   :config
;;   (setq company-show-numbers t
;;         company-tabnine-always-trigger nil)

;;   (setq company-backends (cons '(company-tabnine :separate company-capf)
;;                                company-backends)))

;;;; Definition location

;; Package `dumb-jump' provides a mechanism to jump to the definitions
;; of functions, variables, etc. in a variety of programming
;; languages. The advantage of `dumb-jump' is that it doesn't try to
;; be clever, so it "just works" instantly for dozens of languages
;; with zero configuration.
(pow! dumb-jump
  :init
  (add-hook! 'after-change-major-mode-hook
    (defun dumb-jump-enable ()
      "enable `dumb-jump'"
      (add-hook 'xref-backend-functions #'dumb-jump-xref-activate nil t)))
  :bind* (("C-M-d" . xref-find-references)))

;;;; Display contextual metadata

;; Feature `eldoc' provides a minor mode (enabled by default in Emacs
;; 25) which allows function signatures or other metadata to be
;; displayed in the echo area.
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

;;;; Syntax checking and code linting

;; Package `flycheck' provides a framework for in-buffer error and
;; warning highlighting. We kind of don't use it because we use
;; `lsp-ui' instead, but internally `lsp-ui' actually hacks Flycheck
;; to behave differently, so it is a dependency. We just don't enable
;; Flycheck anywhere else and rely on `lsp-ui' to handle things when
;; appropriate. However, interestingly, Flycheck is not marked as a
;; dependency of `lsp-ui', hence this declaration.
(pow! flycheck
  :config

  ;; For use with `lsp-ui'.
  (radian-bind-key "p" #'flycheck-previous-error)
  (radian-bind-key "n" #'flycheck-next-error)

  :blackout t)

;; Package `lsp-ui' provides a pretty UI for showing diagnostic
;; messages from LSP in the buffer using overlays. It's configured
;; automatically by `lsp-mode'.
(pow! lsp-ui
  :bind (lsp-ui-mode-map ("C-c f" . lsp-ui-sideline-apply-code-actions))
  :config

  (setq lsp-ui-peek-enable (featurep! '+peek)
        lsp-ui-doc-max-height 8
        lsp-ui-doc-max-width 35
        ;; lsp-ui-doc-show-with-mouse nil  ; don't disappear on mouseover

        ;; Don't show symbol definitions in the sideline. They are pretty noisy,
        ;; and there is a bug preventing Flycheck errors from being shown (the
        ;; errors flash briefly and then disappear).
        lsp-ui-sideline-show-hover nil
        ;; Re-enable icon scaling (it's disabled by default upstream for Emacs
        ;; 26.x compatibility; see emacs-lsp/lsp-ui#573)
        lsp-ui-sideline-actions-icon lsp-ui-sideline-actions-icon-default)

  (defadvice! radian--advice-lsp-ui-apply-single-fix
    (orig-fun &rest args)
    "Apply code fix immediately if only one is possible."
    :around #'lsp-ui-sideline-apply-code-actions
    (letf! ((defun completing-read (prompt collection &rest args)
              (if (= (safe-length collection) 1)
                  (car collection)
                (apply completing-read prompt collection args))))
      (apply orig-fun args)))

  (-ow lsp-ui-imenu
    :bind (lsp-ui-imenu-mode-map ("n" . next-line) ("p" . previous-line)))

  ;; Feature `lsp-ui-doc' from package `lsp-ui' displays documentation
  ;; in a child frame when point is on a symbol.
  (-ow lsp-ui-doc
    :config
    ;; https://github.com/emacs-lsp/lsp-ui/issues/414
    (add-to-list 'lsp-ui-doc-frame-parameters '(no-accept-focus . t))
    (defadvice! radian--advice-lsp-ui-doc-allow-multiline (func &rest args)
      "Prevent `lsp-ui-doc' from removing newlines from documentation."
      :around #'lsp-ui-doc--render-buffer
      (letf! ((defun replace-regexp-in-string
                  (regexp rep string &rest args)
                (if (equal regexp "`\\([\n]+\\)")
                    string
                  (apply replace-regexp-in-string
                         regexp rep string args))))
        (apply func args))))

  ;; With `lsp-ui', there's no need for the ElDoc integration
  ;; provided by `lsp-mode', and in fact for Bash it is very
  ;; annoying since all the hover information is multiline.
  (eval-after-load 'lsp-mode '(setq lsp-eldoc-enable-hover nil)))

;;; MODULE {Language support}
;;;; Common Lisp

;; Feature `lisp-mode' provides a base major mode for Lisp languages,
;; and supporting functions for dealing with Lisp code.
(-ow lisp-mode
  :bind
  ([remap eval-expression] . pp-eval-expression)
  :init
  (add-to-list 'safe-local-variable-values
               '(lisp-indent-function . common-lisp-indent-function)))

;; common-lisp
(pow! sly
  :preface
  (defer-feature! lisp-mode)
  (add-hook 'lisp-mode-hook #'rainbow-delimiters-mode)
  (defvar inferior-lisp-program "ros -Q run")

  :hook (lisp-mode-local-vars-hook . sly-editing-mode)
  :init
  ;; I moved this hook to `lisp-mode-local-vars-hook', so it only affects
  ;; `lisp-mode', and not every other derived lisp mode (like `fennel-mode').
  ;; We run it twice because the hook is both autoloaded and evaluated at
  ;; load-time, so it must be removed twice.
  (after! (:or emacs sly) (remove-hook 'lisp-mode-hook #'sly-editing-mode))

  ;; HACK Ensures that sly's contrib modules are loaded as soon as possible, but
  ;;      also as late as possible, so users have an opportunity to override
  ;;      `sly-contrib' in an `after!' block.
  (add-hook! 'after-init-hook (after! sly (sly-setup)))

  :config

  (setq sly-kill-without-query-p t
        sly-net-coding-system 'utf-8-unix
        ;; Doom defaults to non-fuzzy search, because it is faster and more
        ;; precise (but requires more keystrokes). Change this to
        ;; `sly-flex-completions' for fuzzy completion
        sly-complete-symbol-function 'sly-simple-completions)

  (defun +common-lisp--cleanup-sly-maybe-h ()
    "Kill processes and leftover buffers when killing the last sly buffer."
    (unless (cl-loop for buf in (delq (current-buffer) (buffer-list))
                     if (and (buffer-local-value 'sly-mode buf)
                             (get-buffer-window buf))
                     return t)
      (dolist (conn (sly--purge-connections))
        (sly-quit-lisp-internal conn 'sly-quit-sentinel t))
      (let (kill-buffer-hook kill-buffer-query-functions)
        (mapc #'kill-buffer
              (cl-loop for buf in (delq (current-buffer) (buffer-list))
                       if (buffer-local-value 'sly-mode buf)
                       collect buf)))))

  (add-hook! 'sly-mode-hook
    (defun +common-lisp-init-sly-h ()
      "Attempt to auto-start sly when opening a lisp buffer."
      (cond ((or (equal (substring (buffer-name (current-buffer)) 0 1) " ")
                 (sly-connected-p)))
            ((executable-find (car (split-string inferior-lisp-program)))
             (let ((sly-auto-start 'always))
               (sly-auto-start)
               (add-hook 'kill-buffer-hook #'+common-lisp--cleanup-sly-maybe-h nil t)))
            ((message "WARNING: Couldn't find `inferior-lisp-program' (%s)"
                      inferior-lisp-program)))))

  ;; * SLY KINDINGS
  ;; NOTE `leaf-keys' when binding keymap, :package convert to
  ;;`require'. when binding command, :package convert to `eval-after-load'.

  ;; Clear this keymap, for rebinding prefer keys.
  (defvar sly-prefix-map (make-sparse-keymap))
  (-keys
   (sly-prefix-map
    ("d"  . sly-documentation-lookup)
    ("'"  . sly)
    ("m"  . macrostep-expand)
    ;;sly-compile-c-keymap
    ("cc" . sly-compile-file)
    ("cC" . sly-compile-and-load-file)
    ("cf" . sly-compile-defun)
    ("cl" . sly-load-file)
    ("cn" . sly-remove-notes)
    ("cr" . sly-compile-region)
    ;;sly-elvaluate-e-keymap
    ("eb" . sly-eval-buffer)
    ("ee" . sly-eval-last-expression)
    ("eE" . sly-eval-print-last-expression)
    ("ef" . sly-eval-defun)
    ("eF" . sly-undefine-function)
    ("er" . sly-eval-region)
    ;;sly-goto-g-key-map
    ("gb" . sly-pop-find-definition-stack)
    ("gd" . sly-edit-definition)
    ("gD" . sly-edit-definition-other-window)
    ("gn" . sly-net-note)
    ("gN" . sly-previous-note)
    ("gs" . sly-stickers-next-sticker)
    ("gS" . sly-stickers-prev-sticker)
    ;;sly-help-h-key-map
    ("h<" . sly-who-calls)
    ("h>" . sly-calls-who)
    ("h~" . hyperspec-lookup-format)
    ("h#" . hyperspec-lookup-reader-macro)
    ("ha" . sly-apropos)
    ("hb" . sly-who-binds)
    ("hd" . sly-disassemble-symbol)
    ("hh" . sly-describe-symbol)
    ("hH" . sly-hyperspec-lookup)
    ("hm" . sly-who-macroexpands)
    ("hp" . sly-apropos-package)
    ("hr" . sly-who-references)
    ("hs" . sly-who-specializes)
    ("hS" . sly-who-sets)
    ;;sly-repl-r-key-map
    ("rc" . sly-mrepl-clear-repl)
    ("rq" . sly-quit-lisp)
    ("rr" . sly-restart-inferior-lisp)
    ("rs" . sly-mrepl-sync)
    ;;sly-stickers-s-key-map
    ("sb" . sly-stickers-toggle-break-on-stickers)
    ("sc" . sly-stickers-clear-defun-stickers)
    ("sC" . sly-stickers-clear-buffer-stickers)
    ("sf" . sly-stickers-fetch)
    ("sr" . sly-stickers-replay)
    ("ss" . sly-stickers-dwim)
    ;;sly-trace-t-key-map
    ("tt" . sly-toggle-trace-fdefinition)
    ("tT" . sly-toggle-fancy-trace)
    ("tu" . sly-untrace-all))))

(pow! sly-repl-ansi-color :init (add-to-list 'sly-contribs 'sly-repl-ansi-color))
(pow! sly-macrostep :commands macrostep-expand)

;;;; Dart
(pow! dart-mode
  :when (featurep! 'lsp-mode)
  :config
  (set-ligatures! '(dart-mode)
    ;; Functional
    :def "Function"
    :lambda "() =>"
    ;; Types
    :null "null"
    :true "true" :false "false"
    :int "int" :float "double"
    :str "String"
    :bool "bool"
    :list "List"
    ;; Flow
    :not "!"
    :in "in"
    :and "&&" :or "||"
    :for "for"
    :return "return"
    ;; Other
    :yield "yield"))

(pow! lsp-dart
  :straight treemacs
  :when (featurep! 'lsp-mode)
  :config
  (if IS-WINDOWS
      (setq lsp-dart-sdk-dir "C:\\ProgramData\\scoop\\apps\\flutter\\current\\bin\\cache\\dart-sdk"
            lsp-dart-flutter-sdk-dir  "C:\\ProgramData\\scoop\\apps\\flutter\\current")))
(pow! flutter :bind (dart-mode-map ("r" . flutter-run-or-hot-reload)))
(pow! hover :when (featurep! 'flutter)) ;; (set-popup-rule! "\\*Hover\\*" :quit nil)

;;;; AppleScript
;; https://developer.apple.com/library/content/documentation/AppleScript/Conceptual/AppleScriptLangGuide/introduction/ASLR_intro.html
(pow! apples-mode :mode "\\.\\(applescri\\|sc\\)pt\\'")

;;;; C, C++, Objective-C, Java
;; https://en.wikipedia.org/wiki/C_(programming_language)
;; https://en.wikipedia.org/wiki/C%2B%2B
;; https://en.wikipedia.org/wiki/Objective-C
;; https://en.wikipedia.org/wiki/Java_(programming_language)

;; Feature `cc-mode' provides major modes for C, C++, Objective-C, and
;; Java.
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

;;;; Clojure
;; https://clojure.org/
;; Package `clojure-mode' provides a major mode for Clojure.
(pow! clojure-mode)

;;;; Go
;; https://golang.org/

;; Package `go-mode' provides a major mode for Go.
(pow! go-mode

  :defer-config

  (defvar radian--go-defun-regexp
    "^\\(const\\|func\\|import\\|interface\\|package\\|type\\|var\\)"
    "Regexp matching top-level declarations in Go.")

  (defun radian--go-beginning-of-defun (&optional arg)
    "Move to beginning of current or previous top-level declaration."
    (cond
     ((null arg)
      (cl-block nil
        (while t
          (re-search-backward radian--go-defun-regexp nil 'noerror)
          (when (or (bobp)
                    (eq (get-text-property (point) 'face)
                        'font-lock-keyword-face))
            (cl-return)))))
     ((> arg 0)
      (dotimes (_ arg)
        (radian--go-beginning-of-defun)))
     ((< arg 0)
      ;; Yuck -- but we need to implement this, otherwise
      ;; `end-of-defun' just does the wrong thing :/
      (dotimes (_ (- arg))
        (radian--go-beginning-of-defun)
        (radian--go-end-of-defun)
        (radian--go-end-of-defun))
      (radian--go-beginning-of-defun))))

  (defun radian--go-end-of-defun ()
    "Move to end of current or previous top-level declaration.
Only works if `radian--go-beginning-of-defun' was just called
previously."
    (dotimes (_ 2)
      (cl-block nil
        (while t
          (re-search-forward radian--go-defun-regexp nil 'noerror)
          (when (or (eobp)
                    (save-excursion
                      (beginning-of-line)
                      (eq (get-text-property (point) 'face)
                          'font-lock-keyword-face)))
            (cl-return)))))
    (beginning-of-line)
    (go--backward-irrelevant 'stop-at-string)
    (forward-line))

  (add-hook! 'go-mode-hook
    (defun radian--go-defun-setup ()
      "Set up \\[beginning-of-defun] and \\[end-of-defun] correctly.
See <https://github.com/dominikh/go-mode.el/issues/232>."
      (setq-local beginning-of-defun-function #'radian--go-beginning-of-defun)
      (setq-local end-of-defun-function #'radian--go-end-of-defun)))

  (-ow! lsp-ui

    :config

    (defadvice! radian--advice-lsp-ui-organize-imports-more-cleanly
      (func actions &rest args)
      "Clean up the \"Organize Imports\" code actions for Go.
Firstly, don't display \"Organize Imports\" or \"Organize All
Imports\" in the sideline, as gopls sometimes reports these code
actions when the indentation is wrong (rather than when imports
need to be changed). Secondly, filter out \"Organize All
Imports\" internally, so that applying a code action will default
to \"Organize Imports\" instead of prompting you to decide
between that and \"Organize All Imports\" (which does the same
thing as far as I can tell)."
      :around #'lsp-ui-sideline--code-actions
      (let ((actions-to-keep nil)
            (actions-to-render nil))
        (dolist (action actions)
          (unless (equal "Organize All Imports" (gethash "title" action))
            (push action actions-to-keep)
            (unless (equal "Organize Imports" (gethash "title" action))
              (push action actions-to-render))))
        (setq actions-to-keep (nreverse actions-to-keep))
        (setq actions-to-render (nreverse actions-to-render))
        (when actions-to-render
          (apply func actions-to-render args))
        (setq lsp-ui-sideline--code-actions actions-to-keep)))))

;;;; Haskell
;; https://www.haskell.org/

;; Package `haskell-mode' provides a major mode and REPL integration
;; for Haskell.
(pow! haskell-mode
  :config

  ;; Enable REPL integration.
  (add-hook 'haskell-mode-hook #'interactive-haskell-mode)

  (defadvice! radian--advice-haskell-fix-back-to-indentation
    (back-to-indentation)
    "Fix `back-to-indentation' in `literate-haskell-mode'.
Otherwise, it just moves point to column 0, which is wrong.

This works around an upstream bug; see
<https://github.com/haskell/haskell-mode/issues/1594>."
    :around #'back-to-indentation
    (if (derived-mode-p 'literate-haskell-mode)
        (progn
          (beginning-of-line 1)
          (when-let ((c (char-after)))
            (when (= c ? )
              (forward-char)))
          (skip-syntax-forward " " (line-end-position))
          (backward-prefix-chars))
      (funcall back-to-indentation)))

  ;; Feature `haskell' from package `haskell-mode' is a meta-feature
  ;; which includes many other features from the package, and also for
  ;; some reason is where `interactive-haskell-mode' is defined.
  (-ow haskell

    :defer-config

    ;; Prevent this binding from overriding the alternative binding from
    ;; LSP that we actually want to use.
    (define-key interactive-haskell-mode-map "\M-." nil)

    :blackout interactive-haskell-mode)

  ;; Feature `haskell-customize' from package `haskell-mode' defines the
  ;; user options for the package.
  (-ow haskell-customize
    :config

    ;; Disable in-buffer underlining of errors and warnings, since we
    ;; already have them from `lsp-ui'.
    (setq haskell-process-show-overlays nil))

  ;; Package `lsp-haskell' configures the HIE Haskell language server
  ;; for use with `lsp-mode'.
  (pow! lsp-haskell
    :after (:all lsp-mode haskell-mode)))

;;;; Lua
;; <http://www.lua.org/>

;; Package `lua-mode' provides a major mode for Lua code.
(pow! lua-mode)

;;;; Makefile

;; Feature `make-mode' provides major modes for editing Makefiles.
(-ow! make-mode
  :blackout ((makefile-automake-mode . "Makefile")
             (makefile-gmake-mode . "Makefile")
             (makefile-makepp-mode . "Makefile")
             (makefile-bsdmake-mode . "Makefile")
             (makefile-imake-mode . "Makefile")))

;;;; Markdown
;; https://daringfireball.net/projects/markdown/

;; Package `markdown-mode' provides a major mode for Markdown.
(pow! markdown-mode

  :mode (;; Extension used by Hugo.
         ("\\.mmark\\'" . markdown-mode))

  :bind (;; C-c C-s p is a really dumb binding, we prefer C-c C-s C-p.
         ;; Same for C-c C-s q.
         (markdown-mode-style-map
          ("C-p" . markdown-insert-pre)
          ("C-q" . markdown-insert-blockquote))
         (markdown-mode-map
          ("TAB" . radian-markdown-tab)
          ;; Try to override all the bindings in
          ;; `markdown-mode-map'...
          ("<S-iso-lefttab>" . radian-markdown-shifttab)
          ("<S-tab>" . radian-markdown-shifttab)
          ("<backtab>" . radian-markdown-shifttab)))
  :defer-config

  (defun radian-markdown-tab ()
    "Do something reasonable when the user presses TAB.
This means moving forward a table cell, indenting a list item, or
performing normal indentation."
    (interactive)
    (cond
     ((markdown-table-at-point-p)
      (markdown-table-forward-cell))
     ((markdown-list-item-at-point-p)
      (markdown-demote-list-item))
     (t
      ;; Ew. But `markdown-indent-line' checks to see if
      ;; `this-command' is `markdown-cycle' before doing something
      ;; useful, so we have to.
      (let ((this-command 'markdown-cycle))
        (indent-for-tab-command)))))

  (defun radian-markdown-shifttab ()
    "Do something reasonable when the user presses S-TAB.
This means moving backward a table cell or unindenting a list
item."
    (interactive)
    (cond
     ((markdown-table-at-point-p)
      (markdown-table-backward-cell))
     ((markdown-list-item-at-point-p)
      (markdown-promote-list-item))))

  (defadvice! radian--disable-markdown-metadata-fontification (&rest _)
    "Prevent fontification of YAML metadata blocks in `markdown-mode'.
This prevents a mis-feature wherein if the first line of a
Markdown document has a colon in it, then it's distractingly and
usually wrongly fontified as a metadata block. See
https://github.com/jrblevin/markdown-mode/issues/328."
    :override #'markdown-match-generic-metadata
    (prog1 nil (goto-char (point-max)))))

;;;; Protobuf

;; Package `protobuf-mode' provides a major mode for Protobuf.
(pow! protobuf-mode)

;;;; Python
;; https://www.python.org/

;; Feature `python' provides a major mode for Python.
(-ow! python
  :config

  ;; The only consistent style.
  (setq python-fill-docstring-style 'django)

  (add-hook! 'python-mode-hook
    (defun radian--python-fix-outline-mode-config ()
      "Prevent `python-mode' from overriding `outline-minor-mode' config.
If this hook is not used, then `python-mode' will override even a
file-local setting of e.g. `outline-regexp' with its own setting."
      (kill-local-variable 'outline-regexp)
      (kill-local-variable 'outline-level)
      (kill-local-variable 'outline-heading-end-regexp)))

  (add-hook! 'python-mode-hook
    (defun radian--python-no-reindent-on-colon ()
      "Don't reindent on typing a colon.
See https://emacs.stackexchange.com/a/3338/12534."
      (setq electric-indent-chars (delq ?: electric-indent-chars))))

  ;; Default to Python 3. Prefer the versioned Python binaries since
  ;; some systems stupidly make the unversioned one point at Python 2.
  (cond
   ((executable-find "python3")
    (setq python-shell-interpreter "python3"))
   ((executable-find "python2")
    (setq python-shell-interpreter "python2"))
   (t
    (setq python-shell-interpreter "python")))

  (add-hook! 'python-mode-hook
    (defun radian--python-use-correct-executable ()
      "Use correct executables for Python tooling."
      (save-excursion
        (save-match-data
          (when (or (looking-at "#!/usr/bin/env \\(python[^ \n]+\\)")
                    (looking-at "#!\\([^ \n]+/python[^ \n]+\\)"))
            (setq-local
             python-shell-interpreter
             (substring-no-properties (match-string 1))))))
      (with-no-warnings
        (setq-local
         lsp-python-ms-python-executable-cmd
         python-shell-interpreter))))

  ;; I honestly don't understand why people like their packages to
  ;; spew so many messages.
  (setq python-indent-guess-indent-offset-verbose nil)

  (defun radian--python-find-virtualenv ()
    "Find a virtualenv corresponding to the current buffer.
Return either a string or nil."
    (cl-block nil
      (when (and (executable-find "poetry")
                 (locate-dominating-file default-directory "pyproject.toml"))
        (with-temp-buffer
          ;; May create virtualenv, but whatever.
          (when (= 0 (call-process
                      "poetry" nil '(t nil) nil "run" "which" "python"))
            (goto-char (point-min))
            (when (looking-at "\\(.+\\)/bin/python\n")
              (let ((venv (match-string 1)))
                (when (file-directory-p venv)
                  (cl-return venv)))))))
      (when (and (executable-find "pipenv")
                 (locate-dominating-file default-directory "Pipfile"))
        (with-temp-buffer
          ;; May create virtualenv, but whatever.
          (when (= 0 (call-process "pipenv" nil '(t nil) nil "--venv"))
            (goto-char (point-min))
            (let ((venv (string-trim (buffer-string))))
              (when (file-directory-p venv)
                (cl-return venv)))))))))
;; lsp for python
(pow! lsp-pyright
  :after python
  :require t)

;;;; Shell
;; http://pubs.opengroup.org/onlinepubs/9699919799/utilities/sh.html
;; https://www.gnu.org/software/bash/
;; http://www.zsh.org/

(-ow! sh-script

  :defer-config

  (dolist (func '(sh-set-shell sh-make-vars-local))
    (advice-add func :around #'fn-quiet!))

  (add-hook! 'sh-mode-hook
    (defun radian--sh-prettify-mode-line ()
      "Instead of \"Shell[bash]\", display mode name as \"Bash\"."
      ;; Only do this for `sh-mode', not derived modes such as
      ;; `pkgbuild-mode'.
      (setq mode-line-process nil)
      (when (eq major-mode 'sh-mode)
        (setq mode-name (capitalize (symbol-name sh-shell)))))))

;;;; Web
;; https://developer.mozilla.org/en-US/docs/web/HTML
;; https://developer.mozilla.org/en-US/docs/Web/CSS
;; https://developer.mozilla.org/en-US/docs/Web/JavaScript

;; Feature `js' provides a major mode `js-mode' for JavaScript. We
;; don't use it (because `web-mode' is better), but we still configure
;; some of its variables because `json-mode' uses them.
(-ow! js
  :config

  ;; Default is 4, and nobody should indent JSON with four spaces.
  (setq js-indent-level 2))

;; Package `web-mode' provides a major mode for HTML, CSS, JavaScript,
;; and every conceivable thing adjacent (TypeScript, JSX, TSX, PSP,
;; ASP, Handlebars, etc.) all at once.
(pow! web-mode

  ;; Unfortunately `web-mode' does not come with `auto-mode-alist'
  ;; autoloads. We have to establish them manually. This list comes
  ;; from the official website at <http://web-mode.org/> as of
  ;; 2018-07-09.
  :mode (("\\.phtml\\'" . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.[agj]sp\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode)
         ("\\.html?\\'" . web-mode)
         ;; My additions.
         ("\\.ejs\\'" . web-mode)
         ("\\.[cm]?jsx?\\'" . web-mode)
         ("\\.tsx?\\'" . web-mode)
         ("\\.css\\'" . web-mode)
         ("\\.hbs\\'" . web-mode))
  ;; Use `web-mode' rather than `js-mode' for scripts.
  :interpreter (("js" . web-mode) ("node" . web-mode))
  :defer-config

  ;; Indent by two spaces by default. Compatibility with Prettier.
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)

  ;; Not sure why anyone would want 1 space indent for inline scripts
  ;; and CSS. Set it to 2 for consistency.
  (setq web-mode-script-padding 2)
  (setq web-mode-style-padding 2)

  ;; Autocomplete </ instantly.
  (setq web-mode-enable-auto-closing t)

  ;; Insert matching tags automatically. Why this is "mode 2", I have
  ;; not the slightest idea.
  (setq web-mode-auto-close-style 2)

  ;; Don't insert quotes automatically. It messes with JSX.
  (setq web-mode-enable-auto-quoting nil)

  ;; Disable `web-mode' automatically reindenting a bunch of
  ;; surrounding code when you paste anything. It's real annoying if
  ;; it happens to not know how to indent your code correctly.
  (setq web-mode-enable-auto-indentation nil)

  ;; When using `web-mode' to edit JavaScript files, support JSX tags.
  (add-to-list 'web-mode-content-types-alist
               '("jsx" . "\\.[cm]?js[x]?\\'"))

  ;; Create line comments instead of block comments by default in
  ;; JavaScript. See <https://github.com/fxbois/web-mode/issues/619>.
  (let ((types '("javascript" "jsx")))
    (setq web-mode-comment-formats
          (cl-remove-if (lambda (item)
                          (member (car item) types))
                        web-mode-comment-formats))
    (dolist (type types)
      (push (cons type "//") web-mode-comment-formats)))

  (add-hook! 'web-mode-hook
    (defun radian--web-js-fix-comments ()
      "Fix comment handling in `web-mode' for JavaScript.
Note that this somewhat breaks HTML comments, but it's good
enough for the moment."

      ;; For some reason the default is to insert HTML comments even
      ;; in JavaScript.
      (setq-local comment-start "//")
      (setq-local comment-end "")

      ;; Needed since otherwise the default value generated by
      ;; `comment-normalize-vars' will key off the syntax and think
      ;; that a single "/" starts a comment, which completely borks
      ;; auto-fill.
      (setq-local comment-start-skip "// *")))

  (-ow apheleia
    :config

    (add-hook! 'apheleia-post-format-hook
      (defun radian--web-highlight-after-formatting ()
        "Make sure syntax highlighting works with Apheleia.
The problem is that `web-mode' doesn't do highlighting correctly
in the face of arbitrary buffer modifications, and kind of hacks
around the problem by hardcoding a special case for yanking based
on the value of `this-command'. So, when buffer modifications
happen in an unexpected (to `web-mode') way, we have to manually
poke it. Otherwise the modified text remains unfontified."
        (when (and web-mode-scan-beg web-mode-scan-end global-font-lock-mode)
          (save-excursion
            (font-lock-fontify-region web-mode-scan-beg web-mode-scan-end)))))))

;;; Configuration file formats

;; Package `apache-mode' provides a major mode for .htaccess and
;; similar files.
(pow! apache-mode)

;; Package `crontab-mode' provides a major mode for crontab files.
(pow! crontab-mode)

;; Package `dockerfile-mode' provides a major mode for Dockerfiles.
(pow! dockerfile-mode)

;; This package include `gitconfig-mode' `gitignore-mode' `gitattributes-mode'
(pow! git-modes :init (-ow gitconfig-mode :mode "\\.gitconfig.*"))

;; Package `json-mode' provides a major mode for JSON.
(pow! json-mode
  :init/el-patch

  (defconst json-mode-standard-file-ext '(".json" ".jsonld")
    "List of JSON file extensions.")

  (defsubst json-mode--update-auto-mode (filenames)
    "Update the `json-mode' entry of `auto-mode-alist'.

FILENAMES should be a list of file as string.
Return the new `auto-mode-alist' entry"
    (let* ((new-regexp
            (rx-to-string
             `(seq (eval
                    (cons 'or
                          (append json-mode-standard-file-ext
                                  ',filenames)))
                   eot)))
           (new-entry (cons new-regexp 'json-mode))
           (old-entry (when (boundp 'json-mode--auto-mode-entry)
                        json-mode--auto-mode-entry)))
      (setq auto-mode-alist (delete old-entry auto-mode-alist))
      (add-to-list 'auto-mode-alist new-entry)
      new-entry))

  (defcustom json-mode-auto-mode-list '(".babelrc" ".bowerrc" "composer.lock")
    "List of filename as string to pass for the JSON entry of
`auto-mode-alist'.

Note however that custom `json-mode' entries in `auto-mode-alist'
won’t be affected."
    :group 'json-mode
    :type '(repeat string)
    :set (lambda (symbol value)
           "Update SYMBOL with a new regexp made from VALUE.

This function calls `json-mode--update-auto-mode' to change the
`json-mode--auto-mode-entry' entry in `auto-mode-alist'."
           (set-default symbol value)
           (setq json-mode--auto-mode-entry
                 (json-mode--update-auto-mode value))))

  (defvar json-mode--auto-mode-entry
    (json-mode--update-auto-mode json-mode-auto-mode-list)
    "Regexp generated from the `json-mode-auto-mode-list'.")

  :config

  (add-hook! 'json-mode-hook
    (defun radian--fix-json-indentation ()
      "Set the tab width to 2 for JSON."
      (setq-local tab-width 2))))

;; Package `pip-requirements' provides a major mode for
;; requirements.txt files used by Pip.
(pow! pip-requirements

  ;; The default mode lighter is "pip-require". Ew.
  :blackout "Requirements")

;; Package `pkgbuild-mode' provides a major mode for PKGBUILD files
;; used by Arch Linux and derivatives.
(pow! pkgbuild-mode)

;; Package `ssh-config-mode' provides major modes for files in ~/.ssh.
(pow! ssh-config-mode
  :blackout "SSH-Config")

;; Package `terraform-mode' provides major modes for Terraform
;; configuration files.
(pow! terraform-mode)

;; Package `toml-mode' provides a major mode for TOML.
(pow! toml-mode
  :mode "Pipfile\\'"
  ;; Correct the capitalization from "Toml" to "TOML".
  :blackout "TOML")

;; Package `yaml-mode' provides a major mode for YAML.
(pow! yaml-mode)

;; Extra file extensions to support
(nconc auto-mode-alist
       '(("/LICENSE\\'" . text-mode)
         ("\\.log\\'" . text-mode)
         ("rc\\'" . conf-mode)
         ("\\.\\(?:hex\\|nes\\)\\'" . hexl-mode)))

;;; Introspection
;;;; Help

;; Feature `help' powers the *Help* buffer and related functionality.
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

;; Package `helpful' provides a complete replacement for the built-in
;; Emacs help facility which provides much more contextual information
;; in a better format.
(pow! helpful

  :commands helpful--read-symbol
  :bind (;; Remap standard commands.
         ([remap describe-function] . helpful-callable)
         ([remap describe-command]  . helpful-command)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-symbol]   . helpful-symbol)
         ([remap describe-key]      . helpful-key)

         ;; Suggested bindings from the documentation at
         ;; https://github.com/Wilfred/helpful.
         ("C-c C-d" . helpful-at-point)

         (help-map
          ("F"   . helpful-function)
          ("M-f" . helpful-macro)
          ("C"   . helpful-command)))

  :init
  (defun radian-use-helpful-a (fn &rest args)
    "Force FN to use helpful instead of the old describe-* commands."
    (letf! ((#'describe-function #'helpful-function)
            (#'describe-variable #'helpful-variable))
      (apply fn args)))

  ;; Note that this function is actually defined in `elisp-mode'
  ;; because screw modularity.
  (defadvice! radian--advice-company-elisp-use-helpful
    (func &rest args)
    "Cause `company' to use Helpful to show Elisp documentation."
    :around #'elisp--company-doc-buffer
    (cl-letf (((symbol-function #'describe-function) #'helpful-function)
              ((symbol-function #'describe-variable) #'helpful-variable)
              ((symbol-function #'help-buffer) #'current-buffer))
      (apply func args)))

  (after! apropos
    ;; patch apropos buttons to call helpful instead of help
    (dolist (fun-bt '(apropos-function apropos-macro apropos-command))
      (button-type-put
       fun-bt 'action
       (lambda (button)
         (helpful-callable (button-get button 'apropos-symbol)))))
    (dolist (var-bt '(apropos-variable apropos-user-option))
      (button-type-put
       var-bt 'action
       (lambda (button)
         (helpful-variable (button-get button 'apropos-symbol))))))

  :config

  ;; Make it so you can quit out of `helpful-key' with C-g, like for
  ;; every other command. Put this in a minor mode so it can be
  ;; disabled.
  (define-minor-mode radian-universal-keyboard-quit-mode
    "Minor mode for making C-g work in `helpful-key'."
    :global t
    (if radian-universal-keyboard-quit-mode
        (defadvice! radian--advice-helpful-key-allow-keyboard-quit
          (&rest _)
          "Make C-g work in `helpful-key'."
          :before #'helpful-key
          ;; The docstring of `add-function' says that if we make our
          ;; advice interactive and the interactive spec is *not* a
          ;; function, then it overrides the original function's
          ;; interactive spec.
          (interactive
           (list
            (let ((ret (read-key-sequence "Press key: ")))
              (when (equal ret "\^G")
                (signal 'quit nil))
              ret))))
      (advice-remove
       #'helpful-key #'radian--advice-helpful-key-allow-keyboard-quit)))

  (radian-universal-keyboard-quit-mode +1)

  (defadvice! radian--advice-helpful-clone-emacs-source (library-name)
    "Prompt user to clone Emacs source code when looking up functions.
Otherwise, it only happens when looking up variables, for some
bizarre reason."
    :before #'helpful--library-path
    (when (member (file-name-extension library-name) '("c" "rs"))
      (radian-clone-emacs-source-maybe))))

;;;; Custom

;; Feature `cus-edit' powers Customize buffers and related
;; functionality.
(-ow! cus-edit
  :config

  ;; Don't show the search box in Custom.
  (setq custom-search-field nil))

;;;; Emacs Lisp development

;; Feature `elisp-mode' provides the major mode for Emacs Lisp. Very
;; important! It also provides the major mode for the *scratch*
;; buffer, which is very similar but slightly different. Not as
;; important.
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

(defun radian-reload-init ()
  "Reload the init-file."
  (interactive)
  (run-hooks radian-before-reload-hook)
  (message "Reloading init-file...")
  ;; Total hack here. I have no idea why it's needed. But, probably
  ;; due to some kind of disgusting Gilardi scenario, if we don't
  ;; explicitly load it here, the autoloading does not quite suffice
  ;; to make everything work out. Specifically, if we byte-compile the
  ;; init-file, start up using that init-file, then make a
  ;; modification to the init-file and reload using
  ;; `radian-reload-init', then all the `leaf' declarations
  ;; fail to recognize `:straight' as a supported keyword, strongly
  ;; suggesting there is some kind of eager macroexpansion that fails
  ;; because straight.el has not yet installed the `leaf'
  ;; integration. I would have thought that putting a `require'
  ;; statement inside `eval-when-compile' (or even a bare `require')
  ;; after we request `leaf' from straight.el would solve the
  ;; problem, but unfortunately it does not. As such, the hack.
  (require 'leaf)
  (load user-init-file nil 'nomessage)
  (message "Reloading init-file...done")
  (run-hooks radian-after-reload-hook))

(radian-bind-key "r" #'radian-reload-init)

(defun radian-eval-buffer-or-region (&optional start end)
  "Evaluate the current region, or the whole buffer if no region is active.
In Lisp code, START and END denote the region to be evaluated;
they default to `point-min' and `point-max' respectively.

If evaluating a buffer visiting this file, then delegate instead
to `radian-reload-init'."
  (interactive)
  (if (and buffer-file-name
           (member (file-truename buffer-file-name)
                   (list
                    (when (bound-and-true-p early-init-file)
                      (file-truename early-init-file))
                    (file-truename user-init-file)
                    (file-truename radian-lib-file)
                    (file-truename radian-local-init-file)))
           (not (region-active-p)))
      (radian-reload-init)
    (let ((name nil))
      (if (region-active-p)
          (progn
            (setq start (region-beginning))
            (setq end (region-end))
            (setq name "region"))
        (setq start (point-min))
        (setq end (point-max))
        (setq name (buffer-name)))
      (let ((load-file-name (buffer-file-name)))
        (message "Evaluating %s..." name)
        (eval-region start end)
        (message "Evaluating %s...done" name)))))

;; This keybinding is used for evaluating a buffer of Clojure code in
;; CIDER, and for evaluating a buffer of Scheme code in Geiser.
(dolist (map (list emacs-lisp-mode-map lisp-interaction-mode-map))
  (define-key map "\C-c\C-k" #'radian-eval-buffer-or-region))

(defun radian-find-symbol (&optional symbol)
  "Same as `xref-find-definitions' but only for Elisp symbols.
SYMBOL is as in `xref-find-definitions'."
  (interactive)
  (let ((xref-backend-functions '(elisp--xref-backend))
        ;; Make this command behave the same as `find-function' and
        ;; `find-variable', i.e. always prompt for an identifier,
        ;; defaulting to the one at point.
        (xref-prompt-for-identifier t))
    ;; for slience unused warnings, when byte-compile.
    (eval-when-compile (require 'xref))
    (if symbol
        (xref-find-definitions symbol)
      (call-interactively 'xref-find-definitions))))

;; By default, C-h f, C-h v, and C-h o are bound to
;; `describe-function', `describe-variable', and `describe-symbol'
;; respectively. By analogy, C-h C-f, C-h C-v, and C-h C-o should be
;; bound as follows. (There's no `find-symbol' function by default for
;; some reason; note that `xref-find-definitions' is not a replacement
;; because it is major-mode dependent.) By further analogy, we should
;; bind `find-library'.
(-keys (("C-h f"   . find-function)
        ("C-h v"   . find-variable)
        ("C-h o"   . radian-find-symbol)
        ("C-h C-l" . find-library)
        ("C-h C-f" . describe-function)
        ("C-h C-v" . describe-variable)
        ("C-h C-o" . describe-symbol)
        ("C-h C-e" . view-echo-area-messages)))

;;;;; Locate emacs src directory
;; Let's establish a standard location for the Emacs source code.
(setq source-directory (expand-file-name "src" user-emacs-directory))

;; This is initialized to nil by `find-func' if the source is not
;; cloned when the library is loaded.
(setq find-function-C-source-directory (expand-file-name "src" source-directory))

(defun radian-clone-emacs-source-maybe ()
  "Prompt user to clone Emacs source repository if needed."
  (when (and (not (file-directory-p source-directory))
             (not (get-buffer "*clone-emacs-src*"))
             (yes-or-no-p "Clone Emacs source repository? "))
    (make-directory (file-name-directory source-directory) 'parents)
    (let ((compilation-buffer-name-function
           (lambda (&rest _)
             "*clone-emacs-src*")))
      (save-current-buffer
        (compile
         (format
          "git clone https://github.com/emacs-mirror/emacs.git %s"
          (shell-quote-argument source-directory)))))))

;; Feature `find-func' provides the ability for you to locate the
;; definitions of Emacs Lisp functions and variables.
(-ow! find-func

  :config

  (defadvice! radian--advice-find-func-clone-emacs-source (&rest _)
    "Clone Emacs source if needed to view definition."
    :before #'find-function-C-source
    (radian-clone-emacs-source-maybe)))

;; Package `macrostep' provides a facility for interactively expanding
;; Elisp macros.
(pow! macrostep
  :bind ("C-c e" . macrostep-expand)
  :config
  (add-hook! 'macrostep-mode-hook
    (defun radian--meow-setup-macrostep-h ()
      "Let macrostep-mode-keymap work."
      (if (bound-and-true-p meow-mode)
          (if macrostep-mode
              (meow--switch-state 'motion)
            (meow--switch-state 'normal))))))


;;;;; Emacs Lisp byte-compilation

;; Feature `bytecomp' handles byte-compilation of Emacs Lisp code.
(-ow bytecomp
  :config

  ;; Eliminate two warnings that are essentially useless for me. The
  ;; `make-local' warning gets triggered every time you call
  ;; `define-minor-mode' inside of `leaf', and the `noruntime'
  ;; warning gets triggered basically all the time for everything.
  (setq byte-compile-warnings '(not make-local noruntime))

  (defun radian-batch-byte-compile ()
    "Byte-compile radian.el. For usage in batch mode."
    (byte-compile-file radian-lib-file))

  (defun radian-byte-compile (&optional report-progress)
    "Byte-compile radian.el. For interactive usage.
REPORT-PROGRESS non-nil (or interactively) means to print more
messages."
    (interactive (list 'report-progress))
    (cl-block nil
      (unless (cl-some (lambda (f)
                         (file-newer-than-file-p f (concat radian-lib-file "c")))
                       (append (list radian-lib-file)
                               (directory-files *radian-lisp/* t ".+\\.el")))
        (when report-progress
          (message "Byte-compiled configuration already up to date"))
        (cl-return))
      (when report-progress
        (message "Byte-compiling updated configuration..."))
      (ignore-errors
        (kill-buffer " *radian-byte-compile*"))
      (let ((default-directory *radian-directory*))
        (radian-env-setup)
        (make-process
         :name "radian-byte-compile"
         :buffer " *radian-byte-compile*"
         :command '("make" "compile")
         :noquery t
         :sentinel
         (lambda (proc _event)
           (unless (process-live-p proc)
             (with-current-buffer (process-buffer proc)
               (if (= 0 (process-exit-status proc))
                   (progn
                     (insert "Byte-compilation completed successfully!\n")
                     (message
                      (if report-progress
                          "Byte-compiling updated configuration...done"
                        "Byte-compiled updated configuration")))
                 (message "!! FAILED TO BYTE-COMPILE:\n%s" (buffer-string))))))))))

  :blackout (emacs-lisp-compilation-mode . "Byte-Compile"))

;;;;; Emacs Lisp linting
;; Feature `checkdoc' provides some tools for validating Elisp
;; docstrings against common conventions.
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

;; Package `package-lint' provides a command that lets you check for
;; common package.el packaging problems in your packages.
(pow! package-lint)

;;; Applications
;;;; Email


;;;; Organization
;; Use `-ow!' here because we already installed Org earlier.
(-ow! org
  :preface
  (pow ox-pandoc)
  (pow htmlize)
  (pow orgit)
  (pow orgit-forge)
  (pow ox-clip)
  (sup 'org-appear)
  (sup 'org-superstar)
  (sup 'toc-org)
  (req! -org)
  :chord (",c" . org-capture)
  :increment
  calendar find-func format-spec org-macs org-compat org-faces
  org-entities org-list org-pcomplete org-src org-footnote
  org-macro ob org org-agenda org-capture

  :init
  (setq org-use-property-inheritance t   ; it's convenient to have properties inherited
        org-log-done 'time               ; having the time a item is done sounds convininet
        org-list-allow-alphabetical t    ; have a. A. a) A) list bullets
        org-export-in-background t       ; run export processes in external emacs process
        org-catch-invisible-edits 'smart ; try not to accidently do weird stuff in invisible regions
        org-re-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js")

  :config
  (-ow! org-appear
    :hook (org-mode-hook . org-appear-mode)
    :config
    (setq org-appear-autoemphasis t
          org-appear-autosubmarkers t
          org-appear-autoentities t
          org-appear-autolinks t
          org-appear-delay 0.1)
    ;; for proper first-time setup, `org-appear--set-elements'
    ;; needs to be run after other hooks have acted.
    (run-at-time nil nil #'org-appear--set-elements))

  (-ow! org-superstar
    :hook (org-mode-hook . org-superstar-mode)
    :config
    (setq org-superstar-leading-bullet ?\s
          org-superstar-leading-fallback ?\s
          org-hide-leading-stars nil
          org-superstar-todo-bullet-alist
          '(("TODO" . 9744) ("[ ]"  . 9744)
            ("DONE" . 9745) ("[X]"  . 9745))
          org-superstar-headline-bullets-list '("◉" "○" "✸" "✿" "✤" "✜" "◆" "▶")
          org-superstar-prettify-item-bullets t ))

  (-ow! toc-org ; auto-table of contents
    :hook (org-mode-hook . toc-org-enable)
    :config
    (setq toc-org-hrefify-default "gh")

    (defadvice! +org-inhibit-scrolling-a (fn &rest args)
      "Prevent the jarring scrolling that occurs when the-ToC is regenerated."
      :around #'toc-org-insert-toc
      (let ((p (set-marker (make-marker) (point)))
            (s (window-start)))
        (prog1 (apply fn args)
          (goto-char p)
          (set-window-start nil s t)
          (set-marker p nil)))))

  ;; HACK `org-id' doesn't check if `org-id-locations-file' exists or is
  ;;      writeable before trying to read/write to it.
  (defadvice! +org--fail-gracefully-a (&rest _)
    :before-while '(org-id-locations-save org-id-locations-load)
    (file-writable-p org-id-locations-file))

  ;; Add the ability to play gifs, at point or throughout the buffer. However,
  ;; 'playgifs' is stupid slow and there's not much I can do to fix it; use at
  ;; your own risk.
  (add-to-list 'org-startup-options '("inlinegifs" +org-startup-with-animated-gifs at-point))
  (add-to-list 'org-startup-options '("playgifs"   +org-startup-with-animated-gifs t))
  (add-hook! 'org-mode-local-vars-hook
    (defun +org-init-gifs-h ()
      (remove-hook 'post-command-hook #'+org-play-gif-at-point-h t)
      (remove-hook 'post-command-hook #'+org-play-all-gifs-h t)
      (pcase +org-startup-with-animated-gifs
        (`at-point (add-hook 'post-command-hook #'+org-play-gif-at-point-h nil t))
        (`t (add-hook 'post-command-hook #'+org-play-all-gifs-h nil t)))))

  (put 'org-tags-exclude-from-inheritance 'safe-local-variable
       #'radian--list-of-strings-p)

  ;; Feature `org-indent' provides an alternative view for Org files in
  ;; which sub-headings are indented.
  (-ow! org-indent

    :init

    (add-hook 'org-mode-hook #'org-indent-mode))
  ;; Feature `org-agenda' from package `org' provides the agenda view
  ;; functionality, which allows for collating TODO items from your Org
  ;; files into a single buffer.
  (-ow! org-agenda

    :config

    (defadvice! radian--advice-org-agenda-default-directory
      (org-agenda &rest args)
      "If `org-directory' exists, set `default-directory' to it in the agenda.
This makes the behavior of `find-file' more reasonable."
      :around #'org-agenda
      (let ((default-directory (if (file-exists-p org-directory)
                                   org-directory
                                 default-directory)))
        (apply org-agenda args)))

    (defadvice! radian--advice-blackout-org-agenda
      (&rest _)
      "Override the `org-agenda' mode lighter to just \"Org-Agenda\"."
      "Org-Agenda"
      :override #'org-agenda-set-mode-name)

    (add-hook! 'org-agenda-mode-hook
      (defun radian--org-agenda-setup ()
        "Disable `visual-line-mode' locally."
        ;; See https://superuser.com/a/531670/326239.
        (visual-line-mode -1)
        (let ((inhibit-message t)
              (message-log-max nil))
          ;; I'm not exactly sure why this is necessary. More research is
          ;; needed.
          (toggle-truncate-lines +1))))

    ;; Hide blocked tasks in the agenda view.
    (setq org-agenda-dim-blocked-tasks 'invisible))

  (-ow! org-crypt ; built-in
    :commands org-encrypt-entries org-encrypt-entry org-decrypt-entries org-decrypt-entry
    :hook (org-reveal-start-hook . org-decrypt-entry)
    :preface
    ;; org-crypt falls back to CRYPTKEY property then `epa-file-encrypt-to', which
    ;; is a better default than the empty string `org-crypt-key' defaults to.
    (defvar org-crypt-key nil)
    (after! org
      (add-to-list 'org-tags-exclude-from-inheritance "crypt")
      (add-hook! 'org-mode-hook
        (add-hook 'before-save-hook 'org-encrypt-entries nil t))))

  ;; Feature `org-clock' from package `org' provides the task clocking
  ;; functionality.
  (-ow! org-clock
    ;; We have to autoload these functions in order for the below code
    ;; that enables clock persistence without slowing down startup to
    ;; work.
    :commands (org-clock-load org-clock-save)
    :init

    ;; Allow clock data to be saved persistently.
    (setq org-clock-persist t)

    ;; Actually enable clock persistence. This is taken from
    ;; `org-clock-persistence-insinuate', but we can't use that function
    ;; since it causes both `org' and `org-clock' to be loaded for no
    ;; good reason.
    (add-hook 'org-mode-hook 'org-clock-load)
    (add-hook! 'kill-emacs-hook
      (defun radian--org-clock-save ()
        "Run `org-clock-save', but only if Org has been loaded.
Using this on `kill-emacs-hook' instead of `org-clock-save'
prevents a delay on killing Emacs when Org was not yet loaded."
        (when (featurep 'org)
          (org-clock-save))))

    (defun radian--advice-org-clock-load-automatically (&rest _)
      "Run `org-clock-load'.
This is a `:before' advice for various Org functions which might
be invoked before `org-mode-hook' is run."
      (org-clock-load))

    :config

    (advice-add #'org-clock-load :around #'fn-quiet!)

    (dolist (fun '(org-clock-in
                   org-clock-out
                   org-clock-in-last
                   org-clock-goto
                   org-clock-cancel))
      (advice-add fun :before #'radian--advice-org-clock-load-automatically))))


;;;; Roam

(-ow! org-roam
  :straight (org-roam :host github :repo "org-roam/org-roam" :files (:defaults "extensions/*"))
  :preface
  ;; Set this to nil so we can later detect if the user has set custom values
  ;; for these variables. If not, default values will be set in the :config
  ;; section.
  (defvar org-roam-directory nil)
  (defvar org-roam-db-location nil)
  (defvar +org-roam-open-buffer-on-find-file t
    "If non-nil, open the org-roam buffer when opening an org roam file.")
  (defvar +org-roam-link-to-org-use-id 'create-if-interactive
    "`org-roam-directory' local value for `org-id-link-to-org-use-id'.
It's not recommended to set this to nil in order for other parts
of org-mode to properly utilize ID links.")

  (setq org-roam-directory
        (thread-first (or org-roam-directory "roam")
                      (expand-file-name org-directory)
                      (file-truename)
                      (file-name-as-directory))
        org-roam-db-location
        (expand-file-name "org-roam.db" org-roam-directory)
        org-roam-node-display-template
        "${radian-hierarchy:*} ${radian-tags:45}"
        org-roam-completion-everywhere t
        org-roam-mode-section-functions
        #'(org-roam-backlinks-section org-roam-reflinks-section)
        org-roam-db-gc-threshold most-positive-fixnum)

  :increment
  ansi-color dash f rx seq magit-section emacsql emacsql-sqlite
  :init/el-patch
  ;; WORKAROUND:
  ;; https://github.com/org-roam/org-roam/issues/1221#issuecomment-959871775
  (defun org-roam-db-query (sql &rest args)
    "Run SQL query on Org-roam database with ARGS.
SQL can be either the emacsql vector representation, or a string."
    (el-patch-add (sleep-for 0 1))
    (apply #'emacsql (org-roam-db) sql args))

  :init
  ;; Don't display warning message dedicated for v1 users. Need to be set early.
  (setq org-roam-v2-ack t)

  (defadvice! +org-roam-suppress-sqlite-build-a (fn &rest args)
    "Suppress automatic building of sqlite3 binary when loading `org-roam'.
This is a blocking operation that can take a while to complete
and better be deferred when there will be an actual demand for
the database. See `+org-init-roam-h' for the launch process."
    :around #'emacsql-sqlite-ensure-binary
    (if (not (boundp 'org-roam-db-version))
        (apply fn args)
      (advice-remove #'emacsql-sqlite-ensure-binary #'+org-roam-suppress-sqlite-build-a)
      nil))

  (defun +org-init-roam-h ()
    "Setup `org-roam' but don't immediately initialize its database.
Instead, initialize it when it will be actually needed."
    (require 'org-roam-db)
    (cl-letf (((symbol-function #'org-roam-db-sync) #'ignore))
      (org-roam-db-autosync-enable))
    (defadvice! +org-roam-try-init-db-a (&rest _)
      "Try to initialize org-roam database at the last possible safe moment.
In case of failure, fail gracefully."
      :before #'org-roam-db-query
      (message "Initializing org-roam database...")
      (let ((run-cleanup-p t))
        (unwind-protect
            ;; Try to build the binary if it doesn't exist. In case of failure
            ;; this will error, run the cleanup and exit, and in case of success
            ;; this will return nil and sync the database.
            (setq run-cleanup-p (emacsql-sqlite-ensure-binary))
          (when run-cleanup-p
            (org-roam-db-autosync-disable)
            (message (concat "EmacSQL failied to build SQLite binary for org-roam; "
                             "see *Compile-Log* buffer for details.\n"
                             "To try reinitialize org-roam, run \"M-x org-roam-db-autosync-enable\"")))))
      (advice-remove 'org-roam-db-query #'+org-roam-try-init-db-a)
      (org-roam-db-sync)))

  (eval-after-load 'org (+org-init-roam-h))

  :bind (radian-comma-keymap ("rc". org-roam-capture))

  :config
  (setq-hook! 'org-roam-find-file-hook
    org-id-link-to-org-use-id +org-roam-link-to-org-use-id)

  ;; Normally, the org-roam buffer doesn't open until you explicitly call
  ;; `org-roam'. If `+org-roam-open-buffer-on-find-file' is non-nil, the
  ;; org-roam buffer will be opened for you whenever you visit a file in
  ;; `org-roam-directory'.
  (add-hook! 'org-roam-find-file-hook :append
    (defun +org-roam-open-with-buffer-maybe-h ()
      (and +org-roam-open-buffer-on-find-file
           (not org-roam-capture--node) ; don't proc for roam capture buffers
           (not org-capture-mode) ; don't proc for normal capture buffers
           (not (eq 'visible (org-roam-buffer--visibility)))
           (org-roam-buffer-toggle))))

  (set-popup-rules!
    `((,(regexp-quote org-roam-buffer) ; persistent org-roam buffer
       :side right :width 0.33 :height 0.5 :ttl nil :modeline nil :quit nil :slot 1)
      ("^\\*org-roam: " ; node dedicated org-roam buffer
       :side right :width 0.33 :height 0.5 :ttl nil :modeline nil :quit nil :slot 2)))

  ;; (after! shackle
  ;;   (cl-pushnew '("*org-roam*" :modeline nil :popup t :align right :size 0.22) shackle-rules))

  (add-hook 'org-roam-mode-hook #'turn-on-visual-line-mode))


;;;; Filesystem management
;; When deleting a file interactively, move it to the trash instead.
(setq delete-by-moving-to-trash t)

;; Package `osx-trash' provides functionality that allows Emacs to
;; place files in the trash on macOS.
(pow osx-trash
  :disabled (not (and IS-MAC (featurep! osx-trash)))
  :commands (osx-trash-move-file-to-trash)
  :init

  (defun osx-trash-setup ()
    "Provide trash support for OS X.

Provide `system-move-file-to-trash' as an alias for
`osx-trash-move-file-to-trash'.

Note that you still need to set `delete-by-moving-to-trash' to a
non-nil value to enable trashing for file operations."
    (when (and (eq system-type 'darwin)
               (not (fboundp 'system-move-file-to-trash)))
      (defalias 'system-move-file-to-trash
        'osx-trash-move-file-to-trash)))

  (osx-trash-setup))

;;;;; Dired
;; Dired has some trouble parsing out filenames that have e.g. leading
;; spaces, unless the ls program used has support for Dired. GNU ls
;; has this support, so if it is available we tell Dired (and the
;; `list-directory' command, not that it sees much use) to use it.
;;
;; This is in an advice so that we can defer the PATH search until
;; necessary.
(defadvice! radian--use-gls-for-list-directory (&rest _)
  "Make Dired use GNU ls, if it is available."
  :before #'list-directory
  (when (executable-find "gls")
    (setq insert-directory-program "gls"))
  ;; Only do the check once, for efficiency.
  (advice-remove #'list-directory #'radian--use-gls-for-list-directory))

;; Feature `dired' provides a simplistic filesystem manager in Emacs.
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
  (setq dired-auto-revert-buffer t))

(-ow dired-x
  :bind (;; Bindings for jumping to the current directory in Dired.
         ("C-x C-j" . dired-jump)
         ("C-x 4 C-j" . dired-jump-other-window))
  :config

  ;; Prevent annoying "Omitted N lines" messages when auto-reverting.
  (setq dired-omit-verbose nil)

  (when IS-MAC
    (defadvice! radian--advice-dired-guess-open-on-macos
      (&rest _)
      "Cause Dired's '!' command to use open(1).
This advice is only activated on macOS, where it is helpful since
most of the Linux utilities in `dired-guess-shell-alist-default'
are probably not going to be installed."
      :override #'dired-guess-default)))


;; find-name-dired find stuff from different directory.
(-ow find-dired :setq (find-ls-option . '("-print0 | xargs -0 ls -ld" . "-ld")))

;;;; Terminal emulator

;; Feature `term' provides a workable, though slow, terminal emulator
;; within Emacs.
(-ow! term
  ;; Allow usage of more commands from within the terminal.
  :bind (term-raw-map
         ("M-x" . execute-extended-command)
         ("C-h" . help-command)))

;;;; Version control

;; Feature `vc-hooks' provides hooks for the Emacs VC package. We
;; don't use VC, because Magit is superior in pretty much every way.

;; Disable VC. This improves performance and disables some annoying
;; warning messages and prompts, especially regarding symlinks. See
;; https://stackoverflow.com/a/6190338/3538165.
(exclude "I USE VC" (-ow! vc-hooks :config (setq vc-handled-backends nil)))

;; Feature `smerge-mode' provides an interactive mode for visualizing
;; and resolving Git merge conflicts.
(-ow smerge-mode :blackout t)

;; Package `magit' provides a full graphical interface for Git within
;; Emacs.
(pow! magit
  :leaf-autoload nil
  :bind (;; Alternate transient entry point; binding recommended in
         ;; <https://magit.vc/manual/magit.html#Transient-Commands>.
         ("C-x M-g" . magit-dispatch)
         ;; Completing the trio of bindings in `magit-file-mode-map'.
         ("C-c M-g" . magit-file-dispatch))

  :init

  (defadvice! +magit--ignore-version-a (fn &rest args)
    :around #'magit-version
    (let ((inhibit-message (not (called-interactively-p 'any))))
      (apply fn args)))

  ;; Suppress the message we get about "Turning on
  ;; magit-auto-revert-mode" when loading Magit.
  (setq magit-no-message '("Turning on magit-auto-revert-mode..."))

  :config/el-patch

  ;; Prevent Emacs asking if we're sure we want to exit, if a
  ;; Magit-spawned git-credential-cache process is running.
  (defun magit-maybe-start-credential-cache-daemon ()
    "Maybe start a `git-credential-cache--daemon' process.

If such a process is already running or if the value of option
`magit-credential-cache-daemon-socket' is nil, then do nothing.
Otherwise start the process passing the value of that options
as argument."
    (unless (or (not magit-credential-cache-daemon-socket)
                (process-live-p magit-credential-cache-daemon-process)
                (memq magit-credential-cache-daemon-process
                      (list-system-processes)))
      (setq magit-credential-cache-daemon-process
            (or (--first (let* ((attr (process-attributes it))
                                (comm (cdr (assq 'comm attr)))
                                (user (cdr (assq 'user attr))))
                           (and (string= comm "git-credential-cache--daemon")
                                (string= user user-login-name)))
                         (list-system-processes))
                (condition-case nil
                    (el-patch-wrap 2
                      (with-current-buffer
                          (get-buffer-create " *git-credential-cache--daemon*")
                        (start-process "git-credential-cache--daemon"
                                       (el-patch-swap
                                         " *git-credential-cache--daemon*"
                                         (current-buffer))
                                       (magit-git-executable)
                                       "credential-cache--daemon"
                                       magit-credential-cache-daemon-socket)
                        (el-patch-add
                          (set-process-query-on-exit-flag
                           (get-buffer-process (current-buffer)) nil))))
                  ;; Some Git implementations (e.g. Windows) won't have
                  ;; this program; if we fail the first time, stop trying.
                  ((debug error)
                   (remove-hook
                    'magit-credential-hook
                    #'magit-maybe-start-credential-cache-daemon)))))))

  :config

  ;; The default location for git-credential-cache is in
  ;; ~/.config/git/credential. However, if ~/.git-credential-cache/
  ;; exists, then it is used instead. Magit seems to be hardcoded to
  ;; use the latter, so here we override it to have more correct
  ;; behavior.
  (unless (file-exists-p "~/.git-credential-cache/")
    (let* ((xdg-config-home (or (getenv "XDG_CONFIG_HOME")
                                (expand-file-name "~/.config/")))
           (socket (expand-file-name "git/credential/socket" xdg-config-home)))
      (setq magit-credential-cache-daemon-socket socket)))

  ;; Don't try to save unsaved buffers when using Magit. We know
  ;; perfectly well that we need to save our buffers if we want Magit
  ;; to see them.
  (setq magit-save-repository-buffers nil)

  (transient-append-suffix
    'magit-merge "-n"
    '("-u" "Allow unrelated" "--allow-unrelated-histories"))

  (transient-append-suffix 'magit-pull "-r"
    '("-a" "Autostash" "--autostash"))

  (transient-append-suffix 'magit-fetch "-t"
    '("-u" "Unshallow" "--unshallow")))

;; Feature `magit-diff' from package `magit' handles all the stuff
;; related to interactive Git diffs.
(-ow! magit-diff
  :config

  (defadvice! radian--magit-diff-revert-before-smerge (buf _pos)
    "Before calling `smerge-start-session', try to revert buffer.
This is necessary because it's possible that the file being
visited has changed on disk (due to merge conflict, for example)
but it was already visited, and hasn't been autoreverted yet
(because it hasn't been visible in a window, for example). But
`smerge-start-session', which is called by Magit while jumping
you to the file, will not wait for an autorevert. It will just
see that there aren't any conflict markers in the file and
disable itself. Sad."
    :before #'magit-diff-visit-file--setup
    (with-current-buffer buf
      (auto-revert-handler))))

;; Feature `git-commit' from package `magit' provides the commit
;; message editing capabilities of Magit.
(-ow! git-commit
  :config

  ;; Max length for commit message summary is 50 characters as per
  ;; https://chris.beams.io/posts/git-commit/.
  (setq git-commit-summary-max-length 50))

;; Package `magit-todo'
(pow! magit-todos
  :defun rxt--re-builder-switch-pcre-mode
  :commands magit-todos-mode magit-todos-list
  :after hl-todo)

;; Package `emacsql-sqlite' is a dependency of Forge which is used to
;; interact with the SQLite database that Forge uses to keep track of
;; information about pull requests.
(-ow! emacsql-sqlite

  :init

  ;; Put the EmacSQL binary in the repository, not the build dir. That
  ;; way we don't have to recompile it every time packages get rebuilt
  ;; by straight.el. See
  ;; <https://github.com/raxod502/straight.el/issues/274> for not
  ;; having to use the internal function `straight--dir'.
  (setq emacsql-sqlite-data-root (straight--repos-dir "emacsql"))

  :config

  (defadvice! radian--advice-emacsql-no-compile-during-compile
    (&rest _)
    "Prevent EmacSQL from trying to compile stuff during byte-compilation.
This is a problem because Forge tries to get EmacSQL to compile
its binary at load time, which is bad (you should never do
anything significant at package load time) since it breaks CI."
    :before-until #'emacsql-sqlite-ensure-binary
    byte-compile-current-file))

;; Package `forge' provides a GitHub/GitLab/etc. interface directly
;; within Magit.
;; Feature `forge-core' from package `forge' implements the core
;; functionality.
(pow! forge

  :config

  (defadvice! radian--forge-get-repository-lazily (&rest _)
    "Make `forge-get-repository' return nil if the binary isn't built yet.
This prevents having EmacSQL try to build its binary (which may
be annoying, inconvenient, or impossible depending on the
situation) just because you tried to do literally anything with
Magit."
    :before-while #'forge-get-repository
    (file-executable-p emacsql-sqlite-executable))

  (defadvice! radian--forge-build-binary-lazily (&rest _)
    "Make `forge-dispatch' build the binary if necessary.
Normally, the binary gets built as soon as Forge is loaded, which
is terrible UX. We disable that above, so we now have to manually
make sure it does get built when we actually issue a Forge
command."
    :before #'forge-dispatch
    (unless (file-executable-p emacsql-sqlite-executable)
      (emacsql-sqlite-compile 2))))

;; Package `git-gutter' adds a column to the left-hand side of each
;; window, showing which lines have been added, removed, or modified
;; since the last Git commit.
(pow! git-gutter

  :hook (radian-first-file-hook . global-git-gutter-mode)
  :commands (git-gutter:previous-hunk
             git-gutter:next-hunk
             radian-git-gutter:beginning-of-hunk
             git-gutter:end-of-hunk
             git-gutter:revert-hunk)
  :init
  (-keys (radian-comma-keymap
          ("v p" . git-gutter:previous-hunk)
          ("v n" . git-gutter:next-hunk)
          ("v a" . radian-git-gutter:beginning-of-hunk)
          ("v e" . git-gutter:end-of-hunk)
          ("v k" . git-gutter:revert-hunk)))

  ;; Disable in Org mode, as per
  ;; <https://github.com/syl20bnr/spacemacs/issues/10555> and
  ;; <https://github.com/syohex/emacs-git-gutter/issues/24>.
  ;; Apparently, the mode-enabling function for global minor modes
  ;; gets called for new buffers while they are still in
  ;; `fundamental-mode', before a major mode has been assigned. I
  ;; don't know why this is the case, but adding `fundamental-mode'
  ;; here fixes the issue.
  (setq git-gutter:disabled-modes '(fundamental-mode org-mode))

  ;; (add-hook! 'find-file-hook
  ;;   (defun radian--git-gutter-load ()
  ;;     "Load `git-gutter' when initially finding a file."
  ;;     (require 'git-gutter)
  ;;     (remove-hook 'find-file-hook #'radian--git-gutter-load)))

  :config

  ;; Don't prompt when reverting hunk.
  (setq git-gutter:ask-p nil)

  (defun radian-git-gutter:beginning-of-hunk ()
    "Move to beginning of current diff hunk."
    (interactive)
    (git-gutter:awhen (git-gutter:search-here-diffinfo git-gutter:diffinfos)
      (let ((lines (- (git-gutter-hunk-start-line it) (line-number-at-pos))))
        ;; This will move backwards since lines will be negative.
        (forward-line lines))))

  ;; Shuffle around all the hooks. `git-gutter' puts itself on a bunch
  ;; of different things, but not exactly the right things. Remove all
  ;; its meddling, and then do the right thing (run on window or
  ;; buffer switch after a top-level command, after a buffer revert,
  ;; and after Apheleia runs).

  (remove-hook 'post-command-hook #'git-gutter:post-command-hook)
  (advice-remove 'quit-window #'git-gutter:quit-window)
  (advice-remove 'switch-to-buffer #'git-gutter:switch-to-buffer)

  (defvar radian--git-gutter-last-buffer-and-window nil
    "Cons of current buffer and selected window before last command.
This is used to detect when the current buffer or selected window
changes, which means that `git-gutter' needs to be re-run.")

  (add-hook! 'post-command-hook
    (defun radian--git-gutter-on-buffer-or-window-change ()
      "Update `git-gutter' when current buffer or selected window changes."
      (let ((new (cons (current-buffer) (selected-window))))
        (unless (equal new radian--git-gutter-last-buffer-and-window)
          (setq radian--git-gutter-last-buffer-and-window new)
          ;; Sometimes the current buffer has not gotten updated yet
          ;; after switching window, for example after `quit-window'.
          (with-current-buffer (window-buffer)
            (when git-gutter-mode
              (when buffer-file-name
                (unless (file-remote-p buffer-file-name)
                  (git-gutter)))))))))

  (-ow autorevert
    :config

    (add-hook! 'after-revert-hook
      (defun radian--git-gutter-after-autorevert ()
        "Update `git-gutter' after the buffer is autoreverted."
        (when git-gutter-mode
          (git-gutter)))))

  (-ow apheleia
    :config
    (add-hook! 'apheleia-post-format-hook
      (defun radian--git-gutter-after-apheleia ()
        "Update `git-gutter' after Apheleia formats the buffer."
        (when git-gutter-mode
          (git-gutter)))))

  :blackout git-gutter-mode)

;; Package `git-gutter-fringe' integrates with `git-gutter' to make
;; the gutter display use the window fringe rather than a column of
;; text.
;;
;; Note that we only even put the package on the load path if
;; `git-gutter-fringe' fringe is defined. The function might not be
;; defined if Emacs was not built with X/Cocoa support, and if that's
;; the case, then loading it will cause errors (and besides that, will
;; break `git-gutter' since the fringe stuff is not available).
;; However, we do need to load the package in order to byte-compile
;; this configuration. That's okay since it's only done in a
;; subprocess (so it won't break `git-gutter') but we still need to
;; fix the errors in that case. Hence the `eval-when-compile'.
(straight-register-package 'git-gutter-fringe)
(when (fboundp 'define-fringe-bitmap)
  (eval-when-compile
    (unless (fboundp 'define-fringe-bitmap)
      (fset 'define-fringe-bitmap #'ignore))
    (unless (boundp 'overflow-newline-into-fringe)
      (setq overflow-newline-into-fringe t)))
  (pow! git-gutter-fringe
    :after git-gutter
    :init
    ;; This function is only available when Emacs is built with
    ;; X/Cocoa support, see e.g.
    ;; <https://github.com/pft/mingus/issues/5>. If we try to
    ;; load/configure `git-gutter-fringe' without it, we run into
    ;; trouble.
    (when (fboundp 'define-fringe-bitmap)
      (require 'git-gutter-fringe))

    :config
    (fringe-helper-define 'radian--git-gutter-blank nil
      "........"
      "........"
      "........"
      "........"
      "........"
      "........"
      "........"
      "........")

    (defadvice! radian--advice-git-gutter-remove-bitmaps
      (func &rest args)
      "Disable the cutesy bitmap pluses and minuses from `git-gutter-fringe'.
Instead, display simply a flat colored region in the fringe."
      :around #'git-gutter-fr:view-diff-infos
      (letf! ((defun fringe-helper-insert-region
                  (beg end _bitmap &rest args)
                (apply fringe-helper-insert-region
                       beg end 'radian--git-gutter-blank args)))
        (apply func args)))))

;;;; Internet applications
;; Feature `browse-url' provides commands for opening URLs in
;; browsers.
(exclude "WIP"
(-ow! browse-url
  :bind ("C-c C-o" . (cmds! (radian--browse-url-predicate) #'browse-url-at-point))
  :init
  (defun radian--browse-url-predicate ()
    "Return non-nil if \\[browse-url-at-point] should be rebound."
    ;; All of these major modes provide more featureful bindings for
    ;; C-c C-o than `browse-url-at-point'.
    (not (derived-mode-p
          #'markdown-mode #'org-mode #'org-agenda-mode #'magit-mode)))))

;;;; Emacs profiling

;; Package `esup' allows you to run a child Emacs process with special
;; profiling functionality, and to collect timing results for each
;; form in your init-file.
(pow! esup
  :defer-config

  ;; Work around a bug where esup tries to step into the byte-compiled
  ;; version of `cl-lib', and fails horribly.
  (setq esup-depth 0)

  (defadvice! radian--advice-esup-unwrap-init-file
    (esup &optional init-file)
    "Help `esup' to work with the Radian init-file."
    :around #'esup
    (if init-file
        (funcall esup init-file)
      (let ((fname (expand-file-name "esup-init.el" temporary-file-directory)))
        (with-temp-file fname
          (print
           `(progn
              ;; We need this for `string-trim', but it's not
              ;; `require'd until the beginning of radian.el.
              (require 'subr-x)

              ;; Prevent indentation from being lost in the profiling
              ;; results.
              (advice-add #'esup-child-chomp :override #'string-trim)

              ;; esup does not set `user-init-file'.
              (setq user-init-file ,radian-lib-file)

              ;; If there's an error, let me see where it is.
              (setq debug-on-error t)

              ;; Make it possible to detect whether the init-file is
              ;; being profiled.
              (defvar radian--currently-profiling-p t)

              ;; Abbreviated (and flattened) version of init.el.
              (defvar radian-minimum-emacs-version ,radian-minimum-emacs-version)
              (defvar radian-local-init-file ,radian-local-init-file)
              (setq package-enable-at-startup nil)
              (setq custom-file
                    (expand-file-name
                     (format "custom-%d-%d.el" (emacs-pid) (random))
                     temporary-file-directory))
              (defvar radian-lib-file ,radian-lib-file)
              (defvar radian--finalize-init-hook nil))
           (current-buffer))
          (insert-file-contents-literally radian-lib-file)
          (goto-char (point-max))
          (print
           '(run-hooks 'radian--finalize-init-hook)
           (current-buffer)))
        (funcall esup fname)))))

;;;; calendar
(pow! cal-china-x
  :after calendar
  :aftercall calendar
  :setq (calendar-week-start-day . 0)
  :config
  (setq
   calendar-holidays
   '(;;公历节日
     (holiday-fixed 1 1 "元旦")
     (holiday-fixed 2 14 "情人节")
     (holiday-fixed 3 8 "妇女节")
     (holiday-fixed 3 14 "白色情人节")
     (holiday-fixed 4 1 "愚人节")
     (holiday-fixed 5 1 "劳动节")
     (holiday-fixed 5 4 "青年节")
     (holiday-float 5 0 2 "母亲节")
     (holiday-fixed 6 1 "儿童节")
     (holiday-float 6 0 3 "父亲节")
     (holiday-fixed 9 10 "教师节")
     (holiday-fixed 10 1 "国庆节")
     (holiday-fixed 10 24 "程序员节")
     (holiday-fixed 12 25 "圣诞节")
     ;; 农历节日
     (holiday-lunar 1 1 "春节" 0)
     (holiday-lunar 1 2 "春节" 0)
     (holiday-lunar 1 3 "春节" 0)
     (holiday-lunar 1 15 "元宵节" 0)
     (holiday-solar-term "清明" "清明")
     (holiday-solar-term "小寒" "小寒")
     (holiday-solar-term "大寒" "大寒")
     (holiday-solar-term "立春" "立春")
     (holiday-solar-term "雨水" "雨水")
     (holiday-solar-term "惊蛰" "惊蛰")
     (holiday-solar-term "春分" "春分")
     (holiday-solar-term "谷雨" "谷雨")
     (holiday-solar-term "立夏" "立夏")
     (holiday-solar-term "小满" "小满")
     (holiday-solar-term "芒种" "芒种")
     (holiday-solar-term "夏至" "夏至")
     (holiday-solar-term "小暑" "小暑")
     (holiday-solar-term "大暑" "大暑")
     (holiday-solar-term "立秋" "立秋")
     (holiday-solar-term "处暑" "处暑")
     (holiday-solar-term "白露" "白露")
     (holiday-solar-term "秋分" "秋分")
     (holiday-solar-term "寒露" "寒露")
     (holiday-solar-term "霜降" "霜降")
     (holiday-solar-term "立冬" "立冬")
     (holiday-solar-term "小雪" "小雪")
     (holiday-solar-term "大雪" "大雪")
     (holiday-solar-term "冬至" "冬至")
     (holiday-lunar 5 5 "端午" 0)
     (holiday-lunar 8 15 "中秋" 0)
     (holiday-lunar 7 7 "七夕" 0)
     (holiday-lunar 12 8 "腊八" 0)
     (holiday-lunar 9 9 "重阳" 0))))

;;; MODULE {Appearance}
;;;; pixel-scroll
(-ow pixel-scroll :emacs> 29 :init (pixel-scroll-precision-mode +1))

;;;; tab-bar
(-ow tab-bar
  :init
  (defun +tab-bar-right ()
    (let* ((p (or (cdr (project-current)) ""))
           (w (string-width p)))
      (concat
       (propertize
        " " 'display `((space :align-to (- (+ right right-fringe right-margin) ,w 1))))
       p)))

  (defun +tab-bar-tab-format-function (tab i)
    (let ((current-p (eq (car tab) 'current-tab)))
      (ignore i current-p)
      (concat
       (propertize (concat
                    " "
                    (alist-get 'name tab)
                    " ")
                   'face
                   (funcall tab-bar-tab-face-function tab))
       "")))
  :bind
  ([remap project-switch-project] . +tab-bar-switch-project)
  (radian-comma-keymap ("tp" . tab-bar-switch-to-prev-tab)
                       ("tn" . tab-bar-switch-to-next-tab)
                       ("tc" . tab-bar-close-tab))
  :custom (tab-bar-new-tab-choice . "*scratch*")
  :setq
  (tab-bar-border . nil)
  (tab-bar-close-button . nil)
  (tab-bar-back-button . nil)
  (tab-bar-new-button . nil)
  (tab-bar-format . '(tab-bar-format-tabs +tab-bar-right))
  (tab-bar-tab-name-format-function . '+tab-bar-tab-format-function)
  (tab-bar-tab-name-truncated-max . 10)
  :config
  (defun +tab-bar-switch-project ()
    "Switch to project in a new tab, project name will be used as tab name.
No tab will created if the command is cancelled."
    (interactive)
    (let (succ)
      (unwind-protect
          (progn
            (tab-bar-new-tab)
            (call-interactively #'project-switch-project)
            (when-let ((proj (project-current)))
              (tab-bar-rename-tab (format "%s" (file-name-nondirectory (directory-file-name (cdr proj)))))
              (setq succ t)))
        (unless succ
          (tab-bar-close-tab)))))
  (tab-bar-mode -1)
  (tab-bar-history-mode +1))

;;;; outline-minor-faces
(pow! outline-minor-faces
  :after outline
  :hook (emacs-lisp-mode-hook . outline-minor-faces-add-font-lock-keywords))

;;;; highlight-numbers
;; Many major modes do no highlighting of number literals, so we do it for them
(pow! highlight-numbers
  :hook ((prog-mode-hook conf-mode-hook) . highlight-numbers-mode)
  :config
  (setq highlight-numbers-generic-regexp "\\_<[[:digit:]]+\\(?:\\.[0-9]*\\)?\\_>"))

;;;; link-hint
(pow! link-hint
  :bind ("C-c C-o" . link-hint-open-link)
  :init
  (eval-after-load 'help-mode '(define-key help-mode-map "o" #'link-hint-open-link))
  (after! helpful (define-key helpful-mode-map "o" #'link-hint-open-link))
  (eval-after-load 'apropos '(define-key apropos-mode-map "o" #'link-hint-open-link))
  (eval-after-load 'info '(define-key Info-mode-map "o" #'link-hint-open-link)))

;;;; hl-line
(-ow! hl-line
  ;; Highlights the current line
  :hook (radian-first-buffer-hook . global-hl-line-mode)
  :init
  (defvar global-hl-line-modes
    '(prog-mode text-mode conf-mode special-mode
                org-agenda-mode)
    "What modes to enable `hl-line-mode' in.")
  :config
  ;; HACK I reimplement `global-hl-line-mode' so we can white/blacklist modes in
  ;;      `global-hl-line-modes' _and_ so we can use `global-hl-line-mode',
  ;;      which users expect to control hl-line in Emacs.
  (define-globalized-minor-mode global-hl-line-mode hl-line-mode
    (lambda ()
      (and (cond (hl-line-mode nil)
                 ((null global-hl-line-modes) nil)
                 ((eq global-hl-line-modes t))
                 ((eq (car global-hl-line-modes) 'not)
                  (not (derived-mode-p global-hl-line-modes)))
                 ((apply #'derived-mode-p global-hl-line-modes)))
           (hl-line-mode +1))))

  ;; Temporarily disable `hl-line' when selection is active, since it doesn't
  ;; serve much purpose when the selection is so much more visible.
  (defvar radian--hl-line-mode nil)

  (add-hook! 'hl-line-mode-hook
    (defun radian-truly-disable-hl-line-h ()
      (unless hl-line-mode
        (setq-local radian--hl-line-mode nil))))

  (add-hook! 'activate-mark-hook
    (defun radian-disable-hl-line-h ()
      (when hl-line-mode
        (hl-line-mode -1)
        (setq-local radian--hl-line-mode t))))

  (add-hook! 'deactivate-mark-hook
    (defun radian-enable-hl-line-maybe-h ()
      (when radian--hl-line-mode
        (hl-line-mode +1)))))

;;;; pulse.el
(-ow! pulse
  :defvar pulse-delay
  :config
  (defface radian-pulse-line
    '((default :extend t)
      (((class color) (min-colors 88) (background light)) :background "#8eecf4")
      (((class color) (min-colors 88) (background dark)) :background "#004065")
      (t :inverse-video t))
    "Default face for `pulse-line'."
    :group 'radian-pulse)

  (defun pulse-line (&rest _)
    "Pulse the current line."
    (interactive)
    (let ((start (if (eobp) (line-beginning-position 0) (line-beginning-position)))
          (end (line-beginning-position 2))
          (pulse-delay 0.05)
          (face 'radian-pulse-line))
      (pulse-momentary-highlight-region start end face)))

  (dolist (command '(recenter-top-bottom select-window
                     move-to-window-line-top-bottom reposition-window
                     bookmark-jump other-window scroll-up-command
                     scroll-down-command org-next-visible-heading
                     org-previous-visible-heading
                     org-forward-heading-same-level
                     org-backward-heading-same-level
                     org-tree-slide-move-next-tree
                     org-tree-slide-move-previous-tree))
    (advice-add command :after #'pulse-line)))

;;;; Rainbow
(pow! rainbow-mode
  :hook ((prog-mode-hook text-mode-hook) . rainbow-mode)
  :config
  (add-hook! '(c-mode-hook c++-mode-hook) (rainbow-mode -1))
  (with-no-warnings
    ;; HACK: Use overlay instead of text properties to override `hl-line' faces.
    ;; @see https://emacs.stackexchange.com/questions/36420
    (defun my-rainbow-colorize-match (color &optional match)
      (let* ((match (or match 0))
             (ov (make-overlay (match-beginning match) (match-end match))))
        (overlay-put ov 'ovrainbow t)
        (overlay-put ov 'face `((:foreground ,(if (> 0.5 (rainbow-x-color-luminance color))
                                                  "white" "black"))
                                (:background ,color)))))
    (advice-add #'rainbow-colorize-match :override #'my-rainbow-colorize-match)
    (defun my-rainbow-clear-overlays ()
      "Clear all rainbow overlays."
      (remove-overlays (point-min) (point-max) 'ovrainbow t))
    (advice-add #'rainbow-turn-off :after #'my-rainbow-clear-overlays))
  :blackout t)

;;;; TAIL-CORE 

;;;; Appearance
(appendq! initial-frame-alist
              '((tool-bar-lines . 0)
                (fullscreen . maximized)
                ;;(width . 100)
                ;;(height . 30)
                ;;(left . 50)
                ;;(top . 50)
                ;;(undecorated . t)
                (internal-border-width . 0)
                (alpha-background . 80)
                (alpha . (95 . 80))))
(appendq! default-frame-alist initial-frame-alist)

(setq frame-title-format
      '(""
        (:eval
         (if (eq major-mode 'org-mode)
             (replace-regexp-in-string
              ".*/[0-9]*-?" "☰ "
              (subst-char-in-string ?_ ?  buffer-file-name))
           "%b"))
        (:eval (format (if (buffer-modified-p)  " ◉ %s" "  ●  %s")
                       (radian-project-name))))

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

  (eval-when! IS-MAC
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
  (when IS-MAC
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
(eval-when! IS-LINUX (setq x-gtk-use-system-tooltips nil))

;; Expand the minibuffer to fit multi-line text displayed in the echo-area. This
;; doesn't look too great with direnv, however...
(setq resize-mini-windows 'grow-only)

;; Try to keep the cursor out of the read-only portions of the minibuffer.
(setq minibuffer-prompt-properties '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;;;; Fonts
(req! -font)

;;;; Mode line
(-ow! recursion-indicator
  :straight (recursion-indicator :host github :repo "minad/recursion-indicator")
  :custom (recursion-indicator-minibuffer . "⮜")
  :init (recursion-indicator-mode +1))

;; Normally the buffer name is right-padded with whitespace until it
;; is at least 12 characters. This is a waste of space, so we
;; eliminate the padding here. Check the docstrings for more
;; information.
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

;;; Miscellaneous
;; Dos2unix on emacs-lisp-mode
(when IS-WINDOWS
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
      ;; pull in a ton of packages. `doom/open-scratch-buffer' provides a better
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

;; Emacs stores `authinfo' in $HOME and in plain-text. Let's not do that, mkay?
;; This file stores usernames, passwords, and other such treasures for the
;; aspiring malicious third party.
;; (setq auth-sources (list "~/.authinfo.gpg"))

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

;; Unfortunately, `which-key' sets an internal variable at load time
;; based on the value of `echo-keystrokes', and then later overrides
;; `echo-keystrokes' to the value of this internal variable,
;; effectively overwriting our configuration here. Stop that behavior.
(-ow! which-key :config (setq which-key-echo-keystrokes echo-keystrokes))

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

;;; Theme configuration.
(-ow modus-themes
  :init
  (setq
   modus-themes-italic-constructs t
   modus-themes-bold-constructs t
   modus-themes-no-mixed-fonts nil
   modus-themes-subtle-line-numbers t
   modus-themes-success-deuteranopia nil
   modus-themes-tabs-accented nil
   ;; only applies to `customize-set-variable' and related
   modus-themes-inhibit-reload t
   modus-themes-intense-markup t

   modus-themes-fringes nil ; {nil,'subtle,'intense}

   ;; Options for `modus-themes-lang-checkers' are either nil (the
   ;; default), or a list of properties that may include any of those
   ;; symbols: `straight-underline', `text-also', `background',
   ;; `intense'
   modus-themes-lang-checkers '(text-also intense)

   ;; Options for `modus-themes-mode-line' are either nil, or a list
   ;; that can combine any of `3d' OR `moody', `borderless',
   ;; `accented', `padded'.
   modus-themes-mode-line '(moody accented borderless)

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
   modus-themes-paren-match '(bold intense)

   ;; Options for `modus-themes-links' are either nil (the default),
   ;; or a list of properties that may include any of those symbols:
   ;; `neutral-underline' OR `no-underline', `faint' OR `no-color',
   ;; `bold', `italic', `background'
   modus-themes-links '(neutral-underline background)

   ;; Options for `modus-themes-prompts' are either nil (the
   ;; default), or a list of properties that may include any of those
   ;; symbols: `background', `bold', `gray', `intense', `italic'
   modus-themes-prompts '(intense bold)

   modus-themes-completions 'opinionated ; {nil,'moderate,'opinionated}

   modus-themes-mail-citations 'monochrome ; {nil,'faint,'monochrome}

   ;; Options for `modus-themes-region' are either nil (the default),
   ;; or a list of properties that may include any of those symbols:
   ;; `no-extend', `bg-only', `accented'
   modus-themes-region '(bg-only no-extend accented)

   ;; Options for `modus-themes-diffs': nil, 'desaturated,
   ;; 'bg-only, 'deuteranopia, 'fg-only-deuteranopia
   modus-themes-diffs nil

   ;; {nil,'gray-background,'tinted-background}
   modus-themes-org-blocks 'gray-background

   modus-themes-org-agenda ; this is an alist: read the manual or its doc string
   '((header-block . (variable-pitch scale-title))
     (header-date . (grayscale workaholic bold-today))
     (event . (accented italic varied))
     (scheduled . uniform)
     (habit . traffic-light-deuteranopia))

   modus-themes-headings ; this is an alist: read the manual or its doc string
   '((1 . (overline background))
     (2 . (rainbow overline))
     (t . t))

   modus-themes-variable-pitch-ui t
   modus-themes-variable-pitch-headings t
   modus-themes-scale-headings t
   modus-themes-scale-1 1.05
   modus-themes-scale-2 1.1
   modus-themes-scale-3 1.15
   modus-themes-scale-4 1.2
   modus-themes-scale-title 1.3
   modus-themes-scale-small 0.9))

(-ow nano-theme
  :straight `(nano-theme :local-repo ,(-contrib/ "nano-theme/") :type nil :build (:not compile))
  :custom
  (nano-theme-padded-modeline . nil)
  (nano-theme-header-scales . '(1.3 0.95 0.85 0.75 0.7 0.7 0.7))
  :config
  (eval-when! (boundp 'ns-system-appearance)
    (add-to-list
     'ns-system-appearance-change-functions
     (lambda (l?d) (setq nano-theme-light/dark l?d)
       (mapc #'disable-theme custom-enabled-themes)
       (load-theme 'nano t)))))

;;;; change theme and customize face.
(with-no-warnings
  (defun radian/change-theme ()
    (interactive)
    (let* ((theme-cons (car radian-theme-list))
           (theme (car theme-cons))
           (is-dark (cdr theme-cons))
           (class '((class color) (min-colors 89)))
           (fg         (if is-dark "#ECEFF4" "#37474F"))
           (bg         (if is-dark "#2E3440" "#FFFFFF"))
           (prompt     (if is-dark "violet" "red"))
           (match      (if is-dark "cyan" "blue"))
           (match2     (if is-dark "green" "#228b22"))
           (highlight  (if is-dark "#3B4252" "#FAFAFA"))
           (critical   (if is-dark "#EBCB8B" "#FF6F00"))
           (salient    (if is-dark "#81A1C1" "#673AB7"))
           (strong     (if is-dark "#ECEFF4" "#000000"))
           (popout     (if is-dark "#D08770" "#FFAB91"))
           (subtle     (if is-dark "#434C5E" "#ECEFF1"))
           (faded      (if is-dark "#677691" "#B0BEC5")))
      (setq radian-theme-list (append (cdr radian-theme-list) (list theme-cons)))

      (letf!
        (defun tinker-theme (theme)
          (custom-theme-set-faces
           theme
           ;; whitespace-line
           `(whitespace-line ((,class :background "yellow" :foreground "purple")))

           ;; vertico
           `(vertico-current ((,class :background ,subtle)))

           ;; git-gutter
           `(git-gutter:added ((,class :background "green")))
           `(git-gutter:deleted ((,class :background "red")))
           `(git-gutter:modified ((,class :background ,popout)))
           `(git-gutter:separator ((,class :background ,salient)))
           `(git-gutter:unchanged ((,class :background "purple")))
           ;; git-gutter-fr
           `(git-gutter-fr:added ((,class :background "green")))
           `(git-gutter-fr:deleted ((,class :background "red")))
           `(git-gutter-fr:modified ((,class :background ,popout)))

           ;; M-x prompt-face
           `(minibuffer-prompt      ((,class (:foreground ,prompt))))
           `(comint-highlight-input ((,class (:foreground "green" :bold t)))))
          (enable-theme theme))

        (cond ((memq theme '(modus-vivendi modus-operandi))
               (if is-dark
                   (setq modus-themes-vivendi-color-overrides
                         `((bg-main . ,bg) (fg-unfocused . ,fg)))
                 (setq modus-themes-operandi-color-overrides
                       `((bg-main . ,bg) (fg-unfocused . ,fg))))
               (disable-theme theme)
               (load-theme theme t)
               (tinker-theme theme))
              ((eq theme 'nano)
               (if (eq (car custom-enabled-themes) 'nano)
                   (nano-theme-toggle)
                 (load-theme theme t)))
              (t
               (disable-theme theme)
               (load-theme theme t)
               (tinker-theme theme)))))
    (radian-run-hooks 'radian-load-theme-hook)))

(-key "M-h" #'radian/change-theme)

;;;;;;; --> Expose <radian-after-init-hook> contents
(radian--run-hook after-init)

;;; Closing
(setq radian--current-feature 'normal)

(unless (or mini-p (bound-and-true-p radian--currently-profiling-p))
  ;; Prune the build cache for straight.el; this will prevent it from
  ;; growing too large. Do this after the final hook to prevent packages
  ;; installed there from being pruned.
  (straight-prune-build-cache)
  ;; Occasionally prune the build directory as well. For similar reasons
  ;; as above, we need to do this after local configuration.
  (if (= 0 (random 100)) (straight-prune-build-directory))

  ;; We should only get here if init was successful. If we do,
  ;; byte-compile this file asynchronously in a subprocess using the
  ;; Radian Makefile. That way, the next startup will be fast(er).
  (run-with-idle-timer 1 nil #'radian-byte-compile))

;;; Bootstrap interactive session

(defun radian-init-ui-h (&optional _)
  "Inintialize user interface by applying all its advice and hooks"
  (advice-add #'run-hooks :override #'radian-run-hooks)
  (radian-run-hooks 'radian-init-ui-hook)

  (add-hook 'kill-buffer-query-functions #'radian-protect-fallback-buffer-h)
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

;; Local Variables:
;; checkdoc-symbol-words: ("top-level")
;; indent-tabs-mode: nil
;; no-native-compile: t
;; outline-regexp: ";;;;* [^ 	\n]\\|("
;; sentence-end-double-space: nil
;; byte-compile-warnings: (not make-local docstrings)
;; End:
