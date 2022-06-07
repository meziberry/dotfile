;;; radian-modeline.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:

;; This is a slimmed down version of `doom-modeline' that manipulates
;; `mode-line-format' directly. Its purpose is to be truer to the original goal
;; of Doom's modeline: to be more performant and minimalistic alternative to
;; other modeline packages and to be abstraction-light. Too much abstraction is
;; too much magic.
;;
;; Warning: this is still a WIP!

;;; Code:

;;;; Default Mode-line format
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
(cl-loop with index = 0
         for e in mode-line-modes do
         (cond ((and (stringp e) (string-match-p "^\\(%\\[\\|%\\]\\)$" e))
                (setf (nth index mode-line-modes) ""))
               ;; ((equal "(" e) (setf (nth index mode-line-modes) "#"))
               ((equal ")" e) (setf (nth index mode-line-modes) ";)")))
         (setq index (1+ index)))

(defcustom radian-mode-line-left
  '((:propertize ("" mode-line-mule-info) display (min-width (5.0)))
    "%*" "%@"
    " "     ;; Show the name of the current buffer.
    mode-line-buffer-identification
    mode-line-front-space
    ;; Show the row and column of point.
    mode-line-position
    ;; Show the active major and minor modes.
    " "
    mode-line-modes
    mode-line-misc-info)
  "Composite mode line construct to be shown left-aligned."
  :type 'sexp)

(defcustom radian-mode-line-right          ;⊙_⚆⚈☭
  '("%e"
    (vc-mode vc-mode)
    " "
    (:eval (when (featurep 'meow) (meow-indicator)))
    mode-line-end-spaces)
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


(require 'all-the-icons nil t)

(defun +modeline--set-var-and-refresh-bars-fn (&optional symbol value)
  (when symbol
    (set-default symbol value))
  (when radian-init-time
    (+modeline-refresh-bars-h)))


;;
;;; Variables

(defcustom +modeline-height 18
  "The height of the modeline.

This is enforced by the xpm bitmap bar in `+modeline-bar'. Without it (and in
the terminal), this variable does nothing.

Use `setq!' to adjust this variable live, as it will trigger an refresh of the
bars in the modeline. `setq' will not."
  :type 'integer
  :set #'+modeline--set-var-and-refresh-bars-fn)

(defcustom +modeline-bar-width 3
  "The width of the bar in the modeline.

If nil, the bar will be made transparent and 1 pixel wide, as to be invisible,
but without sacrificing its ability to enforce `+modeline-height'.

Use `setq!' to adjust this variable live, as it will trigger an refresh of the
bars in the modeline. `setq' will not."
  :type 'integer
  :set #'+modeline--set-var-and-refresh-bars-fn)

(defcustom +modeline-enable-icon t
  "Enable the `all-the-icons' option."
  :set #'+modeline--set-var-and-refresh-bars-fn)

(defvar +modeline-format-alist ()
  "An alist of modeline formats defined with `def-modeline!'.

Each entry's CAR is the name and CDR is a cons cell whose CAR is the left-hand
side of the modeline, and whose CDR is the right-hand side.")


;;
;;; Faces

(defface radian-modeline-bar '((t (:inherit highlight)))
  "Face used for left-most bar on the mode-line of an active window.")

(defface radian-modeline-bar-inactive '((t (:inherit mode-line-inactive)))
  "Face used for left-most bar on the mode-line of an inactive window.")

(defface radian-modeline-highlight '((t (:inherit mode-line-highlight)))
  "Face used for highlighted modeline panels (like search counts).")

(defface radian-modeline-alternate-highlight '((t (:inherit mode-line-highlight)))
  "Alternative face used for highlighted modeline panels (like search counts).")


;;
;;; Helpers

;;; `active'
(defvar +modeline--active-window (selected-window))

(defun +modeline-icon-displayable-p ()
  "Return non-nil if icons are displayable."
  (and +modeline-enable-icon
       (display-graphic-p)
       (featurep 'all-the-icons)))

(defun +modeline-active ()
  "Return non-nil if the selected window has an active modeline."
  (eq (selected-window) +modeline--active-window))

(add-hook! 'pre-redisplay-functions
  (defun +modeline-set-selected-window-h (&rest _)
    "Track the active modeline's window in `+modeline--active-window'."
    (let ((win (selected-window)))
      (unless (minibuffer-window-active-p win)
        (setq +modeline--active-window (frame-selected-window))))))

(defun +modeline--make-xpm (color width height)
  "Create an XPM bitmap via COLOR, WIDTH and HEIGHT. Inspired by `powerline''s `pl/+modeline--make-xpm'."
  (propertize
   " " 'display
   (let ((data (make-list height (make-list width 1)))
         (color (or color "None")))
     (ignore-errors
       (create-image
        (concat
         (format "/* XPM */\nstatic char * percent[] = {\n\"%i %i 2 1\",\n\". c %s\",\n\"  c %s\","
                 (length (car data))
                 (length data)
                 color
                 color)
         (apply #'concat
                (cl-loop with idx = 0
                         with len = (length data)
                         for dl in data
                         do (cl-incf idx)
                         collect
                         (concat "\""
                                 (cl-loop for d in dl
                                          if (= d 0) collect (string-to-char " ")
                                          else collect (string-to-char "."))
                                 (if (eq idx len) "\"};" "\",\n")))))
        'xpm t :ascent 'center)))))

(defun +modeline-format-icon (icon-set icon label &optional face help-echo voffset)
  "Build from ICON-SET the ICON with LABEL.
Using optionals attributes FACE, HELP-ECHO and VOFFSET."
  (let* ((fn (intern-soft (concat "all-the-icons-" (symbol-name icon-set))))
         (str (pcase icon-set
                ('octicon "")
                ('faicon "")
                ('material "🗸")
                ('alltheicon "")
                ('fileicon "")))
         (icon-set-fn (if (+modeline-icon-displayable-p)
                          fn
                        (lambda (&rest _) (propertize str 'face face)))))
    (propertize (concat (funcall icon-set-fn
                                 icon
                                 :face face
                                 :height 1.1
                                 :v-adjust (or voffset -0.225))
                        (propertize label 'face face))
                'help-echo help-echo)))

(defun set-modeline! (name &optional default)
  "Set the modeline to NAME.
If DEFAULT is non-nil, apply to all future buffers. Modelines are defined with
`def-modeline!'."
  (if-let (format (assq name +modeline-format-alist))
      (cl-destructuring-bind (lhs . rhs) (cdr format)
        (if default
            (setq-default +modeline-format-left lhs
                          +modeline-format-right rhs)
          (setq +modeline-format-left lhs
                +modeline-format-right rhs)))
    (error "Could not find %S modeline format" name)))

(defun set-modeline-hook! (hooks name)
  "Set the modeline to NAME on HOOKS.
See `def-modeline!' on how modelines are defined."
  (let ((fn (intern (format "+modeline-set-%s-format-h" name))))
    (dolist (hook (radian-enlist hooks))
      (when after-init-time
        (dolist (name (mapcar #'car +modeline-format-alist))
          (remove-hook hook (intern (format "+modeline-set-%s-format-h" name)))))
      (add-hook hook fn))))

(defun def-modeline! (name lhs rhs)
  "Define a modeline format by NAME.
LHS and RHS are the formats representing the left and right hand side of the
mode-line, respectively. See the variable `format-mode-line' for details on what
LHS and RHS will accept."
  (setf (alist-get name +modeline-format-alist)
        (cons lhs rhs))
  (fset (intern (format "+modeline-set-%s-format-h" name))
        (lambda (&rest _) (set-modeline! name))))

(defmacro def-modeline-var! (name body &optional docstring &rest plist)
  "Define a modeline segment variable."
  (unless (stringp docstring)
    (push docstring plist)
    (setq docstring nil))
  `(progn
     (defconst ,name ,body ,docstring)
     ,@(if (plist-get plist :local) `((make-variable-buffer-local ',name)))
     (put ',name 'risky-local-variable t)))

(mapc #'(lambda (f) (put f 'lisp-indent-function 'defun))
      '(set-modeline! def-modeline! def-modeline-var!))

;;
;;; Segments

(def-modeline-var! +modeline-format-left nil
  "The left-hand side of the modeline."
  :local t)

(def-modeline-var! +modeline-format-right nil
  "The right-hand side of the modeline."
  :local t)


;;; `+modeline-bar'
(def-modeline-var! +modeline-bar "")

(defvar +modeline-active-bar "")
(defvar +modeline-inactive-bar "")

(add-hook! '(radian-init-ui-hook radian-load-theme-hook) :append
  (defun +modeline-refresh-bars-h ()
    (let ((width (or +modeline-bar-width 1))
          (height (max +modeline-height 0))
          (active-bg (face-background 'radian-modeline-bar nil t))
          (inactive-bg (face-background 'radian-modeline-bar-inactive nil t)))
      (when (or (null +modeline-bar-width)
                (= +modeline-bar-width 0))
        (setq active-bg nil
              inactive-bg nil))
      (setq +modeline-active-bar
            (+modeline--make-xpm (and +modeline-bar-width active-bg)
                                 width height)
            +modeline-inactive-bar
            (+modeline--make-xpm (and +modeline-bar-width inactive-bg)
                                 width height)
            +modeline-bar
            '(:eval (if (+modeline-active)
                        +modeline-active-bar
                      +modeline-inactive-bar))))))

(add-hook! 'after-setting-font-hook
  (defun +modeline-adjust-height-h ()
    (defvar +modeline--old-height +modeline-height)
    (let ((default-height +modeline--old-height)
          (scale (or (frame-parameter nil 'font-scale) 0)))
      (setq +modeline-height
            (if (> scale 0)
                (+ default-height (* (or (frame-parameter nil 'font-scale) 1)
                                     radian-font-increment))
              default-height))
      (when radian-init-time
        (+modeline-refresh-bars-h)))))

;;; `+modeline-matches'
(defun +modeline--overlay< (a b)
  "Sort overlay A and B."
  (< (overlay-start a) (overlay-start b)))

(defun +modeline--iedit ()
  "Show the number of iedit regions matches + what match you're on."
  (when (and (bound-and-true-p iedit-mode)
             (bound-and-true-p iedit-occurrences-overlays))
    (propertize
     (let ((this-oc (or (let ((inhibit-message t))
                          (iedit-find-current-occurrence-overlay))
                        (save-excursion
                          (iedit-prev-occurrence)
                          (iedit-find-current-occurrence-overlay))))
           (length (length iedit-occurrences-overlays)))
       (format " %s/%d "
               (if this-oc
                   (- length
                      (length (memq this-oc (sort (append iedit-occurrences-overlays nil)
                                                  #'+modeline--overlay<)))
                      -1)
                 "-")
               length))
     'face (if (+modeline-active) 'radian-modeline-highlight))))

(defun +modeline--macro-recording ()
  "Display current Emacs or evil macro being recorded."
  (when (and (+modeline-active)
             (or defining-kbd-macro
                 executing-kbd-macro))
    (let ((sep (propertize " " 'face 'radian-modeline-highlight)))
      (concat sep
              (propertize (if (bound-and-true-p evil-this-macro)
                              (char-to-string evil-this-macro)
                            "Macro")
                          'face 'radian-modeline-highlight)
              sep
              (if (+modeline-icon-displayable-p)
                  (all-the-icons-octicon "triangle-right"
                                         :face 'radian-modeline-highlight
                                         :v-adjust -0.05)
                "▶")
              sep))))

(def-modeline-var! +modeline-matches
  '(:eval
    (let ((meta (concat (+modeline--macro-recording)
                        (+modeline--iedit))))
      (or (and (not (equal meta "")) meta)
          " %I "))))                    ;file-size


;;; `+modeline-modes'
(def-modeline-var! +modeline-modes ; remove minor modes
  '(""
    minor-mode-alist
    " "
    (:propertize mode-name
                 face bold
                 mouse-face radian-modeline-highlight)
    mode-line-process
    "%n"
    " "))


;;; `+modeline-buffer-identification'
(defvar-local +modeline--buffer-id-cache nil)

;; REVIEW Generating the buffer's file name can be relatively expensive.
;;        Compounded with how often the modeline updates this can add up, so
;;        we cache it ahead of time.
(add-hook! '(change-major-mode-after-body-hook
             ;; In case the user saves the file to a new location
             after-save-hook
             ;; ...or makes external changes then returns to Emacs
             focus-in-hook
             ;; ...when the visited file changes (e.g. it's renamed)
             after-set-visited-file-name-hook
             ;; ...when the underlying file changes
             after-revert-hook)
  (defun +modeline--generate-buffer-id-cache-h ()
    (when after-init-time
      (setq +modeline--buffer-id-cache
            (let ((file-name (buffer-file-name (buffer-base-buffer))))
              (unless (or (null default-directory)
                          (null file-name)
                          (file-remote-p file-name))
                (when-let (project-root (radian-project-root))
                  (file-relative-name (or buffer-file-truename (file-truename file-name))
                                      (concat project-root "..")))))))))

(def-modeline-var! +modeline-buffer-identification ; slightly more informative buffer id
  '((:eval
     (propertize
      (or +modeline--buffer-id-cache "%b")
      'face (cond ((buffer-modified-p) '(error bold mode-line-buffer-id))
                  ((+modeline-active)  'mode-line-buffer-id))
      'help-echo (or +modeline--buffer-id-cache (buffer-name))))
    (buffer-read-only (:propertize " RO" face warning))))


;;; `+modeline-position'
(def-modeline-var! +modeline-position '("  %l:%C %p  "))


;;; `+modeline-checker'
(def-modeline-var! +modeline-checker nil
  "Displays color-coded error status & icon for the current buffer."
  :local t)

(add-hook! '(flycheck-status-changed-functions
             flycheck-mode-hook)
  (defun +modeline-checker-update (&optional status)
    "Update flycheck text via STATUS."
    (setq +modeline-checker
          (pcase status
            (`finished
             (if flycheck-current-errors
                 (let-alist (flycheck-count-errors flycheck-current-errors)
                   (let ((error (or .error 0))
                         (warning (or .warning 0))
                         (info (or .info 0)))
                     (+modeline-format-icon 'material "do_not_disturb_alt"
                                            (number-to-string (+ error warning info))
                                            (cond ((> error 0)   'error)
                                                  ((> warning 0) 'warning)
                                                  ('success))
                                            (format "Errors: %d, Warnings: %d, Debug: %d"
                                                    error
                                                    warning
                                                    info))))
               (+modeline-format-icon 'material "check" "" 'success)))
            (`running     (+modeline-format-icon 'material "access_time" "*" 'mode-line-inactive "Running..."))
            (`errored     (+modeline-format-icon 'material "sim_card_alert" "!" 'error "Errored!"))
            (`interrupted (+modeline-format-icon 'material "pause" "!" 'mode-line-inactive "Interrupted"))
            (`suspicious  (+modeline-format-icon 'material "priority_high" "!" 'error "Suspicious"))))))



;;; `+modeline-selection-info'
(defsubst +modeline--column (pos)
  "Get the column of the position `POS'."
  (save-excursion (goto-char pos)
                  (current-column)))

(def-modeline-var! +modeline-selection-info
  '(:eval
    (when (or (and (bound-and-true-p evil-local-mode)
                   (eq evil-state 'visual))
              mark-active)
      (cl-destructuring-bind (beg . end)
          (if (bound-and-true-p evil-visual-selection)
              (cons evil-visual-beginning evil-visual-end)
            (cons (region-beginning) (region-end)))
        (propertize
         (let ((lines (count-lines beg (min end (point-max)))))
           (concat " "
                   (cond ((or (bound-and-true-p rectangle-mark-mode)
                              (and (bound-and-true-p evil-visual-selection)
                                   (eq 'block evil-visual-selection)))
                          (let ((cols (abs (- (+modeline--column end)
                                              (+modeline--column beg)))))
                            (format "%dx%dB" lines cols)))
                         ((and (bound-and-true-p evil-visual-selection)
                               (eq evil-visual-selection 'line))
                          (format "%dL" lines))
                         ((> lines 1)
                          (format "%dC %dL" (- end beg) lines))
                         ((format "%dC" (- end beg))))
                   (when (derived-mode-p 'text-mode)
                     (format " %dW" (count-words beg end)))
                   " "))
         'face (if (+modeline-active) 'success)))))
  "Information about the current selection, such as how many characters and
lines are selected, or the NxM dimensions of a block selection.")

(defun +modeline-add-selection-segment-h ()
  (add-to-list '+modeline-format-left '+modeline-selection-info 'append))
(defun +modeline-remove-selection-segment-h ()
  (delq! '+modeline-selection-info +modeline-format-left))

(add-hook 'activate-mark-hook #'+modeline-add-selection-segment-h)
(add-hook 'deactivate-mark-hook #'+modeline-remove-selection-segment-h)

;;; `+modeline-encoding'
(def-modeline-var! +modeline-encoding
  `(:eval
    (let ((sys (coding-system-plist buffer-file-coding-system))
          (eol (coding-system-eol-type-mnemonic buffer-file-coding-system)))
      (concat (unless (equal eol ,(if *WINDOWS "CRLF" "LF"))
                (concat "  " eol " "))
              (if (memq (plist-get sys :category)
                        '(coding-category-undecided coding-category-utf-8))
                  (unless (string-match-p "utf-8" (symbol-name buffer-file-coding-system))
                    "UTF-8  ")
                (concat (upcase (symbol-name (plist-get sys :name)))
                        "  "))))))

(def-modeline-var! +modeline-pdf-page nil
  "Display page number of pdf"
  :local t)

(defun +modeline-update-pdf-pages ()
  "Update PDF pages."
  (setq +modeline-pdf-page
        (format "  P%d/%d "
                (eval `(pdf-view-current-page))
                (pdf-cache-number-of-pages))))

(add-hook 'pdf-view-change-page-hook #'+modeline-update-pdf-pages)

;; Clearer mnemonic labels for EOL styles
(setq eol-mnemonic-dos "CRLF"
      eol-mnemonic-mac "CR"
      eol-mnemonic-unix "LF"
      eol-mnemonic-undecided "??")

;;; `+modeline-meow'
(def-modeline-var! +modeline-meow
  '(:eval (if (featurep 'meow) (string-trim (meow-indicator)))))

;;
;;; Default modeline

(def-modeline! :main
  '(""
    +modeline-matches
    " "
    +modeline-buffer-identification
    +modeline-position)
  `(""
    mode-line-misc-info
    +modeline-modes
    (vc-mode ("  "
              ,(if (+modeline-icon-displayable-p)
                   (all-the-icons-octicon "git-branch" :v-adjust 0.0)
                 "")
              vc-mode))
    "  "
    +modeline-encoding
    (+modeline-checker ("" +modeline-checker " "))
    +modeline-meow))

(def-modeline! 'project
  `(" "
    ,(if (+modeline-icon-displayable-p)
         (all-the-icons-octicon
          "file-directory"
          :face 'bold
          :v-adjust -0.06
          :height 1.1)
       "")
    (:propertize (" " (:eval (abbreviate-file-name default-directory)))
                 face bold))
  '("" mode-line-misc-info +modeline-modes +modeline-meow))

(def-modeline! 'special
  '("" +modeline-matches
    " " +modeline-buffer-identification)
  '("" +modeline-modes +modeline-meow))

(def-modeline! 'pdf
  '(""
    +modeline-matches
    " "
    +modeline-buffer-identification
    +modeline-pdf-page)
  `(""
    +modeline-modes
    " " +modeline-meow))
;; TODO (def-modeline! helm ...)


;; Other modes
(set-modeline! :main 'default)
(set-modeline-hook! 'pdf-tools-enabled-hook 'pdf)
(set-modeline-hook! '(special-mode-hook
                      image-mode-hook
                      circe-mode-hook)
                    'special)

(add-hook! 'magit-mode-hook
  (defun +modeline-init-project-or-hide-h ()
    (if (eq major-mode 'magit-status-mode)
        (set-modeline! 'project)
      (hide-mode-line-mode +1))))


;;
;;; Bootstrap

(if (featurep! '+bottom)
    (progn
      (defvar +modeline--old-format (default-value 'mode-line-format))
      (define-minor-mode +modeline-mode
        "TODO"
        :init-value nil
        :global nil
        (cond
         (+modeline-mode
          (setq mode-line-format
                (cons
                 "" '(+modeline-bar
                      +modeline-format-left
                      (:eval
                       (propertize
                        " "
                        'display
                        `((space :align-to (- (+ right right-fringe right-margin)
                                              ,(string-width
                                                (format-mode-line '("" +modeline-format-right))))))))
                      +modeline-format-right))))
         ((setq mode-line-format +modeline--old-format))))
      (define-global-minor-mode +modeline-global-mode +modeline-mode +modeline-mode)
      (add-hook '+modeline-global-mode-hook #'size-indication-mode))
  (progn
    (defvar +headerline--old-format (default-value 'header-line-format))
    (setq-default mode-line-format nil)
    (define-minor-mode +headerline-mode
      "TODO"
      :init-value nil
      :global nil
      (cond
       (+headerline-mode
        (setq header-line-format
              (cons
               "" '(+modeline-bar
                    +modeline-format-left
                    (:eval
                     (propertize
                      " "
                      'display
                      `((space :align-to (- (+ right right-fringe right-margin)
                                            ,(string-width
                                              (format-mode-line '("" +modeline-format-right))))))))
                    +modeline-format-right))))
       ((setq header-line-format +headerline--old-format))))
    (define-global-minor-mode +headerline-global-mode +headerline-mode +headerline-mode)
    (add-hook '+headerline-global-mode-hook #'size-indication-mode)
    (global-hide-mode-line-mode +1)))

(provide 'radian-modeline)
;;; radian-modeline.el ends here
