;;; autoload/text.el; -*- lexical-binding: t; -*-

;;;###autoload
(defun radian-thing-at-point-or-region (&optional thing prompt)
  "Grab the current selection, THING at point, or xref identifier at point.

Returns THING if it is a string. Otherwise, if nothing is found at point and
PROMPT is non-nil, prompt for a string (if PROMPT is a string it'll be used as
the prompting string). Returns nil if all else fails.

NOTE: Don't use THING for grabbing symbol-at-point. The xref fallback is smarter
in some cases."
  (declare (side-effect-free t))
  (cond ((stringp thing)
         thing)
        ((use-region-p)
         (buffer-substring-no-properties
          (region-beginning)
          (region-end)))
        (thing
         (thing-at-point thing t))
        ((require 'xref nil t)
         ;; Eglot, nox (a fork of eglot), and elpy implementations for
         ;; `xref-backend-identifier-at-point' betray the documented purpose of
         ;; the interface. Eglot/nox return a hardcoded string and elpy prepends
         ;; the line number to the symbol.
         (if (memq (xref-find-backend) '(eglot elpy nox))
             (thing-at-point 'symbol t)
           ;; A little smarter than using `symbol-at-point', though in most
           ;; cases, xref ends up using `symbol-at-point' anyway.
           (xref-backend-identifier-at-point (xref-find-backend))))
        (prompt
         (read-string (if (stringp prompt) prompt "")))))

;;;###autoload
(defun radian/dos2unix ()
  "Convert the current buffer to a Unix file encoding."
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))

;;;###autoload
(defun radian/unix2dos ()
  "Convert the current buffer to a DOS file encoding."
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos nil))

;;
;;; Hooks

;;;###autoload
(defun radian-enable-delete-trailing-whitespace-h ()
  "Enables the automatic deletion of trailing whitespaces upon file save.

i.e. enables `ws-butler-mode' in the current buffer."
  (ws-butler-mode +1))

;;;###autoload
(defun radian-disable-delete-trailing-whitespace-h ()
  "Disables the automatic deletion of trailing whitespaces upon file save.

i.e. disables `ws-butler-mode' in the current buffer."
  (ws-butler-mode -1))

;;;###autoload
(defun radian-enable-show-trailing-whitespace-h ()
  "Enable `show-trailing-whitespace' in the current buffer."
  (setq-local show-trailing-whitespace t))

;;;###autoload
(defun radian-disable-show-trailing-whitespace-h ()
  "Disable `show-trailing-whitespace' in the current buffer."
  (setq-local show-trailing-whitespace nil))



;;;###autoload
(defun +default/yank-buffer-path (&optional root)
  "Copy the current buffer's path to the kill ring."
  (interactive)
  (if-let (filename (or (buffer-file-name (buffer-base-buffer))
                        (bound-and-true-p list-buffers-directory)))
      (message "Copied path to clipboard: %s"
               (kill-new (abbreviate-file-name
                          (if root
                              (file-relative-name filename root)
                            filename))))
    (error "Couldn't find filename in current buffer")))

;;;###autoload
(defun +default/yank-buffer-path-relative-to-project ()
  "Copy the current buffer's path to the kill ring."
  (interactive)
  (+default/yank-buffer-path (radian-project-root)))

;;;###autoload
(defun +default/insert-file-path (arg)
  "Insert the file name (absolute path if prefix ARG).
If `buffer-file-name' isn't set, uses `default-directory'."
  (interactive "P")
  (let ((path (or buffer-file-name default-directory)))
    (insert
     (if arg
         (abbreviate-file-name path)
       (file-name-nondirectory path)))))

;;;###autoload
(defun radian/backward-delete-whitespace-to-column ()
  "Delete back to the previous column of whitespace, or as much whitespace as
possible, or just one char if that's not possible."
  (interactive)
  (let* ((context
          (if (bound-and-true-p smartparens-mode)
              (ignore-errors (sp-get-thing))))
         (op (plist-get context :op))
         (cl (plist-get context :cl))
         open-len close-len current-column)
    (cond ;; When in strings (sp acts weird with quotes; this is the fix)
          ;; Also, skip closing delimiters
          ((and op cl
                (string= op cl)
                (and (string= (char-to-string (or (char-before) 0)) op)
                     (setq open-len (length op)))
                (and (string= (char-to-string (or (char-after) 0)) cl)
                     (setq close-len (length cl))))
           (delete-char (- open-len))
           (delete-char close-len))

          ;; Delete up to the nearest tab column IF only whitespace between
          ;; point and bol.
          ((and (not indent-tabs-mode)
                (> tab-width 1)
                (not (bolp))
                (not (radian-point-in-string-p))
                (>= (abs (save-excursion (skip-chars-backward " \t")))
                    (setq current-column (current-column))))
           (delete-char (- (1+ (% (1- current-column) tab-width)))))

          ;; Otherwise do a regular delete
          ((delete-char -1)))))

;;;###autoload
(defun +default--delete-backward-char-a (n &optional killflag)
  "Same as `delete-backward-char', but preforms these additional checks:

+ If point is surrounded by (balanced) whitespace and a brace delimiter ({} []
  ()), delete a space on either side of the cursor.
+ If point is at BOL and surrounded by braces on adjacent lines, collapse
  newlines:
  {
  |
  } => {|}
+ Otherwise, resort to `radian/backward-delete-whitespace-to-column'.
+ Resorts to `delete-char' if n > 1"
  (interactive "p\nP")
  (or (integerp n)
      (signal 'wrong-type-argument (list 'integerp n)))
  (cond ((and (use-region-p)
              delete-active-region
              (= n 1))
         ;; If a region is active, kill or delete it.
         (if (eq delete-active-region 'kill)
             (kill-region (region-beginning) (region-end) 'region)
           (funcall region-extract-function 'delete-only)))
        ;; In Overwrite mode, maybe untabify while deleting
        ((null (or (null overwrite-mode)
                   (<= n 0)
                   (memq (char-before) '(?\t ?\n))
                   (eobp)
                   (eq (char-after) ?\n)))
         (let ((ocol (current-column)))
           (delete-char (- n) killflag)
           (save-excursion
             (insert-char ?\s (- ocol (current-column)) nil))))
        ;;
        ((= n 1)
         (cond ((or (not (bound-and-true-p smartparens-mode))
                    (and (memq (char-before) (list ?\  ?\t))
                         (save-excursion
                           (and (/= (skip-chars-backward " \t" (line-beginning-position)) 0)
                                (bolp)))))
                (radian/backward-delete-whitespace-to-column))
               ((let* ((pair (ignore-errors (sp-get-thing)))
                       (op   (plist-get pair :op))
                       (cl   (plist-get pair :cl))
                       (beg  (plist-get pair :beg))
                       (end  (plist-get pair :end)))
                  (cond ((and end beg (= end (+ beg (length op) (length cl))))
                         (delete-char (- (length op))))
                        ((radian-surrounded-p pair 'inline 'balanced)
                         (delete-char -1 killflag)
                         (delete-char 1)
                         (when (= (point) (+ (length cl) beg))
                           (sp-backward-delete-char 1)
                           (sp-insert-pair op)))
                        ((and (bolp) (radian-surrounded-p pair nil 'balanced))
                         (delete-region beg end)
                         (sp-insert-pair op)
                         t)
                        ((run-hook-with-args-until-success 'radian-delete-backward-functions))
                        ((radian/backward-delete-whitespace-to-column)))))))
        ;; Otherwise, do simple deletion.
        ((delete-char (- n) killflag))))

;;; Diff two regions
;; Step 1: Select a region and `M-x diff-region-tag-selected-as-a'
;; Step 2: Select another region and `M-x diff-region-compare-with-b'
;; Press "q" in evil-mode or "C-c C-c" to exit the diff output buffer

;;;###autoload
(defun diff-region-format-region-boundary (b e)
  "Make sure lines are selected and B is less than E"
  (let* (tmp rlt)
    ;; swap b e, make sure b < e
    (when (> b e)
      (setq tmp b)
      (setq b e)
      (set e tmp))

    ;; select lines
    (save-excursion
      ;; Another workaround for evil-visual-line bug:
      ;; In evil-mode, if we use hotkey V or `M-x evil-visual-line` to select line,
      ;; the (line-beginning-position) of the line which is after the last selected
      ;; line is always (region-end)! Don't know why.
      (if (and (> e b)
               (save-excursion (goto-char e) (= e (line-beginning-position)))
               (boundp 'evil-state) (eq evil-state 'visual))
          (setq e (1- e)))
      (goto-char b)
      (setq b (line-beginning-position))
      (goto-char e)
      (setq e (line-end-position)))
    (setq rlt (list b e))
    rlt))

;;;###autoload
(defmacro diff-region-open-diff-output (content buffer-name)
  `(let ((rlt-buf (get-buffer-create ,buffer-name)))
     (save-current-buffer
       (switch-to-buffer-other-window rlt-buf)
       (set-buffer rlt-buf)
       (erase-buffer)
       (insert ,content)
       ;; `ffip-diff-mode' is more powerful than `diff-mode'
       (ffip-diff-mode)
       (goto-char (point-min)))))

;;;###autoload
(defun diff-region-tag-selected-as-a ()
  "Select a region to compare."
  (interactive)
  (when (region-active-p)
    (let* (tmp buf)
      ;; select lines
      (setq tmp (diff-region-format-region-boundary (region-beginning) (region-end)))
      (setq buf (get-buffer-create "*Diff-regionA*"))
      (save-current-buffer
        (set-buffer buf)
        (erase-buffer))
      (append-to-buffer buf (car tmp) (cadr tmp))))
  (message "Now select other region to compare and run `diff-region-compare-with-b'"))

;;;###autoload
(defun diff-region-compare-with-b ()
  "Compare current region with the region set by `diff-region-tag-selected-as-a'.
If no region is selected, `kill-ring' or clipboard is used instead."
  (interactive)
  (let* (rlt-buf
         diff-output
         tmp
         ;; file A
         (fa (make-temp-file (expand-file-name "diff-region"
                                               (or small-temporary-file-directory
                                                   temporary-file-directory))))
         ;; file B
         (fb (make-temp-file (expand-file-name "diff-region"
                                               (or small-temporary-file-directory
                                                   temporary-file-directory)))))
    (when (and fa (file-exists-p fa) fb (file-exists-p fb))
      (cond
       ((region-active-p)
        ;; text from selected region
        (setq tmp (diff-region-format-region-boundary (region-beginning) (region-end)))
        (write-region (car tmp) (cadr tmp) fb))
       (t
        ;; text from `kill-ring' or clipboard
        (let* ((choice (completing-read "Since no region selected, compare text in:"
                                        '("kill-ring" "clipboard")))
               (txt (cond
                     ((string= choice "kill-ring")
                      (car kill-ring))
                     ((string= choice "clipboard")
                      (let* ((powershell-program (executable-find "powershell.exe")))
                        (cond
                         ;; Windows
                         ((and *win64* (fboundp 'w32-get-clipboard-data))
                          ;; `w32-set-clipboard-data' makes `w32-get-clipboard-data' always return null
                          (w32-get-clipboard-data))
                         ;; Windows 10
                         (powershell-program
                          (string-trim-right
                           (with-output-to-string
                             (with-current-buffer standard-output
                               (call-process powershell-program nil t nil "-command" "Get-Clipboard")))))
                         ;; xclip can handle
                         (t (xclip-get-selection 'clipboard))))))))
          (with-temp-file fb
            (insert txt)))))
      ;; save region A as file A
      (save-current-buffer
        (set-buffer (get-buffer-create "*Diff-regionA*"))
        (write-region (point-min) (point-max) fa))
      ;; diff NOW!
      ;; show the diff output
      (cond
       ((string= (setq diff-output (shell-command-to-string (format "%s -Nabur %s %s" diff-command fa fb))) "")
        (message "Two regions are SAME!"))
       ((executable-find "git")
        (unless (featurep 'magit) (require 'magit))
        (magit-diff-setup-buffer nil (list "--no-index" "--indent-heuristic" "--histogram")
                                 nil (list (magit-convert-filename-for-git
                                            (expand-file-name fa))
                                           (magit-convert-filename-for-git
                                            (expand-file-name fb)))))
       (t
        (diff-region-open-diff-output diff-output
                                      "*Diff-region-output*")))
      ;; clean the temporary files
      (if (and fa (file-exists-p fa))
          (delete-file fa))
      (if (and fb (file-exists-p fb))
          (delete-file fb)))))
