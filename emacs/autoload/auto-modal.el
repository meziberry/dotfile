;;; autoload/modal.el; -*- lexical-binding: t; -*-

(defun xah-delete-blank-lines ()
  "Delete all newline around cursor.
URL `http://xahlee.info/emacs/emacs/emacs_shrink_whitespace.html'
Version 2018-04-02"
  (interactive)
  (let ($p3 $p4)
    (skip-chars-backward "\n")
    (setq $p3 (point))
    (skip-chars-forward "\n")
    (setq $p4 (point))
    (delete-region $p3 $p4)))

(defun xah-fly-delete-spaces ()
  "Delete space, tab, IDEOGRAPHIC SPACE (U+3000) around cursor.
Version 2019-06-13"
  (interactive)
  (let (p1 p2)
    (skip-chars-forward " \t　")
    (setq p2 (point))
    (skip-chars-backward " \t　")
    (setq p1 (point))
    (delete-region p1 p2)))

;;;###autoload
(defun xah-shrink-whitespaces ()
  "Remove whitespaces around cursor .

Shrink neighboring space, then newline, then space again, leaving
one space or newline at each step, TILL no more white space.

URL `http://xahlee.info/emacs/emacs/emacs_shrink_whitespace.html'
Version 2014-10-21 2021-11-26 2021-11-30"
  (interactive)
  (let* (($eol-count 0)
         ($p0 (point))
         $p1 ; whitespace begin
         $p2 ; whitespace end
         ($charBefore (char-before))
         ($charAfter (char-after))
         ($space-neighbor-p (or (eq $charBefore 32) (eq $charBefore 9) (eq $charAfter 32) (eq $charAfter 9))))
    (skip-chars-backward " \n\t　")
    (setq $p1 (point))
    (goto-char $p0)
    (skip-chars-forward " \n\t　")
    (setq $p2 (point))
    (goto-char $p1)
    (while (search-forward "\n" $p2 t)
      (setq $eol-count (1+ $eol-count)))
    (goto-char $p0)
    (cond
     ((region-active-p) (cua-delete-region))
     ((eq $p2 $p0) (delete-char 1))
     ((eq $eol-count 0)
      (if (> (- $p2 $p1) 1)
          (progn (delete-horizontal-space) (insert ?\s))
        (progn (delete-horizontal-space))))
     ((eq $eol-count 1)
      (if $space-neighbor-p
          (xah-fly-delete-spaces)
        (progn (xah-delete-blank-lines) (insert ?\s))))
     ((eq $eol-count 2)
      (if $space-neighbor-p
          (xah-fly-delete-spaces)
        (progn (xah-delete-blank-lines) (insert "\n"))))
     ((> $eol-count 2)
      (if $space-neighbor-p
          (xah-fly-delete-spaces)
        (progn
          (goto-char $p2)
          (search-backward "\n")
          (delete-region $p1 (point))
          (insert "\n"))))
     (t (progn
          (message "nothing done. logic error 40873. shouldn't reach here"))))))

;;;###autoload
(defun consult-line-or-isearch ()
  "Use `consult-line' for little file and `isearch' for larger one"
  (interactive)
  (letf!
    (defun consult-line-or-visit-p ()
      (<= (buffer-size)
          (/ 300000 (if (eq major-mode 'org-mode) 2 1))))
    (call-interactively (if (and (fboundp 'consult-line) (consult-line-or-visit-p))
                            #'consult-line
                          #'isearch-forward))))

(defcustom radian-simple-insert-pair-alist
  '(("' Single quote"        . (39 39))     ; ' '
    ("\" Double quotes"      . (34 34))     ; " "
    ("` Elisp quote"         . (96 39))     ; ` '
    ("‘ Single apostrophe"   . (8216 8217)) ; ‘ ’
    ("“ Double apostrophes"  . (8220 8221)) ; “ ”
    ("( Parentheses"         . (40 41))     ; ( )
    ("{ Curly brackets"      . (123 125))   ; { }
    ("[ Square brackets"     . (91 93))     ; [ ]
    ("< Angled brackets"     . (60 62))     ; < >
    ("« Double angled quote" . (171 187))   ; « »
    ("= Equals signs"        . (61 61))     ; = =
    ("~ Tilde"               . (126 126))   ; ~ ~
    ("* Asterisks"           . (42 42))     ; * *
    ("/ Forward Slash"       . (47 47))     ; / /
    ("_ underscores"         . (95 95)))    ; _ _
  "Alist of pairs for radian-simple-insert-pair"
  :type 'alist
  :group 'simple)
(defvar radian-simple--character-hist '()
  "History of inputs for `radian-simple-insert-pair")

(defun radian-simple--character-prompt (chars)
  "Helper of `radian-simple-insert-pair' to read CHARS."
  (let ((def (car radian-simple--character-hist)))
    (completing-read
     (format "Select character [%s]: " def)
     chars nil t nil 'radian-simple--character-hist def)))

;;;###autoload
(defun radian-simple-insert-pair (pair &optional count)
  "Insert PAIR from `radian-simple-insert-pair-alist'.
Operate on the symbol at point.  If the region is active, use it
instead.

With optional COUNT (either as a natural number from Lisp or a
universal prefix argument (\\[universal-argument]) when used
interactively) prompt for the number of delimiters to insert."
  (interactive
   (list (radian-simple--character-prompt
          radian-simple-insert-pair-alist)
         current-prefix-arg))
  (let* ((data radian-simple-insert-pair-alist)
         (left (cadr (assoc pair data)))
         (right (caddr (assoc pair data)))
         (n (cond
             ((and count (natnump count))
              count)
             (count
              (read-number "How many delimiters?" 2))
             (1)))
         (beg)
         (end))
    (cond
     ((region-active-p)
      (setq beg (region-beginning)
            end (region-end)))
     ((when (thing-at-point 'symbol)
        (let ((bounds (bounds-of-thing-at-point 'symbol)))
          (setq beg (car bounds)
                end (cdr bounds)))))
     (t (setq beg (point)
              end (point))))
    (save-excursion
      (goto-char end)
      (dotimes (_ n)
        (insert right))
      (goto-char beg)
      (dotimes (_ n)
        (insert left)))))
