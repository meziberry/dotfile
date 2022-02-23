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

(defun consult-line-or-visit-p ()
  (<= (buffer-size)
      (/ 300000 (if (eq major-mode 'org-mode) 2 1))))

;;;###autoload
(defun consult-line-or-isearch ()
  "Use `consult-line' for little file and `isearch' for larger one"
  (interactive)
  (call-interactively (if (and (fboundp 'consult-line) (consult-line-or-visit-p))
                          #'consult-line
                        #'isearch-forward)))
