;;; autoload/misc.el -*- lexical-binding: t; -*-

;;;###autoload
(defun insert-date (prefix)
  "Insert the current date. With prefix-argument, use ISO format.
Withtwo prefix arguments, write out the day and month name."
  (interactive "P")
  (let ((format (cond
                 ((not prefix) " %Y-%m-%d ")
                 ((equal prefix '(4)) " %A, %d. %B %Y ")
                 ((equal prefix '(16)) " %d.%m.%Y "))))
    (insert (format-time-string format))))
