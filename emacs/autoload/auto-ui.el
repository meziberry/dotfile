;;; autoload/ui.el -*- lexical-binding: t; -*-

;;
;;; Public library

;;;###autoload
(defun radian-resize-window (window new-size &optional horizontal force-p)
  "Resize a window to NEW-SIZE. If HORIZONTAL, do it width-wise.
If FORCE-P is omitted when `window-size-fixed' is non-nil, resizing will fail."
  (with-selected-window (or window (selected-window))
    (let ((window-size-fixed (unless force-p window-size-fixed)))
      (enlarge-window (- new-size (if horizontal (window-width) (window-height)))
                      horizontal))))

;;;###autoload
(defun radian-quit-p (&optional prompt)
  "Prompt the user for confirmation when killing Emacs.

Returns t if it is safe to kill this session. Does not prompt if no real buffers
are open."
  (or (not (ignore-errors (radian-real-buffer-list)))
      (yes-or-no-p (format "%s" (or prompt "Really quit Emacs?")))
      (ignore (message "Aborted"))))


;;
;;; Advice

;;;###autoload
(defun radian-recenter-a (&rest _)
  "Generic advice for recentering window (typically :after other functions)."
  (recenter))

;;;###autoload
(defun radian-preserve-window-position-a (fn &rest args)
  "Generic advice for preserving cursor position on screen after scrolling."
  (let ((row (cdr (posn-col-row (posn-at-point)))))
    (prog1 (apply fn args)
      (save-excursion
        (let ((target-row (- (line-number-at-pos) row)))
          (unless (< target-row 0)
            (evil-scroll-line-to-top target-row)))))))

;;;###autoload
(defun radian-shut-up-a (fn &rest args)
  "Generic advisor for silencing noisy functions.

In interactive Emacs, this just inhibits messages from appearing in the
minibuffer. They are still logged to *Messages*.

In tty Emacs, messages are suppressed completely."
  (quiet! (apply fn args)))


;;
;;; Hooks

;;;###autoload
(defun radian-apply-ansi-color-to-compilation-buffer-h ()
  "Applies ansi codes to the compilation buffers. Meant for
`compilation-filter-hook'."
  (with-silent-modifications
    (ansi-color-apply-on-region compilation-filter-start (point))))

;;;###autoload
(defun radian-disable-show-paren-mode-h ()
  "Turn off `show-paren-mode' buffer-locally."
  (setq-local show-paren-mode nil))

;;;###autoload
(defun radian-enable-line-numbers-h ()
  (display-line-numbers-mode +1))

;;;###autoload
(defun radian-disable-line-numbers-h ()
  (display-line-numbers-mode -1))
