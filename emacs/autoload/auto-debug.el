;;; autoload/debug.el -*- lexical-binding: t; -*-


;;; Profiling

(defvar radian--profiler nil)
;;;###autoload
(defun radian/toggle-profiler ()
  "Toggle the Emacs profiler. Run it again to see the profiling report."
  (interactive)
  (if (not radian--profiler)
      (profiler-start 'cpu+mem)
    (profiler-report)
    (profiler-stop))
  (setq radian--profiler (not radian--profiler)))
