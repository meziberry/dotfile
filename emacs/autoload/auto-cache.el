;;; autoload/auto-cache.el --- -*- lexical-binding: t; -*-

;; This little library abstracts the process of writing arbitrary elisp values
;; to a 2-tiered file store (in `radian-store-dir'/`radian-store-location').

(defvar radian-store-dir (concat *cache/* "store/")
  "Directory to look for and store data accessed through this API.")

(defvar radian-store-persist-alist ()
  "An alist of alists, containing lists of variables for the radian cache library
to persist across Emacs sessions.")

(defvar radian-store-location "default"
  "The default location for cache files. This symbol is translated into a file
name under `pcache-directory' (by default a subdirectory under
`radian-store-dir'). One file may contain multiple cache entries.")

(defvar radian--store-table (make-hash-table :test 'equal))

(defun radian-save-persistent-store-h ()
  "Hook to persist `radian-store's storage when Emacs is killed."
  (let (locations)
    ;; Persist `radian-store-persist-alist'
    (dolist (alist (butlast radian-store-persist-alist 1))
      (cl-loop with location = (car alist)
               for var in (cdr alist)
               do (radian-store-put var (symbol-value var) nil location 'noflush)
               and do (cl-pushnew location locations :test #'equal)))
    ;; Clean up expired entries,
    (dolist (location (directory-files radian-store-dir t "^[^\\.]"))
      (maphash (lambda (key val)
                 (when (radian--store-expired-p key val)
                   (cl-pushnew location locations :test #'equal)
                   (radian--store-rem key location 'noflush)))
               (radian--store-init location)))
    (mapc #'radian--store-flush locations)))
(add-hook 'kill-emacs-hook #'radian-save-persistent-store-h)


;;
;;; Library

;;;###autoload
(defun radian-store-persist (location variables)
  "Persist VARIABLES (list of symbols) in LOCATION (symbol).
This populates these variables with cached values, if one exists, and saves them
to file when Emacs quits. This cannot persist buffer-local variables."
  (cl-check-type location string)
  (dolist (var variables)
    (when (radian-store-member-p var location)
      (set var (radian-store-get var location))))
  (setf (alist-get location radian-store-persist-alist)
        (append variables (alist-get location radian-store-persist-alist))))

;;;###autoload
(defun radian-store-desist (location &optional variables)
  "Unregisters VARIABLES (list of symbols) in LOCATION (symbol).
Variables to persist are recorded in `radian-store-persist-alist'. Does not affect
the actual variables themselves or their values."
  (cl-check-type location string)
  (if variables
      (setf (alist-get location radian-store-persist-alist)
            (cl-set-difference (cdr (assq location radian-store-persist-alist))
                               variables))
    (delq! location radian-store-persist-alist 'assoc)))

(defun radian--store-init (&optional location)
  (cl-check-type location (or null string))
  (let ((location (or location radian-store-location)))
    (or (gethash location radian--store-table)
        (let* ((file-name-handler-alist nil)
               (location-path (expand-file-name location radian-store-dir)))
          (if (file-exists-p location-path)
              (puthash location
                       (with-temp-buffer
                         (set-buffer-multibyte nil)
                         (setq buffer-file-coding-system 'binary)
                         (insert-file-contents-literally location-path)
                         (read (current-buffer)))
                       radian--store-table)
            (puthash location (make-hash-table :test 'equal)
                     radian--store-table))))))

(defun radian--store-expired-p (key data)
  (let ((ttl (car data)))
    (cond ((functionp ttl)
           (not (funcall ttl key data)))
          ((consp ttl)
           (time-less-p ttl (current-time))))))

(defun radian--store-flush (location)
  "Write `radian--store-table' to `radian-store-dir'."
  (let ((file-name-handler-alist nil)
        (coding-system-for-write 'binary)
        (write-region-annotate-functions nil)
        (write-region-post-annotation-function nil))
    (let* ((location (or location radian-store-location))
           (data (radian--store-init location)))
      (make-directory radian-store-dir 'parents)
      (with-temp-file (expand-file-name location radian-store-dir)
        (prin1 data (current-buffer)))
      data)))


;;;###autoload
(defun radian-store-get (key &optional location default-value noflush)
  "Retrieve KEY from LOCATION (defaults to `radian-store-location').
If it doesn't exist or has expired, DEFAULT_VALUE is returned."
  (let ((data (gethash key (radian--store-init location) default-value)))
    (if (not (or (eq data default-value)
                 (radian--store-expired-p key data)))
        (cdr data)
      (radian-store-rem key location noflush)
      default-value)))

;;;###autoload
(defun radian-store-put (key value &optional ttl location noflush)
  "Set KEY to VALUE in the store at LOCATION.
KEY can be any lisp object that is comparable with `equal'. TTL is the duration
(in seconds) after which this cache entry expires; if nil, no cache expiration.
LOCATION is the super-key to store this cache item under. It defaults to
`radian-store-location'."
  (cl-check-type ttl (or null integer function))
  (puthash key (cons (if (integerp ttl)
                         (time-add (current-time) ttl)
                       ttl)
                     value)
           (radian--store-init location))
  (unless noflush
    (radian--store-flush location)))

;;;###autoload
(defun radian-store-rem (key &optional location noflush)
  "Clear a cache LOCATION (defaults to `radian-store-location')."
  (remhash key (radian--store-init location))
  (unless noflush
    (radian--store-flush location)))

;;;###autoload
(defun radian-store-member-p (key &optional location)
  "Return t if KEY in LOCATION exists.
LOCATION defaults to `radian-store-location'."
  (let ((nil-value (format "--nilvalue%s--" (current-time))))
    (not (equal (radian-store-get key location nil-value)
                nil-value))))

;;;###autoload
(defun radian-store-clear (&optional location)
  "Clear the store at LOCATION (defaults to `radian-store-location')."
  (let* ((location (or location radian-store-location))
         (path (expand-file-name location radian-store-dir)))
    (remhash location radian--store-table)
    (when (file-exists-p path)
      (delete-file path)
      t)))
