;;; autoload/auto-roam.el -*- lexical-binding: t; -*-

;;; Custom node accessors
;;;###autoload (autoload 'org-roam-node-radian-filetitle "auto-roam" nil t)
(cl-defmethod org-roam-node-radian-filetitle ((node org-roam-node))
  "Return the value of \"#+title:\" (if any) from file that NODE resides in.
If there's no file-level title in the file, return empty string."
  (or (if (= (org-roam-node-level node) 0)
          (org-roam-node-title node)
        (org-roam-get-keyword "TITLE" (org-roam-node-file node)))
      ""))

;;;###autoload (autoload 'org-roam-node-radian-hierarchy "auto-roam" nil t)
(cl-defmethod org-roam-node-radian-hierarchy ((node org-roam-node))
  "Return hierarchy for NODE, constructed of its file title, OLP and direct title.
If some elements are missing, they will be stripped out."
  (let ((title     (org-roam-node-title node))
        (olp       (org-roam-node-olp   node))
        (level     (org-roam-node-level node))
        (filetitle (org-roam-node-radian-filetitle node))
        (separator (propertize " > " 'face 'shadow)))
    (cl-case level
      ;; node is a top-level file
      (0 filetitle)
      ;; node is a level 1 heading
      (1 (concat (propertize filetitle 'face '(shadow italic))
                 separator title))
      ;; node is a heading with an arbitrary outline path
      (t (concat (propertize filetitle 'face '(shadow italic))
                 separator (propertize (string-join olp " > ") 'face '(shadow italic))
                 separator title)))))

;;;###autoload (autoload 'org-roam-node-radian-subdirs "auto-roam" nil t)
(cl-defmethod org-roam-node-radian-subdirs ((node org-roam-node))
  "Return subdirectories of `org-roam-directory' in which NODE resides in.
If there's none, return an empty string."
  (if-let ((dirs (thread-first node
                   (org-roam-node-file)
                   (file-relative-name org-roam-directory)
                   (file-name-directory))))
      dirs
    ""))

;;;###autoload (autoload 'org-roam-node-radian-tags "auto-roam" nil t)
(cl-defmethod org-roam-node-radian-tags ((node org-roam-node))
  "Return tags formatted in the same way how they appear in org files.
Treat subdirectories as tags too. If there's no elements to build
the tags of, return an empty string."
  (let ((tags (org-roam-node-tags node))
        (subdirs (org-roam-node-radian-subdirs node)))
    (when tags
      (setq tags (propertize (concat (mapconcat (lambda (s) (concat ":" s)) tags nil) ":")
                             'face 'shadow)))
    (unless (string-empty-p subdirs)
      (setq subdirs (propertize (concat ":" (replace-regexp-in-string "/\\|\\\\" ":" subdirs))
                                'face '(shadow italic))))
    (replace-regexp-in-string ":+" (propertize ":" 'face 'shadow) (concat subdirs tags))))
