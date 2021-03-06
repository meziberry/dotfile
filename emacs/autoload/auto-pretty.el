;;; autoload/auto-pretty.el; -*- lexical-binding: t; -*-

;;;###autoload
(defvar +ligatures-extra-symbols
  `(;; Functional
    :lambda        "๐"
    :def           "๐"          ;๐ ๐ค ฦ
    :composition   "โ"
    :map           "โฆ"
    ;; Types
    :null          "โ"          ;๐ฅใ
    :true          "๐"
    :false         "๐ฝ"
    :int           "โค"
    :float         "โ"
    :str           "๐"
    :bool          "๐น"
    :list          "๐"
    ;; Flow
    :not           "๏ฟข"
    :in            "โ"
    :not-in        "โ"
    :and           "โง"
    :or            "โจ"
    :for           "โ"
    :some          "โ"
    :return        "โ"
    :yield         "๐กจ" ;โป
    ;; Other
    :delta         "๐"
    :epsilon       "๐"
    :union         "โ"
    :intersect     "โฉ"
    :diff          "โ"
    :tuple         "โจ"
    :pipe          "โฉ"
    :dot           "โข"
    ;; org
    :name          "โ"
    :src_block     "ยป"
    :src_block_end "ยซ"
    :quote         "โฎ"
    :quote_end     "โฏ"
    ;;extra
    :checkbox      "โ"
    :pending       "โ"
    :checkedbox    "โ"
    :list_property "โท"
    :em_dash       "โ"
    :ellipses      "โฆ"
    :title         "๐"
    :subtitle      "๐ฉ"
    :author        "๐ผ"
    :date          "๐ฟ"
    :property      "โธ"
    :options       "โฅ"
    :latex_class   "๐ฒ"
    :latex_header  "โฅ"
    :beamer_header "โ "
    :attr_latex    "๐"
    :attr_html     "๐"
    :caption       "โฐ"
    :header        "โบ"
    :results       "๐ ถ"
    :begin_export  "โฏฎ"
    :end_export    "โฏฌ"
    :properties    "โ"
    :end           "โ"
    :priority_a   ,(propertize "โ" 'face 'org-habit-overdue-face)
    :priority_b   ,(propertize "โฌ" 'face 'org-habit-overdue-future-face)
    :priority_c   ,(propertize "โ " 'face 'org-habit-alert-face)
    :priority_d   ,(propertize "โฌ" 'face 'org-habit-ready-face)
    :priority_e   ,(propertize "โ" 'face 'org-habit-clear-future-face))
  "Maps identifiers to symbols, recognized by `set-ligatures'.

This should not contain any symbols from the Unicode Private Area! There is no
universal way of getting the correct symbol as that area varies from font to
font. See https://unicodelookup.com for more.")

;;;###autoload
(defun set-ligatures! (modes &rest plist)
  "Associates string patterns with icons in certain major-modes.

  MODES is a major mode symbol or a list of them.
  PLIST is a property list whose keys must match keys in
`+ligatures-extra-symbols', and whose values are strings representing the text
to be replaced with that symbol. If the car of PLIST is nil, then unset any
pretty symbols previously defined for MODES.

This function accepts one special property:

  :alist ALIST
    Appends ALIST to `prettify-symbols-alist' literally, without mapping text to
    `+ligatures-extra-symbols'.

For example, the rule for emacs-lisp-mode is very simple:

  (set-ligatures! 'emacs-lisp-mode
    :lambda \"lambda\")

This will replace any instances of \"lambda\" in emacs-lisp-mode with the symbol
assicated with :lambda in `+ligatures-extra-symbols'.

Pretty symbols can be unset for emacs-lisp-mode with:

  (set-ligatures! 'emacs-lisp-mode nil)"
  (declare (indent defun))
  (if (null (car-safe plist))
      (dolist (mode (ensure-list modes))
        (delq! mode +ligatures-extra-alist 'assq))
    (let (results)
      (while plist
        (let ((key (pop plist)))
          (if (eq key :alist)
              (prependq! results (pop plist))
            (when-let (char (plist-get +ligatures-extra-symbols key))
              (push (cons (pop plist) char) results)))))
      (dolist (mode (ensure-list modes))
        (setf (alist-get mode +ligatures-extra-alist)
              (if-let (old-results (alist-get mode +ligatures-extra-alist))
                  (dolist (cell results old-results)
                    (setf (alist-get (car cell) old-results) (cdr cell)))
                results))))))
