;;; autoload/auto-pretty.el; -*- lexical-binding: t; -*-

;;;###autoload
(defvar +ligatures-extra-symbols
  `(;; Functional
    :lambda        "𝝀"
    :def           "𝞒"          ;𝟊 𝛤 ƒ
    :composition   "∘"
    :map           "↦"
    ;; Types
    :null          "∅"          ;𝞥〆
    :true          "𝕋"
    :false         "𝔽"
    :int           "ℤ"
    :float         "ℝ"
    :str           "𝕊"
    :bool          "𝔹"
    :list          "𝕃"
    ;; Flow
    :not           "￢"
    :in            "∈"
    :not-in        "∉"
    :and           "∧"
    :or            "∨"
    :for           "∀"
    :some          "∃"
    :return        "⌁"
    :yield         "🡨" ;⟻
    ;; Other
    :delta         "𝞓"
    :epsilon       "𝞔"
    :union         "⋃"
    :intersect     "∩"
    :diff          "∖"
    :tuple         "⨂"
    :pipe          "⏩"
    :dot           "•"
    ;; org
    :name          "⁍"
    :src_block     "»"
    :src_block_end "«"
    :quote         "❮"
    :quote_end     "❯"
    ;;extra
    :checkbox      "☐"
    :pending       "⛞"
    :checkedbox    "☑"
    :list_property "∷"
    :em_dash       "—"
    :ellipses      "…"
    :title         "𝙏"
    :subtitle      "𝙩"
    :author        "𝘼"
    :date          "𝘿"
    :property      "☸"
    :options       "⌥"
    :latex_class   "🄲"
    :latex_header  "⇥"
    :beamer_header "↠"
    :attr_latex    "🄛"
    :attr_html     "🄗"
    :caption       "☰"
    :header        "›"
    :results       "🠶"
    :begin_export  "⯮"
    :end_export    "⯬"
    :properties    "⚙"
    :end           "∎"
    :priority_a   ,(propertize "⚑" 'face 'org-habit-overdue-face)
    :priority_b   ,(propertize "⬆" 'face 'org-habit-overdue-future-face)
    :priority_c   ,(propertize "■" 'face 'org-habit-alert-face)
    :priority_d   ,(propertize "⬇" 'face 'org-habit-ready-face)
    :priority_e   ,(propertize "❓" 'face 'org-habit-clear-future-face))
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
