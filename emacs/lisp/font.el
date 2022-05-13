;; conf-font.el --- -*- coding: utf-8; lexical-binding: t -*-

(defvar radian-font (font-spec :family "Microsoft Yahei")
  "The default font to use.
Must be a `font-spec', a font object, an XFT font string, or an XLFD string.

This affects the `default' and `fixed-pitch' faces.

The properties except :family are all ignored.

Examples:
  (setq radian-font (font-spec :family \"Fira Mono\"))
  (setq radian-font \"Terminus (TTF):antialias=off\")")

(defvar radian-font-size 98 "The `height' property of radian fontset ")

(defvar radian-variable-pitch-font nil
  "The default font to use for variable-pitch text.
Must be a `font-spec', a font object, an XFT font string, or an XLFD string. See
`radian-font' for examples.

An omitted font size means to inherit `radian-font''s size.")

(defvar radian-serif-font nil
  "The default font to use for the `fixed-pitch-serif' face.
Must be a `font-spec', a font object, an XFT font string, or an XLFD string. See
`radian-font' for examples.

An omitted font size means to inherit `radian-font''s size.")

(defvar radian-unicode-font nil
  "Fallback font for Unicode glyphs.
Must be a `font-spec', a font object, an XFT font string, or an XLFD string. See
`radian-font' for examples.

The defaults on macOS and Linux are Apple Color Emoji and Symbola, respectively.

WARNING: if you specify a size for this font it will hard-lock any usage of this
font to that size. It's rarely a good idea to do so!")

(defvar radian-emoji-fallback-font-families
  '("Apple Color Emoji"
    "Segoe UI Emoji"
    "Noto Color Emoji"
    "Noto Emoji")
  "A list of fallback font families to use for emojis.")

(defvar radian-symbol-fallback-font-families
  '("Symbola"
    "Segoe UI Symbol"
    "Apple Symbols")
  "A list of fallback font families for general symbol glyphs.")

(defvar radian-cjk-fallback-font-families
  '("Zpix"
    "WenQuanYi Micro Hei Mono"
    "Microsoft YaHei")
  "A list of fallback font families to use for emojis.")

;; I avoid `set-frame-font' at startup because it is expensive; doing extra,
;; unnecessary work we can avoid by setting the frame parameter directly.
(setf (alist-get 'font default-frame-alist)
      (cond ((stringp radian-font)
             (format "-*-%s-*-*-*-*-*-%s-*-*-*-*-*-*" radian-font radian-font-size))
            ((fontp radian-font)
             (format "-*-%s-*-*-*-*-*-%s-*-*-*-*-*-*"
                     (font-get radian-font :family) radian-font-size))
            ((signal 'wrong-type-argument (list '(fontp stringp) radian-font)))))
;;  FIXME: During initialize. this way cannot chang the font
;; height. I don't know why.
(set-face-attribute 'default nil :height radian-font-size)

(defun radian--make-font-specs (face font &optional base-specs)
  (ignore font base-specs)
  (let* ((base-specs (cadr (assq 'user (get face 'theme-face))))
         (base-specs (or base-specs '((t nil))))
         (attrs '(:family :foundry :slant :weight :height :width))
         (new-specs nil))
    (dolist (spec base-specs)
      ;; Each SPEC has the form (DISPLAY ATTRIBUTE-PLIST)
      (let ((display (car spec))
            (plist   (copy-tree (nth 1 spec))))
        ;; Alter only DISPLAY conditions matching this frame.
        (when (or (memq display '(t default))
                  (face-spec-set-match-display display nil))
          (dolist (attr attrs)
            (setq plist (plist-put plist attr (face-attribute face attr)))))
        (push (list display plist) new-specs)))
    (nreverse new-specs)))

(defun radian-init-font-h (&optional reload)
  "Loads `fonts'"
  (if (or reload (daemonp)) (set-frame-font radian-font t t t))

  (dolist (map `((default . ,radian-font)
                 (fixed-pitch . ,radian-font)
                 (fixed-pitch-serif . ,radian-serif-font)
                 (variable-pitch . ,radian-variable-pitch-font)))
    (when-let* ((face (car map))
                (font (cdr map)))
      (dolist (frame (frame-list))
        (when (display-multi-font-p frame)
          (set-face-attribute face frame
                              :width 'normal :weight 'normal
                              :slant 'normal :font font)))
      (let ((new-specs (radian--make-font-specs face font)))
        ;; Don't save to `customized-face' so it's omitted from `custom-file'
        ;;(put face 'customized-face new-specs)
        (custom-push-theme 'theme-face face 'user 'set new-specs)
        (put face 'face-modified nil))))

  (and
   (fboundp 'set-fontset-font)
   (let ((fn (apply-partially (lambda (font) (find-font (font-spec :name font))))))
     (when-let (font (cl-find-if fn radian-symbol-fallback-font-families))
       (set-fontset-font t 'symbol font))
     (when-let (font (cl-find-if fn radian-emoji-fallback-font-families))
       (eval-when! *EMACS28+ (set-fontset-font t 'emoji font nil 'append)))
     (when radian-unicode-font
       (set-fontset-font t 'unicode radian-unicode-font nil 'append))
     (when-let (font (cl-find-if fn radian-cjk-fallback-font-families))
       ;; Set CJK font.
       ;; (set-fontset-font t '(#x4e00 . #x9fff) font nil 'prepend)
       (dolist (script '(kana han cjk-misc bopomofo))
         (set-fontset-font (frame-parameter nil 'font) script font))))))
