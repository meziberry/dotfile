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

(defun radian-init-font-h (&optional reload)
  "Loads `fonts'"
  (if (or reload (daemonp)) (set-frame-font radian-font t t t))

  (apply #'custom-set-faces
         (let ((attrs '(:weight unspecified :slant unspecified :width unspecified)))
           (append (when radian-font
                     `((fixed-pitch ((t (:font ,radian-font ,@attrs))))))
                   (when radian-serif-font
                     `((fixed-pitch-serif ((t (:font ,radian-serif-font ,@attrs))))))
                   (when radian-variable-pitch-font
                     `((variable-pitch ((t (:font ,radian-variable-pitch-font ,@attrs)))))))))
  ;; Never save these settings to `custom-file'
  (dolist (sym '(fixed-pitch fixed-pitch-serif variable-pitch))
    (put sym 'saved-face nil))

  (when (fboundp 'set-fontset-font)
    (let ((fn (radian-partial (lambda (font) (find-font (font-spec :name font))))))
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
