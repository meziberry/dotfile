;;; -theme.el --- customize faces -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(defcustom theme-light/dark 'light
  "Nano theme uses light theme or dark theme?"
  :type 'symbol
  :group 'radian)

(defvar radian-theme-list '((modus-operandi . light) (modus-vivendi . dark))
  "Theme sequence of changing. `(THEME-NAME . *LIGHT-THEME)'")

(defun --l?d (light dark)
  "Determine using the LIGHT or the DARK color of theme."
  (if (eq theme-light/dark 'light) light dark))

(b modus-themes
  :init
  (setq modus-themes-vivendi-color-overrides
        '((bg-main . "#2E3440"))
        modus-themes-operandi-color-overrides
        '((bg-main . "#ECEFF1")))

  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t
        modus-themes-mixed-fonts nil
        modus-themes-subtle-line-numbers t
        modus-themes-intense-mouseovers t
        modus-themes-deuteranopia nil
        modus-themes-tabs-accented nil
        modus-themes-variable-pitch-ui t
        modus-themes-inhibit-reload t ; only applies to `customize-set-variable' and related

        modus-themes-fringes nil ; {nil,'subtle,'intense}

        ;; Options for `modus-themes-lang-checkers' are either nil (the
        ;; default), or a list of properties that may include any of those
        ;; symbols: `straight-underline', `text-also', `background',
        ;; `intense' OR `faint'.
        modus-themes-lang-checkers '(text-also intense)

        ;; Options for `modus-themes-mode-line' are either nil, or a list
        ;; that can combine any of `3d' OR `moody', `borderless',
        ;; `accented', and a natural number for extra padding
        modus-themes-mode-line '(accented borderless (padding . 3) (height . 0.9))
        ;; Same as above:
        ;; modus-themes-mode-line '(accented borderless 4 0.9)

        ;; Options for `modus-themes-markup' are either nil, or a list
        ;; that can combine any of `bold', `italic', `background',
        ;; `intense'.
        modus-themes-markup '(background italic)

        ;; Options for `modus-themes-syntax' are either nil (the default),
        ;; or a list of properties that may include any of those symbols:
        ;; `faint', `yellow-comments', `green-strings', `alt-syntax'
        modus-themes-syntax '(faint)

        ;; Options for `modus-themes-hl-line' are either nil (the default),
        ;; or a list of properties that may include any of those symbols:
        ;; `accented', `underline', `intense'
        modus-themes-hl-line '(accented)

        ;; Options for `modus-themes-paren-match' are either nil (the
        ;; default), or a list of properties that may include any of those
        ;; symbols: `bold', `intense', `underline'
        modus-themes-paren-match '(bold intense)

        ;; Options for `modus-themes-links' are either nil (the default),
        ;; or a list of properties that may include any of those symbols:
        ;; `neutral-underline' OR `no-underline', `faint' OR `no-color',
        ;; `bold', `italic', `background'
        modus-themes-links '(neutral-underline faint)

        ;; Options for `modus-themes-box-buttons' are either nil (the
        ;; default), or a list that can combine any of `flat',
        ;; `accented', `faint', `variable-pitch', `underline', the
        ;; symbol of any font weight as listed in
        ;; `modus-themes-weights', and a floating point number
        ;; (e.g. 0.9) for the height of the button's text.
        modus-themes-box-buttons '(variable-pitch flat faint 0.9)

        ;; Options for `modus-themes-prompts' are either nil (the
        ;; default), or a list of properties that may include any of those
        ;; symbols: `background', `bold', `gray', `intense', `italic'
        modus-themes-prompts '(intense bold)

        ;; The `modus-themes-completions' is an alist that reads three
        ;; keys: `matches', `selection', `popup'.  Each accepts a nil
        ;; value (or empty list) or a list of properties that can include
        ;; any of the following (for WEIGHT read further below):
        ;;
        ;; `key' - `background', `intense', `underline', `italic', WEIGHT
        ;; `selection' - `accented', `intense', `underline', `italic', WEIGHT
        ;; `popup' - same as `selected'
        ;; `t' - applies to any key not explicitly referenced (check docs)
        ;;
        ;; WEIGHT is a symbol such as `semibold', `light', or anything
        ;; covered in `modus-themes-weights'.  Bold is used in the absence
        ;; of an explicit WEIGHT.
        modus-themes-completions '((matches . (extrabold))
                                   (selection . (semibold accented))
                                   (popup . (accented intense)))

        modus-themes-mail-citations 'faint ; {nil,'intense,'faint,'monochrome}

        ;; Options for `modus-themes-region' are either nil (the default),
        ;; or a list of properties that may include any of those symbols:
        ;; `no-extend', `bg-only', `accented'
        modus-themes-region '(no-extend bg-only accented)

        ;; Options for `modus-themes-diffs': nil, 'desaturated, 'bg-only
        modus-themes-diffs 'desaturated

        modus-themes-org-blocks 'tinted-background ; {nil,'gray-background,'tinted-background}

        modus-themes-org-agenda ; this is an alist: read the manual or its doc string
        '((header-block . (variable-pitch regular 1.3))
          (header-date . (bold-today grayscale underline-today 1.1))
          (event . (accented varied))
          (scheduled . uniform)
          (habit . traffic-light))

        modus-themes-headings  ; this is an alist: read the manual or its doc string
        '((1 . (variable-pitch light 1.13))
          (2 . (variable-pitch regular 1.1))
          (3 . (variable-pitch regular 1.07))
          (4 . (monochrome 1.04))
          (5 . (1))
          (t . (rainbow 1)))))

(defun tinker-theme (theme)
  "Tinker the THEME theme"
  (let ((class '((class color) (min-colors 89)))
        ;; (fg         (--l?d "#37474F" "#ECEFF4"))
        ;; (bg         (--l?d "#FFFFFF" "#2E3440"))
        ;; (strong     (--l?d "#000000" "#ECEFF4"))
        ;; (subtle     (--l?d "#ECEFF1" "#434C5E"))
        (bgsubtle   (--l?d "#dbff4e" "#50616D"))
        (yellow     (--l?d "#8b7500" "#FFF143"))
        (warning    (--l?d "#8b0000" "#ffa7ba"))
        (match      (--l?d "#057748" "#BCE672"))
        (critical   (--l?d "#FF6F00" "#EBCB8B"))
        (bright     (--l?d "#FF00FF" "#FF47D1"))
        (highlight  (--l?d "#4169E1" "#30DFF3"))
        (prompt     (--l?d "#F00056" "#FF2D51"))
        (salient    (--l?d "#673AB7" "#81A1C1"))
        (popout     (--l?d "#FE8FA2" "#FFAB91"))
        (faded      (--l?d "#B0BEC5" "#677691")))

    (custom-theme-set-faces
     theme

     ;; awesome-tray
     `(awesome-tray-module-buffer-name-face ((,class :inherit bold :foreground ,warning)))
     `(awesome-tray-module-location-face ((,class :inherit bold :foreground ,yellow)))

     ;; meow
     `(meow-normal-indicator  ((,class (:foreground ,critical :bold t))))
     `(meow-beacon-indicator  ((,class (:foreground ,critical :bold t))))
     `(meow-keypad-indicator  ((,class (:foreground ,critical :bold t))))
     `(meow-insert-indicator  ((,class (:foreground ,critical :bold t))))
     `(meow-motion-indicator  ((,class (:foreground ,critical :bold t))))

     ;; whitespace-line
     `(whitespace-line        ((,class :background ,bgsubtle :foreground ,salient)))

     ;; vertico
     `(vertico-current        ((,class :background ,faded)))

     ;; git-gutter
     `(git-gutter:added       ((,class :background ,match)))
     `(git-gutter:deleted     ((,class :background ,bright)))
     `(git-gutter:modified    ((,class :background ,popout)))
     `(git-gutter:separator   ((,class :background ,salient)))
     `(git-gutter:unchanged   ((,class :background ,highlight)))
     ;; git-gutter-fr
     `(git-gutter-fr:added    ((,class :background ,match)))
     `(git-gutter-fr:deleted  ((,class :background ,bright)))
     `(git-gutter-fr:modified ((,class :background ,popout)))

     ;; M-x prompt-face
     `(minibuffer-prompt      ((,class (:foreground ,prompt))))
     `(comint-highlight-input ((,class (:foreground ,match :bold t))))))
  (enable-theme theme))

(defun radian/change-theme ()
  (interactive)
  (let* ((econf (car radian-theme-list))
         (theme (car econf))
         (theme-light/dark (cdr econf)))
    (setq radian-theme-list (append (cdr radian-theme-list) (list econf)))
    (disable-theme theme)
    (load-theme theme t)
    (tinker-theme theme))
  (radian-run-hooks 'radian-load-theme-hook))

(-key "M-h" #'radian/change-theme)

;;; -theme.el ends here
