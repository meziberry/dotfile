;; nano-theme.el --- A theme split from nano-emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 LiuBo

;; Author: LiuBo <https://github.com/404cn>
;; Created: May 30, 2021
;; Version: 1.0.0
;; Keywords: theme
;; Homepage: https://github.com/404cn/nano-theme.el
;; Package-Requires: ((emacs "28.0.50"))

;;
;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.


;;; Commentary:

;;; A theme split from nano-emacs.

;;; Code:

(deftheme nano "Theme split from nano-emacs")

(defgroup nano-theme nil
  "Options of nano theme."
  :group 'faces)

(defcustom nano-theme-header-scales '(1.3 1.2 1.1 1.0 1.0 1.0 1.0)
  "Scales for headers."
  :group 'nano-theme)

(defcustom nano-theme-light/dark 'light
  "Nano theme uses light theme or dark theme?"
  :type 'symbol
  :group 'nano-theme)

(defcustom nano-theme-padded-modeline 4
  "If non-nil, add a 4px padding to the mode-line. Can be an integer to determine the exact padding"
  :type '(choice integer boolean)
  :group 'nano-theme)

(defun nano-theme--light?dark (light dark)
  "Determine using the LIGHT or the DARK color of nano-theme."
  (if (eq nano-theme-light/dark 'light)
      light
    dark))
(defalias '--l?d #'nano-theme--light?dark)

(defun nano-theme-toggle ()
  "Toggle between light/dark nano theme"
  (interactive)
  (if (eq nano-theme-light/dark 'light)
      (setq nano-theme-light/dark 'dark)
    (setq nano-theme-light/dark 'light))
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme 'nano t))

(let ((class '((class color) (min-colors 89)))
      (n/茶棕 (--l?d "#B35C44" "#D2B38C"))
      (n/咖米 (--l?d "#705438" "#D3CBAF"))
      (n/緑春 (--l?d "#C8E6C6" "#E3EFD1"))
      (n/芽纺 (--l?d "#E3DBBF" "#FEF4B4"))
      (n/荼胡 (--l?d "#FFFAE8" "#F3F9F1"))
      (n/粉萅 (--l?d "#FF00FF" "#FF47D1"))
      (n/鱼肚 (--l?d "#EECBAD" "#FCEFE8"))
      (n/红火 (--l?d "#F00056" "#FF2D51"))
      (n/鸭黄 (--l?d "#FFF143" "#FAFF72"))
      (n/紫牙 (--l?d "#801DAE" "#EEDEB0"))

      (n/松绿 (--l?d "#BCE672" "#057748"))
      (n/绿松 (--l?d "#057748" "#BCE672"))
      (n/丁紫 (--l?d "#CCA4E3" "#801DAE"))
      (n/紫丁 (--l?d "#801DAE" "#CCA4E3"))
      (n/墨灰 (--l?d "#50616D" "#758A99"))
      (n/灰墨 (--l?d "#758A99" "#50616D"))
      (n/蓝靛 (--l?d "#30DFF3" "#4169E1"))
      (n/靛蓝 (--l?d "#4169E1" "#30DFF3"))
      (n/牙墨 (--l?d "#EEDEB0" "#50616D"))
      (n/墨牙 (--l?d "#50616D" "#EEDEB0"))
      (n/牙黛 (--l?d "#EEDEB0" "#4A4266"))
      (n/黛牙 (--l?d "#4A4266" "#EEDEB0"))
      (n/素墨 (--l?d "#E0F0E9" "#50616D"))
      (n/墨素 (--l?d "#50616D" "#E0F0E9"))
      (n/素灰 (--l?d "#E0F0E9" "#758A99"))
      (n/灰素 (--l?d "#758A99" "#E0F0E9"))
      (n/缟墨 (--l?d "#F2ECDE" "#50616D"))
      (n/墨缟 (--l?d "#50616D" "#F2ECDE"))
      (n/缟漆 (--l?d "#F2ECDE" "#161823"))
      (n/漆缟 (--l?d "#161823" "#F2ECDE"))
      (n/缟黛 (--l?d "#F2ECDE" "#4A4266"))
      (n/黛缟 (--l?d "#4A4266" "#F2ECDE"))
      (n/霜黛 (--l?d "#E9F1F6" "#4A4266"))
      (n/黛霜 (--l?d "#4A4266" "#E9F1F6"))
      (n/貝酱 (--l?d "#FFE4E0" "#923A60"))
      (n/酱貝 (--l?d "#923A60" "#FFE4E0"))

      (foreground                    (--l?d "#37474F" "#ECEFF4"))
      (background                    (--l?d "#FFFFFF" "#2E3440"))
      (highlight                     (--l?d "#FAFAFA" "#3B4252"))
      (critical                      (--l?d "#FF6F00" "#EBCB8B"))
      (salient                       (--l?d "#673AB7" "#81A1C1"))
      (strong                        (--l?d "#000000" "#ECEFF4"))
      (popout                        (--l?d "#FFAB91" "#D08770"))
      (subtle                        (--l?d "#ECEFF1" "#434C5E"))
      (faded                         (--l?d "#B0BEC5" "#677691"))
      (fg-alt                        (--l?d "#505050" "#a8a8a8"))
      (bg-alt                        (--l?d "#f0f0f0" "#4A4266"))

      (bg-active                     (--l?d "#d7d7d7" "#323232"))
      (fg-active                     (--l?d "#0a0a0a" "#f4f4f4"))
      (bg-inactive                   (--l?d "#efefef" "#1e1e1e"))
      (fg-inactive                   (--l?d "#404148" "#bfc0c4"))
      (bg-active-accent              (--l?d "#d0d6ff" "#2a2a66"))

      (bg-special-cold               (--l?d "#dde3f4" "#203448"))
      (fg-special-cold               (--l?d "#093060" "#c6eaff"))
      (bg-special-mild               (--l?d "#c4ede0" "#00322e"))
      (fg-special-mild               (--l?d "#184034" "#bfebe0"))
      (bg-special-warm               (--l?d "#f0e0d4" "#382f27"))
      (fg-special-warm               (--l?d "#5d3026" "#f8dec0"))
      (bg-special-calm               (--l?d "#f8ddea" "#392a48"))
      (fg-special-calm               (--l?d "#61284f" "#fbd6f4"))

      (red                           (--l?d "#a60000" "#ff8059"))
      (red-alt                       (--l?d "#972500" "#ef8b50"))
      (red-alt-other                 (--l?d "#a0132f" "#ff9077"))
      (red-faint                     (--l?d "#7f1010" "#ffa0a0"))
      (red-alt-faint                 (--l?d "#702f00" "#f5aa80"))
      (red-alt-other-faint           (--l?d "#7f002f" "#ff9fbf"))
      (green                         (--l?d "#005e00" "#44bc44"))
      (green-alt                     (--l?d "#315b00" "#70b900"))
      (green-alt-other               (--l?d "#145c33" "#00c06f"))
      (green-faint                   (--l?d "#104410" "#78bf78"))
      (green-alt-faint               (--l?d "#30440f" "#99b56f"))
      (green-alt-other-faint         (--l?d "#0f443f" "#88bf99"))
      (yellow                        (--l?d "#813e00" "#d0bc00"))
      (yellow-alt                    (--l?d "#70480f" "#c0c530"))
      (yellow-alt-other              (--l?d "#863927" "#d3b55f"))
      (yellow-faint                  (--l?d "#5f4400" "#d2b580"))
      (yellow-alt-faint              (--l?d "#5d5000" "#cabf77"))
      (yellow-alt-other-faint        (--l?d "#5e3a20" "#d0ba95"))
      (blue                          (--l?d "#0031a9" "#2fafff"))
      (blue-alt                      (--l?d "#2544bb" "#79a8ff"))
      (blue-alt-other                (--l?d "#0000c0" "#00bcff"))
      (blue-faint                    (--l?d "#003497" "#82b0ec"))
      (blue-alt-faint                (--l?d "#0f3d8c" "#a0acef"))
      (blue-alt-other-faint          (--l?d "#001087" "#80b2f0"))
      (magenta                       (--l?d "#721045" "#feacd0"))
      (magenta-alt                   (--l?d "#8f0075" "#f78fe7"))
      (magenta-alt-other             (--l?d "#5317ac" "#b6a0ff"))
      (magenta-faint                 (--l?d "#752f50" "#e0b2d6"))
      (magenta-alt-faint             (--l?d "#7b206f" "#ef9fe4"))
      (magenta-alt-other-faint       (--l?d "#55348e" "#cfa6ff"))
      (cyan                          (--l?d "#00538b" "#00d3d0"))
      (cyan-alt                      (--l?d "#30517f" "#4ae2f0"))
      (cyan-alt-other                (--l?d "#005a5f" "#6ae4b9"))
      (cyan-faint                    (--l?d "#005077" "#90c4ed"))
      (cyan-alt-faint                (--l?d "#354f6f" "#a0bfdf"))
      (cyan-alt-other-faint          (--l?d "#125458" "#a4d0bb"))

      (red-intense                   (--l?d "#b60000" "#fe6060"))
      (orange-intense                (--l?d "#904200" "#fba849"))
      (green-intense                 (--l?d "#006800" "#4fe42f"))
      (yellow-intense                (--l?d "#605b00" "#f0dd60"))
      (blue-intense                  (--l?d "#1f1fce" "#4fafff"))
      (magenta-intense               (--l?d "#a8007f" "#ff62d4"))
      (purple-intense                (--l?d "#7f10d0" "#9f80ff"))
      (cyan-intense                  (--l?d "#005f88" "#3fdfd0"))

      (red-active                    (--l?d "#8a0000" "#ffa7ba"))
      (green-active                  (--l?d "#004c2e" "#70d73f"))
      (yellow-active                 (--l?d "#702f00" "#dbbe5f"))
      (blue-active                   (--l?d "#0030b4" "#34cfff"))
      (magenta-active                (--l?d "#5c2092" "#d5b1ff"))
      (cyan-active                   (--l?d "#003f8a" "#00d8b4"))

      (red-subtle-bg                 (--l?d "#f2b0a2" "#762422"))
      (red-intense-bg                (--l?d "#ff9f9f" "#a4202a"))
      (green-subtle-bg               (--l?d "#aecf90" "#2f4a00"))
      (green-intense-bg              (--l?d "#5ada88" "#006800"))
      (yellow-subtle-bg              (--l?d "#e4c340" "#604200"))
      (yellow-intense-bg             (--l?d "#f5df23" "#874900"))
      (blue-subtle-bg                (--l?d "#b5d0ff" "#10387c"))
      (blue-intense-bg               (--l?d "#77baff" "#2a40b8"))
      (magenta-subtle-bg             (--l?d "#f0d3ff" "#49366e"))
      (magenta-intense-bg            (--l?d "#d5baff" "#7042a2"))
      (cyan-subtle-bg                (--l?d "#c0efff" "#00415e"))
      (cyan-intense-bg               (--l?d "#42cbd4" "#005f88"))

      (red-fringe-bg                 (--l?d "#f08290" "#8f1f4b"))
      (green-fringe-bg               (--l?d "#62c86a" "#006700"))
      (yellow-fringe-bg              (--l?d "#dbba3f" "#6f4f00"))
      (blue-fringe-bg                (--l?d "#82afff" "#3f33af"))
      (magenta-fringe-bg             (--l?d "#e0a3ff" "#6f2f89"))
      (cyan-fringe-bg                (--l?d "#2fcddf" "#004f8f"))

      (red-graph-0-bg                (--l?d "#ef6f79" "#af0404"))
      (red-graph-1-bg                (--l?d "#ff9f9f" "#801f2f"))
      (green-graph-0-bg              (--l?d "#49d239" "#24ba2f"))
      (green-graph-1-bg              (--l?d "#6dec6d" "#0f8f07"))
      (yellow-graph-0-bg             (--l?d "#efec08" "#ffd03e"))
      (yellow-graph-1-bg             (--l?d "#dbff4e" "#d7d800"))
      (blue-graph-0-bg               (--l?d "#55a2f0" "#406fff"))
      (blue-graph-1-bg               (--l?d "#7fcfff" "#2f50c8"))
      (magenta-graph-0-bg            (--l?d "#ba86ef" "#af7bee"))
      (magenta-graph-1-bg            (--l?d "#e7afff" "#7f59cf"))
      (cyan-graph-0-bg               (--l?d "#30d3f0" "#47dcfa"))
      (cyan-graph-1-bg               (--l?d "#6fefff" "#0bc0df"))

      (red-refine-bg                 (--l?d "#ffcccc" "#77002a"))
      (red-refine-fg                 (--l?d "#780000" "#ffb9ab"))
      (green-refine-bg               (--l?d "#aceaac" "#00422a"))
      (green-refine-fg               (--l?d "#004c00" "#9ff0cf"))
      (yellow-refine-bg              (--l?d "#fff29a" "#693200"))
      (yellow-refine-fg              (--l?d "#604000" "#e2d980"))
      (blue-refine-bg                (--l?d "#8fcfff" "#242679"))
      (blue-refine-fg                (--l?d "#002f88" "#8ecfff"))
      (magenta-refine-bg             (--l?d "#ffccff" "#71206a"))
      (magenta-refine-fg             (--l?d "#770077" "#ffcaf0"))
      (cyan-refine-bg                (--l?d "#8eecf4" "#004065"))
      (cyan-refine-fg                (--l?d "#004850" "#8ae4f2"))

      (red-nuanced-bg                (--l?d "#fff1f0" "#2c0614"))
      (red-nuanced-fg                (--l?d "#5f0000" "#ffcccc"))
      (green-nuanced-bg              (--l?d "#ecf7ed" "#001904"))
      (green-nuanced-fg              (--l?d "#004000" "#b8e2b8"))
      (yellow-nuanced-bg             (--l?d "#fff3da" "#221000"))
      (yellow-nuanced-fg             (--l?d "#3f3000" "#dfdfb0"))
      (blue-nuanced-bg               (--l?d "#f3f3ff" "#0f0e39"))
      (blue-nuanced-fg               (--l?d "#201f55" "#bfd9ff"))
      (magenta-nuanced-bg            (--l?d "#fdf0ff" "#230631"))
      (magenta-nuanced-fg            (--l?d "#541f4f" "#e5cfef"))
      (cyan-nuanced-bg               (--l?d "#ebf6fa" "#041529"))
      (cyan-nuanced-fg               (--l?d "#0f3360" "#a8e5e5"))

      (bg-hl-line                    (--l?d "#f2eff3" "#151823"))
      (bg-hl-line-intense            (--l?d "#e0e0e0" "#292929"))
      (bg-hl-line-intense-accent     (--l?d "#b9e1ef" "#00353f"))
      (bg-hl-alt                     (--l?d "#fbeee0" "#181732"))
      (bg-hl-alt-intense             (--l?d "#e8dfd1" "#282e46"))
      (bg-paren-match                (--l?d "#e0af82" "#5f362f"))
      (bg-paren-match-intense        (--l?d "#c488ff" "#7416b5"))
      (bg-paren-expression           (--l?d "#dff0ff" "#221044"))
      (bg-region                     (--l?d "#bcbcbc" "#3c3c3c"))
      (bg-region-accent              (--l?d "#afafef" "#4f3d88"))
      (bg-region-accent-subtle       (--l?d "#efdfff" "#240f55"))

      (bg-tab-active                 (--l?d "#f6f6f6" "#0e0e0e"))
      (bg-tab-inactive               (--l?d "#b7b7b7" "#424242"))
      (bg-tab-inactive-accent        (--l?d "#a9b4f6" "#35398f"))
      (bg-tab-inactive-alt           (--l?d "#9f9f9f" "#595959"))
      (bg-tab-inactive-alt-accent    (--l?d "#9fa6d0" "#505588"))

      (red-tab                       (--l?d "#680000" "#ffc0bf"))
      (green-tab                     (--l?d "#003900" "#88ef88"))
      (yellow-tab                    (--l?d "#393000" "#d2e580"))
      (orange-tab                    (--l?d "#502300" "#f5ca80"))
      (blue-tab                      (--l?d "#000080" "#92d9ff"))
      (cyan-tab                      (--l?d "#052f60" "#60e7e0"))
      (magenta-tab                   (--l?d "#5f004d" "#ffb8ff"))
      (purple-tab                    (--l?d "#400487" "#cfcaff"))

      (fg-escape-char-construct      (--l?d "#8b1030" "#e7a59a"))
      (fg-escape-char-backslash      (--l?d "#654d0f" "#abab00"))

      (fg-lang-error                 (--l?d "#9f004f" "#ef8690"))
      (fg-lang-warning               (--l?d "#604f0f" "#b0aa00"))
      (fg-lang-note                  (--l?d "#4040ae" "#9d9def"))
      (fg-lang-underline-error       (--l?d "#ef4f54" "#ff4a6f"))
      (fg-lang-underline-warning     (--l?d "#cf9f00" "#d0de00"))
      (fg-lang-underline-note        (--l?d "#3f6fef" "#5f6fff"))

      (fg-window-divider-inner       (--l?d "#888888" "#646464"))
      (fg-window-divider-outer       (--l?d "#585858" "#969696"))

      (fg-unfocused                  (--l?d "#56576d" "#93959b"))

      (fg-docstring                  (--l?d "#2a486a" "#b0d6f5"))
      (fg-comment-yellow             (--l?d "#794319" "#d0a070"))

      (bg-header                     (--l?d "#e5e5e5" "#212121"))
      (fg-header                     (--l?d "#2a2a2a" "#dddddd"))

      (bg-whitespace                 (--l?d "#f5efef" "#101424"))
      (fg-whitespace                 (--l?d "#624956" "#aa9e9f"))

      (bg-diff-heading               (--l?d "#b7cfe0" "#304466"))
      (fg-diff-heading               (--l?d "#041645" "#dae7ff"))
      (bg-diff-added                 (--l?d "#d4fad4" "#0a280a"))
      (fg-diff-added                 (--l?d "#004500" "#94ba94"))
      (bg-diff-added-deuteran        (--l?d "#daefff" "#001a3f"))
      (fg-diff-added-deuteran        (--l?d "#002044" "#c4cdf2"))
      (bg-diff-changed               (--l?d "#fcefcf" "#2a2000"))
      (fg-diff-changed               (--l?d "#524200" "#b0ba9f"))
      (bg-diff-removed               (--l?d "#ffe8ef" "#40160f"))
      (fg-diff-removed               (--l?d "#691616" "#c6adaa"))

      (bg-diff-refine-added          (--l?d "#94cf94" "#005a36"))
      (fg-diff-refine-added          (--l?d "#002a00" "#e0f6e0"))
      (bg-diff-refine-added-deuteran (--l?d "#77c0ef" "#234f8f"))
      (fg-diff-refine-added-deuteran (--l?d "#000035" "#dde4ff"))
      (bg-diff-refine-changed        (--l?d "#cccf8f" "#585800"))
      (fg-diff-refine-changed        (--l?d "#302010" "#ffffcc"))
      (bg-diff-refine-removed        (--l?d "#daa2b0" "#852828"))
      (fg-diff-refine-removed        (--l?d "#400000" "#ffd9eb"))

      (bg-diff-focus-added           (--l?d "#bbeabb" "#1d3c25"))
      (fg-diff-focus-added           (--l?d "#002c00" "#b4ddb4"))
      (bg-diff-focus-added-deuteran  (--l?d "#bacfff" "#003959"))
      (fg-diff-focus-added-deuteran  (--l?d "#001755" "#bfe4ff"))
      (bg-diff-focus-changed         (--l?d "#ecdfbf" "#424200"))
      (fg-diff-focus-changed         (--l?d "#392900" "#d0daaf"))
      (bg-diff-focus-removed         (--l?d "#efcbcf" "#500f29"))
      (fg-diff-focus-removed         (--l?d "#4a0000" "#eebdba"))

      (bg-mark-sel                   (--l?d "#a0f0cf" "#002f2f"))
      (fg-mark-sel                   (--l?d "#005040" "#60cfa2"))
      (bg-mark-del                   (--l?d "#ffccbb" "#5a0000"))
      (fg-mark-del                   (--l?d "#840040" "#ff99aa"))
      (bg-mark-alt                   (--l?d "#f5d88f" "#3f2210"))
      (fg-mark-alt                   (--l?d "#782900" "#f0aa20"))

      (-modeline-pad
       (when nano-theme-padded-modeline
         (if (integerp nano-theme-padded-modeline) nano-theme-padded-modeline 4))))
  (custom-theme-set-faces
   `nano
   ;; Basic
   `(default                    ((,class (:foreground ,foreground :background ,background))))
   `(cursor                     ((,class (:background ,foreground))))
   `(fringe                     ((,class (:foreground ,faded))))
   `(show-paren-match           ((,class (:background ,subtle :box (:line-width (-1 . -1))))))
   `(hl-line                    ((,class (:background ,highlight))))
   `(highlight                  ((,class (:background ,subtle))))
   `(lazy-highlight             ((,class (:background ,subtle :box (:line-width (-1 . -1))))))
   `(region                     ((,class (:background ,faded))))
   `(line-number                ((,class (:background ,highlight :foreground ,faded))))
   `(line-number-current-line   ((,class (:background ,highlight :foreground ,strong))))
   `(minibuffer-prompt          ((,class (:foreground ,n/粉萅))))
   `(vertical-border            ((,class (:foreground ,subtle))))
   `(window-divider             ((,class (:foreground ,subtle))))
   `(window-divider-first-pixel ((,class (:foreground ,subtle))))
   `(window-divider-last-pixel  ((,class (:foreground ,subtle))))
   `(fill-column-indicator      ((,class (:foreground ,strong))))
   `(shadow                     ((,class (:foreground ,faded))))
   `(italic                     ((,class :slant italic)))
   `(success                    ((,class (:foreground ,n/绿松))))
   `(warning                    ((,class (:foreground ,popout))))
   `(error                      ((,class (:foreground ,critical))))
   `(match                      ((,class (:foreground ,popout))))
   `(link                       ((,class (:foreground ,salient))))
   `(railing-whitespace         ((,class (:background ,subtle))))
   `(completions-common-part    ((,class (:foreground ,faded))))
   `(secondary-selection        ((,class (:background ,subtle))))
   `(comint-highlight-input     ((,class (:foreground ,n/靛蓝 :bold t))))
   `(header-line                ((,class (:background ,subtle :foreground ,strong
                                                      :box (:line-width 2 :style released-button)))))

   ;; Font Locks
   `(font-lock-comment-face              ((,class :inherit italic :foreground ,faded)))
   `(font-lock-comment-delimiter-face    ((,class :inherit font-lock-comment-face)))
   `(font-lock-keyword-face              ((,class (:foreground ,blue-intense))))
   `(font-lock-string-face               ((,class (:foreground ,n/绿松))))
   `(font-lock-doc-face                  ((,class (:foreground ,faded))))
   `(font-lock-builtin-face              ((,class :inherit bold :foreground ,salient)))
   `(font-lock-type-face                 ((,class :inherit bold :foreground ,popout)))
   `(font-lock-variable-name-face        ((,class :foreground ,magenta-alt)))
   `(font-lock-constant-face             ((,class (:foreground ,salient))))
   `(font-lock-function-name-face        ((,class (:foreground ,strong :underline t))))
   `(font-lock-warning-face              ((,class (:foreground ,n/红火))))
   `(font-lock-preprocessor-face         ((,class (:foreground ,n/墨素))))
   `(font-lock-negation-char-face        ((,class :inherit bold :foreground ,n/茶棕)))
   `(font-lock-regexp-grouping-backslash ((,class :inherit bold :foreground ,n/紫丁)))
   `(font-lock-regexp-grouping-construct ((,class :inherit bold :foreground ,n/靛蓝)))

   ;; Eldoc
   `(eldoc-highlight-function-argument ((,class (:foreground ,strong :bold t))))

   ;; ISearch
   `(isearch      ((,class (:foreground ,strong))))
   `(isearch-fail ((,class (:foreground ,faded))))

   ;; Info
   `(info-menu-header ((,class (:foreground ,foreground :bold t))))
   `(info-header-node ((,class (:foreground ,foreground :background ,background))))
   `(info-index-match ((,class (:foreground ,salient))))
   `(Info-quoted      ((,class (:foreground ,faded))))
   `(info-title-1     ((,class (:inherit info-menu-header))))
   `(info-title-2     ((,class (:inherit info-menu-header))))
   `(info-title-3     ((,class (:inherit info-menu-header))))
   `(info-title-4     ((,class (:inherit info-menu-header))))

   ;; Bookmark
   `(bookmark-face          ((,class (:foreground ,popout :box t))))
   `(bookmark-menu-heading  ((,class (:foreground ,foreground :bold t))))
   `(bookmark-menu-bookmark ((,class (:foreground ,salient))))

   ;; Outline
   `(outline-1 ((,class (:foreground ,foreground :bold t))))
   `(outline-2 ((,class (:foreground ,foreground :bold t))))
   `(outline-3 ((,class (:foreground ,foreground :bold t))))
   `(outline-4 ((,class (:foreground ,foreground :bold t))))
   `(outline-5 ((,class (:foreground ,foreground :bold t))))
   `(outline-6 ((,class (:foreground ,foreground :bold t))))
   `(outline-7 ((,class (:foreground ,foreground :bold t))))
   `(outline-8 ((,class (:foreground ,foreground :bold t))))

   ;; Message
   `(message-cited-text        ((,class (:foreground ,faded))))
   `(message-header-cc         ((,class (:foreground ,foreground :background ,background))))
   `(message-header-name       ((,class (:foreground ,foreground :bold t))))
   `(message-header-newsgroups ((,class (:foreground ,foreground :background ,background))))
   `(message-header-other      ((,class (:foreground ,foreground :background ,background))))
   `(message-header-subject    ((,class (:foreground ,salient))))
   `(message-header-to         ((,class (:foreground ,salient))))
   `(message-header-xheader    ((,class (:foreground ,foreground :background ,background))))
   `(message-mml               ((,class (:foreground ,popout))))
   `(message-separator         ((,class (:foreground ,faded))))

   ;; Customize
   `(widget-field             ((,class (:background ,subtle))))
   `(widget-button            ((,class (:foreground ,foreground :bold t))))
   `(widget-single-line-field ((,class (:background ,subtle))))
   `(custom-group-subtitle    ((,class (:foreground ,foreground :bold t))))
   `(custom-group-tag         ((,class (:foreground ,foreground :bold t))))
   `(custom-group-tag-1       ((,class (:foreground ,foreground :bold t))))
   `(custom-comment           ((,class (:foreground ,faded))))
   `(custom-comment-tag       ((,class (:foreground ,faded))))
   `(custom-changed           ((,class (:foreground ,salient))))
   `(custom-modified          ((,class (:foreground ,salient))))
   `(custom-face-tag          ((,class (:foreground ,foreground :bold t))))
   `(custom-variable-tag      ((,class (:foreground ,foreground :bold t))))
   `(custom-invalid           ((,class (:foreground ,popout))))
   `(custom-visibility        ((,class (:foreground ,salient))))
   `(custom-state             ((,class (:foreground ,salient))))
   `(custom-link              ((,class (:foreground ,salient))))
   `(custom-button            ((,class (:foreground ,faded :background ,background :box `(:line-width 1 :color ,(face-foreground 'faded) :style nil)))))
   `(custom-button-mouse      ((,class (:foreground ,faded :background ,subtle :box `(:line-width 1 :color ,(face-foreground 'faded) :style nil)))))
   `(custom-button-pressed    ((,class (:foreground ,foreground :background ,salient :inverse-video nil :box `(:line-width 1 :color ,(face-foreground 'salient) :style nil)))))

   ;; Package
   `(package-description       ((,class (:foreground ,foreground :background ,background))))
   `(package-help-section-name ((,class (,class (:foreground ,foreground :background ,background)))))
   `(package-name              ((,class (:foreground ,salient))))
   `(package-status-avail-obso ((,class (:foreground ,faded))))
   `(package-status-available  ((,class (:foreground ,foreground :background ,background))))
   `(package-status-built-in   ((,class (:foreground ,salient))))
   `(package-status-dependency ((,class (,class (:foreground ,salient)))))
   `(package-status-disabled   ((,class (:foreground ,faded))))
   `(package-status-external   ((,class (:foreground ,foreground :background ,background))))
   `(package-status-held       ((,class (:foreground ,foreground :background ,background))))
   `(package-status-incompat   ((,class (:foreground ,faded))))
   `(package-status-installed  ((,class (:foreground ,salient))))
   `(package-status-new        ((,class (:foreground ,foreground :background ,background))))
   `(package-status-new        ((,class (:foreground ,foreground :background ,background))))

   ;; Flyspell
   `(flyspell-duplicate ((,class (:underline ,critical))))
   `(flyspell-incorrect ((,class (:underline ,critical))))

   ;; Diff
   `(diff-context     ((,class (:foreground ,faded))))
   `(diff-hunk-header ((,class (:background ,subtle :foreground ,foreground))))
   `(diff-function    ((,class (:inherit diff-hunk-header))))
   `(diff-header      ((,class (:foreground ,strong :bold t))))
   `(diff-file-header ((,class (:inherit diff-header))))

   ;; magit
   `(magit-diff-hunk-heading ((,class (:background ,subtle))))

   ;; dired-hacks
   `(dired-subtree-depth-1-face ((,class (:background ,background))))
   `(dired-subtree-depth-2-face ((,class (:background ,background))))
   `(dired-subtree-depth-3-face ((,class (:background ,background))))
   `(dired-subtree-depth-4-face ((,class (:background ,background))))
   `(dired-subtree-depth-5-face ((,class (:background ,background))))
   `(dired-subtree-depth-6-face ((,class (:background ,background))))

   ;; Agenda
   `(org-agenda-calendar-event   ((,class (:foreground ,foreground :background ,background))))
   `(org-agenda-calendar-sexp    ((,class (:foreground ,salient))))
   `(org-agenda-clocking         ((,class (:foreground ,faded))))
   `(org-agenda-column-dateline  ((,class (:foreground ,faded))))
   `(org-agenda-current-time     ((,class (:foreground ,foreground :bold t))))
   `(org-agenda-date             ((,class (:foreground ,salient))))
   `(org-agenda-date-today       ((,class (:foreground ,salient :bold t))))
   `(org-agenda-date-weekend     ((,class (:foreground ,faded))))
   `(org-agenda-diary            ((,class (:foreground ,faded))))
   `(org-agenda-dimmed-todo-face ((,class (:foreground ,faded))))
   `(org-agenda-done             ((,class (:foreground ,faded))))
   `(org-agenda-filter-category  ((,class (:foreground ,faded))))
   `(org-agenda-filter-effort    ((,class (:foreground ,faded))))
   `(org-agenda-filter-regexp    ((,class (:foreground ,faded))))
   `(org-agenda-filter-tags      ((,class (:foreground ,faded))))
   `(org-agenda-restriction-lock ((,class (:foreground ,faded))))
   `(org-agenda-structure        ((,class (:foreground ,foreground :bold t))))

   ;; Org
   `(org-archived                 ((,class (:foreground ,faded))))
   `(org-block                    ((,class (:background ,highlight))))
   `(org-block-begin-line         ((,class (:foreground ,faded))))
   `(org-block-end-line           ((,class (:foreground ,faded))))
   `(org-checkbox                 ((,class (:foreground ,faded))))
   `(org-checkbox-statistics-done ((,class (:foreground ,faded))))
   `(org-checkbox-statistics-todo ((,class (:foreground ,faded))))
   `(org-clock-overlay            ((,class (:foreground ,faded))))
   `(org-code                     ((,class (:foreground ,faded))))
   `(org-column                   ((,class (:foreground ,faded))))
   `(org-column-title             ((,class (:foreground ,faded))))
   `(org-date                     ((,class (:foreground ,faded))))
   `(org-date-selected            ((,class (:foreground ,faded))))
   `(org-default                  ((,class (:foreground ,faded))))
   `(org-document-info            ((,class (:foreground ,faded))))
   `(org-document-info-keyword    ((,class (:foreground ,faded))))
   `(org-document-title           ((,class (:foreground ,faded))))
   `(org-done                     ((,class (:foreground ,foreground :background ,background))))
   `(org-drawer                   ((,class (:foreground ,faded))))
   `(org-ellipsis                 ((,class (:foreground ,faded))))
   `(org-footnote                 ((,class (:foreground ,faded))))
   `(org-formula                  ((,class (:foreground ,faded))))
   `(org-headline-done            ((,class (:foreground ,faded))))
   `(org-latex-and-related        ((,class (:foreground ,faded))))
   `(org-level-1                  ((,class (:foreground ,foreground :bold t :height ,(nth 0 nano-theme-header-scales)))))
   `(org-level-2                  ((,class (:inherit org-level-1 :height ,(nth 1 nano-theme-header-scales)))))
   `(org-level-3                  ((,class (:inherit org-level-1 :height ,(nth 2 nano-theme-header-scales)))))
   `(org-level-4                  ((,class (:inherit org-level-1 :height ,(nth 3 nano-theme-header-scales)))))
   `(org-level-5                  ((,class (:inherit org-level-1 :height ,(nth 4 nano-theme-header-scales)))))
   `(org-level-6                  ((,class (:inherit org-level-1 :height ,(nth 5 nano-theme-header-scales)))))
   `(org-level-7                  ((,class (:inherit org-level-1 :height ,(nth 6 nano-theme-header-scales)))))
   `(org-link                     ((,class (:foreground ,salient))))
   `(org-list-dt                  ((,class (:foreground ,faded))))
   `(org-macro                    ((,class (:foreground ,faded))))
   `(org-meta-line                ((,class (:foreground ,faded))))
   `(org-mode-line-clock          ((,class (:foreground ,faded))))
   `(org-mode-line-clock-overrun  ((,class (:foreground ,faded))))
   `(org-priority                 ((,class (:foreground ,faded))))
   `(org-property-value           ((,class (:foreground ,faded))))
   `(org-quote                    ((,class (:foreground ,faded))))
   `(org-scheduled                ((,class (:foreground ,faded))))
   `(org-scheduled-previously     ((,class (:foreground ,faded))))
   `(org-scheduled-today          ((,class (:foreground ,faded))))
   `(org-sexp-date                ((,class (:foreground ,faded))))
   `(org-special-keyword          ((,class (:foreground ,faded))))
   `(org-table                    ((,class (:foreground ,faded))))
   `(org-tag                      ((,class (:foreground ,popout))))
   `(org-tag-group                ((,class (:foreground ,faded))))
   `(org-target                   ((,class (:foreground ,faded))))
   `(org-time-grid                ((,class (:foreground ,faded))))
   `(org-todo                     ((,class (:foreground ,salient))))
   `(org-upcoming-deadline        ((,class (:foreground ,foreground :background ,background))))
   `(org-verbatim                 ((,class (:foreground ,popout))))
   `(org-verse                    ((,class (:foreground ,faded))))
   `(org-warning                  ((,class (:foreground ,popout))))

   ;; Elfeed
   `(elfeed-log-date-face            ((,class (:foreground ,faded))))
   `(elfeed-log-info-level-face      ((,class (:foreground ,foreground :background ,background))))
   `(elfeed-log-debug-level-face     ((,class (:foreground ,foreground :background ,background))))
   `(elfeed-log-warn-level-face      ((,class (:foreground ,popout))))
   `(elfeed-log-error-level-face     ((,class (:foreground ,popout))))
   `(elfeed-search-tag-face          ((,class (:foreground ,faded))))
   `(elfeed-search-date-face         ((,class (:foreground ,faded))))
   `(elfeed-search-feed-face         ((,class (:foreground ,salient))))
   `(elfeed-search-filter-face       ((,class (:foreground ,faded))))
   `(elfeed-search-last-update-face  ((,class (:foreground ,salient))))
   `(elfeed-search-title-face        ((,class (:foreground ,foreground :background ,background))))
   `(elfeed-search-tag-face          ((,class (:foreground ,faded))))
   `(elfeed-search-unread-count-face ((,class (:foreground ,foreground :bold t))))
   `(elfeed-search-unread-title-face ((,class (:foreground ,foreground :bold t))))

   ;; Markdown
   `(markdown-blockquote-face         ((,class (:inherit default))))
   `(markdown-bold-face               ((,class (:foreground ,foreground :bold t))))
   `(markdown-code-face               ((,class (:inherit default))))
   `(markdown-comment-face            ((,class (:foreground ,faded))))
   `(markdown-footnote-marker-face    ((,class (:inherit default))))
   `(markdown-footnote-text-face      ((,class (:inherit default))))
   `(markdown-gfm-checkbox-face       ((,class (:inherit default))))
   `(markdown-header-delimiter-face   ((,class (:foreground ,faded))))
   `(markdown-header-face             ((,class (:foreground ,foreground :bold t))))
   `(markdown-header-rule-face        ((,class (:inherit default))))
   `(markdown-highlight-face          ((,class (:inherit default))))
   `(markdown-hr-face                 ((,class (:inherit default))))
   `(markdown-html-attr-name-face     ((,class (:inherit default))))
   `(markdown-html-attr-value-face    ((,class (:inherit default))))
   `(markdown-html-entity-face        ((,class (:inherit default))))
   `(markdown-html-tag-delimiter-face ((,class (:inherit default))))
   `(markdown-html-tag-name-face      ((,class (:inherit default))))
   `(markdown-inline-code-face        ((,class (:foreground ,popout))))
   `(markdown-italic-face             ((,class (:foreground ,faded))))
   `(markdown-language-info-face      ((,class (:inherit default))))
   `(markdown-language-keyword-face   ((,class (:inherit default))))
   `(markdown-line-break-face         ((,class (:inherit default))))
   `(markdown-link-face               ((,class (:foreground ,salient))))
   `(markdown-link-title-face         ((,class (:inherit default))))
   `(markdown-list-face               ((,class (:foreground ,faded))))
   `(markdown-markup-face             ((,class (:foreground ,faded))))
   `(markdown-math-face               ((,class (:inherit default))))
   `(markdown-metadata-key-face       ((,class (:foreground ,faded))))
   `(markdown-metadata-value-face     ((,class (:foreground ,faded))))
   `(markdown-missing-link-face       ((,class (:inherit default))))
   `(markdown-plain-url-face          ((,class (:inherit default))))
   `(markdown-pre-face                ((,class (:inherit default))))
   `(markdown-reference-face          ((,class (:foreground ,salient))))
   `(markdown-strike-through-face     ((,class (:foreground ,faded))))
   `(markdown-table-face              ((,class (:inherit default))))
   `(markdown-url-face                ((,class (:foreground ,salient))))
   `(markdown-header-face-1           ((,class (:foreground ,foreground :bold t :height ,(nth 0 nano-theme-header-scales)))))
   `(markdown-header-face-2           ((,class (:inherit markdown-header-face-1 :height ,(nth 1 nano-theme-header-scales)))))
   `(markdown-header-face-3           ((,class (:inherit markdown-header-face-1 :height ,(nth 2 nano-theme-header-scales)))))
   `(markdown-header-face-4           ((,class (:inherit markdown-header-face-1 :height ,(nth 3 nano-theme-header-scales)))))
   `(markdown-header-face-5           ((,class (:inherit markdown-header-face-1 :height ,(nth 4 nano-theme-header-scales)))))
   `(markdown-header-face-6           ((,class (:inherit markdown-header-face-1 :height ,(nth 5 nano-theme-header-scales)))))
   `(markdown-header-face-7           ((,class (:inherit markdown-header-face-1 :height ,(nth 6 nano-theme-header-scales)))))

   ;; Notmuch
   `(notmuch-tag-face             ((,class (:foreground ,faded))))
   `(notmuch-tag-unread           ((,class (:foreground ,faded))))
   `(notmuch-search-date          ((,class (:foreground ,faded))))
   `(notmuch-tag-deleted          ((,class (:strike-through ,popout))))
   `(notmuch-tag-added            ((,class (:underline ,popout))))
   `(notmuch-wash-cited-text      ((,class (:foreground ,faded))))
   `(notmuch-message-summary-face ((,class (:foreground ,strong :bold t :background ,subtle))))

   ;; Company
   `(company-tooltip                      ((,class (:background ,subtle :foreground ,foreground))))
   `(company-tooltip-selection            ((,class (:background ,popout))))
   `(company-tooltip-annotation           ((,class (:foreground ,foreground))))
   `(company-tooltip-annotation-selection ((,class (:foreground ,strong :bold t))))
   `(company-tooltip-common               ((,class (:foreground ,strong))))
   `(company-tooltip-common-selection     ((,class (:foreground ,strong :bold t))))
   `(company-scrollbar-bg                 ((,class (:background ,faded))))
   `(company-scrollbar-fg                 ((,class (:background ,foreground))))

   ;; Calendar
   `(calendar-today ((,class (:foreground ,foreground :bold t))))

   ;; Mode Line
   `(mode-line          ((,class ( :background ,background :overline ,strong
                                   :box ,(if -modeline-pad `(:line-width ,-modeline-pad :color ,foreground))))))
   `(mode-line-inactive ((,class ( :background ,background :foreground ,faded :overline ,subtle
                                   :box ,(if -modeline-pad `(:line-width ,-modeline-pad :color ,faded))))))
   `(mode-line-active ((,class :inherit mode-line)))
   `(mode-line-buffer-id ((,class :inherit bold)))
   `(mode-line-emphasis ((,class :inherit bold :foreground ,n/靛蓝)))
   `(mode-line-highlight ((,class :inherit highlight :box (:line-width -1 :style pressed-button))))
   `(mood-line-modified ((,class :foreground ,n/粉萅)))
   `(mood-line-status-error ((,class :inherit bold :foreground ,n/红火)))
   `(mood-line-status-info ((,class :foreground ,n/靛蓝)))
   `(mood-line-status-neutral ((,class :foreground ,n/黛霜)))
   `(mood-line-status-success ((,class :inherit success)))
   `(mood-line-status-warning ((,class :inherit bold :foreground ,n/鸭黄)))
   `(mood-line-unimportant ((,class :foreground ,faded)))

   ;;tab-bar
   `(tab-bar                    ((,class (:background ,subtle))))
   `(tab-bar-tab-group-current  ((,class ())))
   `(tab-bar-tab                ((,class (:inverse-video t :bold t))))
   `(tab-bar-tab-group-inactive ((,class ())))
   `(tab-bar-tab-inactive       ((,class (:inherit shadow))))

   ;; solaire Mode
   `(solaire-default-face       ((,class (:inherit default :background ,highlight))))

   ;; Orderless
   `(orderless-match-face-0     ((,class (:foreground ,n/绿松 :bold t))))
   `(orderless-match-face-1     ((,class (:foreground ,n/紫丁 :bold t))))
   `(orderless-match-face-2     ((,class (:foreground ,n/茶棕 :bold t))))
   `(orderless-match-face-3     ((,class (:foreground ,n/墨缟 :bold t))))

   ;; Eshell
   `(eshell-prompt              ((,class (:foreground ,popout :bold t))))

   ;; telega
   `(telega-msg-inline-reply    ((,class (:foreground ,faded))))
   `(telega-msg-heading         ((,class (:underline t))))

   ;; ediff
   `(ediff-current-diff-A        ((,class :background ,bg-diff-focus-removed :foreground ,fg-diff-focus-removed)))
   `(ediff-current-diff-Ancestor ((,class :background ,bg-special-cold :foreground ,fg-special-cold)))
   `(ediff-current-diff-B        ((,class :background ,bg-diff-added :foreground ,fg-diff-added)))
   `(ediff-current-diff-C        ((,class :background ,bg-diff-changed :foreground ,fg-diff-changed)))
   `(ediff-even-diff-A           ((,class :background ,subtle)))
   `(ediff-even-diff-Ancestor    ((,class :background ,subtle)))
   `(ediff-even-diff-B           ((,class :background ,subtle)))
   `(ediff-even-diff-C           ((,class :background ,subtle)))
   `(ediff-fine-diff-A           ((,class :background ,bg-diff-refine-removed :foreground ,fg-diff-refine-removed)))
   `(ediff-fine-diff-Ancestor    ((,class :background ,cyan-refine-bg :foreground ,cyan-refine-fg)))
   `(ediff-fine-diff-B           ((,class :background ,bg-diff-refine-added :foreground ,fg-diff-refine-added)))
   `(ediff-fine-diff-C           ((,class :background ,bg-diff-refine-changed :foreground ,fg-diff-refine-changed)))
   `(ediff-odd-diff-A            ((,class :inherit ediff-even-diff-A)))
   `(ediff-odd-diff-Ancestor     ((,class :inherit ediff-even-diff-Ancestor)))
   `(ediff-odd-diff-B            ((,class :inherit ediff-even-diff-B)))
   `(ediff-odd-diff-C            ((,class :inherit ediff-even-diff-C)))

   ;; whitespace-line
   `(whitespace-line ((,class :background ,n/鸭黄 :foreground ,n/灰墨)))

   ;; vc (vc-dir.el, vc-hooks.el)
   `(vc-dir-directory         ((,class :foreground "blue")))
   `(vc-dir-file              ((,class :foreground ,foreground)))
   `(vc-dir-header            ((,class :foreground ,n/靛蓝)))
   `(vc-dir-header-value      ((,class :foreground ,n/粉萅)))
   `(vc-dir-mark-indicator    ((,class :foreground ,n/紫丁)))
   `(vc-dir-status-edited     ((,class :foreground ,n/茶棕)))
   `(vc-dir-status-ignored    ((,class :inherit shadow)))
   `(vc-dir-status-up-to-date ((,class :foreground ,n/酱貝)))
   `(vc-dir-status-warning    ((,class :inherit error)))
   `(vc-conflict-state        ((,class :inherit bold :foreground ,n/红火)))
   `(vc-edited-state          ((,class :foreground ,n/茶棕)))
   `(vc-locally-added-state   ((,class :foreground ,n/酱貝)))
   `(vc-locked-state          ((,class :foreground ,n/靛蓝)))
   `(vc-missing-state         ((,class :inherit italic :foreground ,n/粉萅)))
   `(vc-needs-update-state    ((,class :inherit italic :foreground ,n/鱼肚)))
   `(vc-removed-state         ((,class :foreground ,n/红火)))
   `(vc-state-base            ((,class :foreground ,foreground)))
   `(vc-up-to-date-state      ((,class :foreground ,popout)))

    ;; vertico
   `(vertico-current ((,class :background ,subtle)))

   ;; vertico-quick
   `(vertico-quick1 ((,class :background ,n/粉萅 :foreground ,foreground)))
   `(vertico-quick2 ((,class :background ,n/靛蓝 :foreground ,foreground)))

   ;; git-gutter
   `(git-gutter:added ((,class :background ,n/松绿)))
   `(git-gutter:deleted ((,class :background ,n/红火)))
   `(git-gutter:modified ((,class :background ,popout)))
   `(git-gutter:separator ((,class :background ,salient)))
   `(git-gutter:unchanged ((,class :background ,n/紫丁)))

   ;; git-gutter-fr
   `(git-gutter-fr:added ((,class :background ,n/松绿)))
   `(git-gutter-fr:deleted ((,class :background ,n/红火)))
   `(git-gutter-fr:modified ((,class :background ,popout)))

   ;; Imenu-ist
   `(imenu-list-entry-subalist-face-0 ((,class (:foreground ,strong :weight bold :underline t))))
   `(imenu-list-entry-subalist-face-1 ((,class (:foreground ,salient :weight bold :underline t))))
   `(imenu-list-entry-subalist-face-2 ((,class (:foreground ,popout :weight bold :underline t))))
   `(imenu-list-entry-subalist-face-3 ((,class (:foreground ,critical :weight bold :underline t))))
   `(imenu-list-entry-face-0          ((,class (:foreground ,strong))))
   `(imenu-list-entry-face-1          ((,class (:foreground ,salient))))
   `(imenu-list-entry-face-2          ((,class (:foreground ,popout))))
   `(imenu-list-entry-face-3          ((,class (:foreground ,critical))))))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'nano)

;;; nano-theme.el ends here
