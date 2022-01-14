;;; nano-theme.el --- A theme split from nano-emacs  -*- lexical-binding: t; -*-

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
      (n/荼胡 (--l?d "#F3F9F1" "#FFFAE8"))
      (n/粉萅 (--l?d "#FF47D1" "#FF00FF"))
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

      (foreground (--l?d "#37474F" "#ECEFF4"))
      (background (--l?d "#FFFFFF" "#2E3440"))
      (highlight  (--l?d "#FAFAFA" "#3B4252"))
      (critical   (--l?d "#FF6F00" "#EBCB8B"))
      (salient    (--l?d "#673AB7" "#81A1C1"))
      (strong     (--l?d "#000000" "#ECEFF4"))
      (popout     (--l?d "#FFAB91" "#D08770"))
      (subtle     (--l?d "#ECEFF1" "#434C5E"))
      (faded      (--l?d "#B0BEC5" "#677691"))
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
   `(font-lock-comment-face           ((,class (:foreground ,faded))))
   `(font-lock-comment-delimiter-face ((,class (:foreground ,faded))))
   `(font-lock-keyword-face           ((,class (:foreground ,salient))))
   `(font-lock-string-face            ((,class (:foreground ,popout))))
   `(font-lock-doc-face               ((,class (:foreground ,faded))))
   `(font-lock-builtin-face           ((,class ())))
   `(font-lock-type-face              ((,class ())))
   `(font-lock-variable-name-face     ((,class ())))
   `(font-lock-constant-face          ((,class (:foreground ,salient))))
   `(font-lock-function-name-face     ((,class (:foreground ,strong :underline t))))
   `(font-lock-warning-face           ((,class ())))
   `(font-lock-preprocessor-face      ((,class ())))

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
