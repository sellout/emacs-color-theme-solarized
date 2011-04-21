;;; solarized-theme.el --- Custom face theme for Emacs

;; Copyright © 2011 Gwenhael Le Moine <gwenhael.le.moine@gmail.com>

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Usage:
;;; 1. Copy this file under custom-theme-load-path (typically ~/.emacs.d/)
;;; 2. load the theme with M-x load-theme

;;; Code:

(deftheme solarized
  "based on http://ethanschoonover.com/solarized")

(let* ((base3   "#002b36")
       (base2   "#073642")
       (base1   "#586e75")
       (base0   "#657b83")
       (base00  "#839496")
       (base01  "#93a1a1")
       (base02  "#eee8d5")
       (base03  "#fdf6e3")
       (yellow  "#b58900")
       (orange  "#cb4b16")
       (red     "#dc322f")
       (magenta "#d33682")
       (violet  "#6c71c4")
       (blue    "#268bd2")
       (cyan    "#2aa198")
       (green   "#859900"))
  (custom-theme-set-faces
   'solarized
   `(default ((t (:background ,base3 :foreground ,base03))))
   `(cursor ((t (:background ,base1 :foreground ,base02))))
   `(region ((t (:background ,base2 :foreground ,base01))))
   `(mode-line ((t (:background ,base1 :foreground ,base02))))
   `(mode-line-inactive ((t (:background ,base2 :foreground ,base01))))
   `(fringe ((t (:background ,base2))))
   `(minibuffer-prompt ((t (:background ,base3 :foreground ,blue :weight bold))))
   `(font-lock-builtin-face ((t (:foreground ,green))))
   `(font-lock-comment-face ((t (:slant italic :foreground ,base1))))
   `(font-lock-constant-face ((t (:foreground ,cyan))))
   `(font-lock-function-name-face ((t (:foreground ,blue))))
   `(font-lock-keyword-face ((t (:foreground ,green))))
   `(font-lock-string-face ((t (:foreground ,cyan))))
   `(font-lock-type-face ((t (:foreground ,yellow))))
   `(font-lock-variable-name-face ((t (:foreground ,blue))))
   `(font-lock-warning-face ((t (:foreground ,red :weight bold))))
   `(isearch ((t (:background ,base2 :foreground ,base01))))
   `(lazy-highlight ((t (:background ,base2 :foreground ,base00))))
   `(link ((t (:foreground ,violet :underline t))))
   `(link-visited ((t (:foreground ,magenta :underline t))))
   `(button ((t (:background ,base1 :underline t))))
   `(header-line ((t (:background ,base2 :foreground ,base01))))
   `(hl-line ((t (:background ,base2))))
   ;; from color-theme version
   `(escape-glyph-face ((t (:foreground ,red))))
   `(highlight ((t (:background ,base02))))
   `(menu ((t (:foreground ,base0 :background ,base02))))
   `(mode-line-buffer-id ((t (:foreground ,base1))))
   `(secondary-selection ((t (:background ,base02))))
   `(trailing-whitespace ((t (:foreground ,red :inverse-video t))))
   `(vertical-border ((t (:foreground ,base0))))
   ;; compilation
   `(compilation-info ((t (:forground ,green :bold t))))
   `(compilation-warning ((t (:foreground ,orange :bold t))))
   ;; customize
   `(custom-button
     ((t (:background ,base02 :box (:line-width 2 :style released-button)))))
   `(custom-button-mouse ((t (:inherit custom-button :foreground ,base1))))
   `(custom-button-pressed
     ((t (:inherit custom-button-mouse
		   :box (:line-width 2 :style pressed-button)))))
   `(custom-comment-tag ((t (:background ,base02))))
   `(custom-comment-tag ((t (:background ,base02))))
   `(custom-documentation ((t (:inherit default))))
   `(custom-group-tag ((t (:foreground ,orange :bold t))))
   `(custom-link ((t (:foreground ,violet))))
   `(custom-state ((t (:foreground ,green))))
   `(custom-variable-tag ((t (:foreground ,orange :bold t))))
   ;; diff
   `(diff-added ((t (:foreground ,green :inverse-video t))))
   `(diff-changed ((t (:foreground ,yellow :inverse-video t))))
   `(diff-removed ((t (:foreground ,red :inverse-video t))))
   ;; emacs-wiki
   `(emacs-wiki-bad-link-face ((t (:foreground ,red :underline t))))
   `(emacs-wiki-link-face ((t (:foreground ,blue :underline t))))
   `(emacs-wiki-verbatim-face ((t (:foreground ,base00 :underline t))))
   ;; info
   `(info-xref ((t (:foreground ,blue :underline t))))
   `(info-xref-visited ((t (:inherit info-xref :foreground ,magenta))))
   ;; org
   `(org-hide ((t (:foreground ,base03))))
   `(org-todo ((t (:foreground ,red :bold t))))
   `(org-done ((t (:foreground ,green :bold t))))
   ;; show-paren
   `(show-paren-match-face ((t (:background ,cyan :foreground ,base3))))
   `(show-paren-mismatch-face ((t (:background ,red :foreground ,base3))))
   ;; extra modules
   ;; -------------
   ;; gnus
   `(gnus-cite-1 ((t (:foreground ,magenta))))
   `(gnus-cite-2 ((t (:foreground ,base2))))
   `(gnus-cite-3 ((t (:foreground ,base3))))
   `(gnus-cite-4 ((t (:foreground ,base1))))
   `(gnus-cite-5 ((t (:foreground ,magenta))))
   `(gnus-cite-6 ((t (:foreground ,base2))))
   `(gnus-cite-7 ((t (:foreground ,base3))))
   `(gnus-cite-8 ((t (:foreground ,base1))))
   `(gnus-cite-9 ((t (:foreground ,base2))))
   `(gnus-cite-10 ((t (:foreground ,base3))))
   `(gnus-cite-11 ((t (:foreground ,blue))))
   `(gnus-group-mail-1 ((t (:foreground ,base3 :bold t))))
   `(gnus-group-mail-1-empty ((t (:foreground ,base3))))
   `(gnus-group-mail-2 ((t (:foreground ,base2 :bold t))))
   `(gnus-group-mail-2-empty ((t (:foreground ,base2))))
   `(gnus-group-mail-3 ((t (:foreground ,magenta :bold t))))
   `(gnus-group-mail-3-empty ((t (:foreground ,magenta))))
   `(gnus-group-mail-low ((t (:foreground ,base00 :bold t))))
   `(gnus-group-mail-low-empty ((t (:foreground ,base00))))
   `(gnus-group-news-1 ((t (:foreground ,base1 :bold t))))
   `(gnus-group-news-1-empty ((t (:foreground ,base1))))
   `(gnus-group-news-2 ((t (:foreground ,blue :bold t))))
   `(gnus-group-news-2-empty ((t (:foreground ,blue))))
   `(gnus-group-news-low ((t (:foreground ,violet :bold t))))
   `(gnus-group-news-low-empty ((t (:foreground ,violet))))
   `(gnus-header-content ((t (:foreground ,cyan :italic t))))
   `(gnus-header-from ((t (:foreground ,base2))))
   `(gnus-header-name ((t (:foreground ,blue))))
   `(gnus-header-newsgroups ((t (:foreground ,green :italic t))))
   `(gnus-header-subject ((t (:foreground ,base1))))
   `(gnus-server-agent ((t (:foreground ,base3 :bold t))))
   `(gnus-server-closed ((t (:foreground ,base1 :italic t))))
   `(gnus-server-denied ((t (:foreground ,base2 :bold t))))
   `(gnus-server-offline ((t (:foreground ,green :bold t))))
   `(gnus-server-opened ((t (:foreground ,cyan :bold t))))
   `(gnus-splash ((t (:foreground ,base2))))
   `(gnus-summary-high-ancient ((t (:foreground ,magenta :bold t))))
   `(gnus-summary-high-read ((t (:foreground ,base1 :bold t))))
   `(gnus-summary-high-ticked ((t (:foreground ,base3 :bold t))))
   `(gnus-summary-high-undownloaded ((t (:foreground ,base2 :bold t))))
   `(gnus-summary-low-ancient ((t (:foreground ,magenta :italic t))))
   `(gnus-summary-low-read ((t (:foreground ,base1 :italic t))))
   `(gnus-summary-low-ticked ((t (:foreground ,base3 :italic t))))
   `(gnus-summary-low-undownloaded ((t (:foreground ,base2 :italic t))))
   `(gnus-summary-normal-ancient ((t (:foreground ,magenta))))
   `(gnus-summary-normal-read ((t (:foreground ,base1))))
   `(gnus-summary-normal-ticked ((t (:foreground ,base3))))
   `(gnus-summary-normal-undownloaded ((t (:foreground ,base2))))
   ;; Message
   `(message-mml ((t (:foreground ,blue))))
   `(message-cited-text ((t (:foreground ,base2))))
   `(message-separator ((t (:foreground ,base3))))
   `(message-header-xheader ((t (:foreground ,violet))))
   `(message-header-name ((t (:foreground ,cyan))))
   `(message-header-other ((t (:foreground ,red))))
   `(message-header-newsgroups
     ((t (:foreground ,yellow :bold t :italic t))))
   `(message-header-subject ((t (:foreground ,base00))))
   `(message-header-cc ((t (:foreground ,green :bold t))))
   `(message-header-to ((t (:foreground ,base1 :bold t))))))

(provide-theme 'solarized)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; solarized-theme.el  ends here
