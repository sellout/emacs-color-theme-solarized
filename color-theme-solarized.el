;;; Author: Ethan Schoonover, Solarized; Greg Pfeil, Emacs adaptation
;;; URL: http://ethanschoonover.com/solarized

;;; This file is not (YET) part of GNU Emacs.

;;; # Usage

;;; 1. Install the color-theme package
;;;   (http://www.emacswiki.org/cgi-bin/wiki/ColorTheme)
;;; 2. Load this file
;;; 3. M-x color-theme-solarized-[dark|light]

(eval-when-compile
  (require 'color-theme))

(defcustom color-theme-solarized-degrade nil
  "For test purposes only; forces Solarized to use the 256 degraded color mode to test the approximate color values for accuracy."
  :type 'boolean
  :group 'color-theme-solarized)

(defvar color-theme-solarized-colors
  ;; name    ideal     256
  '((base03  "#002b36" "#1c1c1c")
    (base02  "#073642" "#262626")
    (base01  "#586e75" "#4e4e4e")
    (base00  "#657b83" "#585858")
    (base0   "#839496" "#808080")
    (base1   "#93a1a1" "#8a8a8a")
    (base2   "#eee8d5" "#d7d7af")
    (base3   "#fdf6e3" "#ffffd7")
    (yellow  "#b58900" "#af8700")
    (orange  "#cb4b16" "#d75f00")
    (red     "#dc322f" "#af0000")
    (magenta "#d33682" "#af005f")
    (violet  "#6c71c4" "#5f5faf")
    (blue    "#268bd2" "#0087ff")
    (cyan    "#2aa198" "#00afaf")
    (green   "#859900" "#5f8700"))
  "This is a table of all the colors used by the Solarized color theme. Each
   column is a different set, one of which will be chosen based on term
   capabilities, etc.")

;;;###autoload
(defun color-theme-solarized (mode)
  "Color theme by Ethan Schoonover, created 2011-03-24.
Ported to Emacs by Greg Pfeil, http://ethanschoonover.com/solarized."
  (interactive "Slight or dark? ")
  (flet ((find-color (name)
           (let ((index (if (or (<= (display-color-cells) 256)
                                color-theme-solarized-degrade)
                            2
                          1)))
             (nth index (assoc name color-theme-solarized-colors)))))
    (let ((base03  (find-color 'base03))
          (base02  (find-color 'base02))
          (base01  (find-color 'base01))
          (base00  (find-color 'base00))
          (base0   (find-color 'base0))
          (base1   (find-color 'base1))
          (base2   (find-color 'base2))
          (base3   (find-color 'base3))
          (yellow  (find-color 'yellow))
          (orange  (find-color 'orange))
          (red     (find-color 'red))
          (magenta (find-color 'magenta))
          (violet  (find-color 'violet))
          (blue    (find-color 'blue))
          (cyan    (find-color 'cyan))
          (green   (find-color 'green)))
      (when (eq 'light mode)
        (rotatef base03 base3)
        (rotatef base02 base2)
        (rotatef base01 base1)
        (rotatef base00 base0))
      (color-theme-install
       `(color-theme-solarized
         ((foreground-color . ,base0)
          (background-color . ,base03)
          (background-mode . ,mode)
          (cursor-color . ,base0))
         ;; basic
         (default ((t (:foreground ,base0))))
         (cursor
          ((t (:foreground ,base0 :background ,base03 :inverse-video t))))
         (escape-glyph-face ((t (:foreground ,red))))
         (fringe ((t (:foreground ,base01 :background ,base02))))
         (header-line ((t (:foreground ,base0 :background ,base2))))
         (highlight ((t (:background ,base02))))
         (isearch ((t (:foreground ,yellow :inverse-video t))))
         (menu ((t (:foreground ,base0 :background ,base02))))
         (minibuffer-prompt ((t (:foreground ,blue))))
         (mode-line
          ((t (:foreground ,base1 :background ,base02
                           :box (:line-width 1 :color ,base1)))))
         (mode-line-buffer-id ((t (:foreground ,base1))))
         (mode-line-inactive
          ((t (:foreground ,base0  :background ,base02
                           :box (:line-width 1 :color ,base02)))))
         (region ((t (:background ,base02))))
         (secondary-selection ((t (:background ,base02))))
         (trailing-whitespace ((t (:foreground ,red :inverse-video t))))
         (vertical-border ((t (:foreground ,base0))))
         ;; comint
         (comint-highlight-prompt ((t (:foreground ,blue))))
         ;; compilation
         (compilation-info ((t (:foreground ,green :bold t))))
         (compilation-warning ((t (:foreground ,orange :bold t))))
         ;; customize
         (custom-button
          ((t (:background ,base02
                           :box (:line-width 2 :style released-button)))))
         (custom-button-mouse ((t (:inherit custom-button :foreground ,base1))))
         (custom-button-pressed
          ((t (:inherit custom-button-mouse
                        :box (:line-width 2 :style pressed-button)))))
         (custom-comment-tag ((t (:background ,base02))))
         (custom-comment-tag ((t (:background ,base02))))
         (custom-documentation ((t (:inherit default))))
         (custom-group-tag ((t (:foreground ,orange :bold t))))
         (custom-link ((t (:foreground ,violet))))
         (custom-state ((t (:foreground ,green))))
         (custom-variable-tag ((t (:foreground ,orange :bold t))))
         ;; diff
         (diff-added ((t (:foreground ,green :inverse-video t))))
         (diff-changed ((t (:foreground ,yellow :inverse-video t))))
         (diff-removed ((t (:foreground ,red :inverse-video t))))
         (diff-header ((t (:background ,base01))))
         (diff-file-header
          ((t (:background ,base1 :foreground ,base01 :bold t))))
         (diff-refine-change ((t (:background ,base1))))
         ;; emacs-wiki
         (emacs-wiki-bad-link-face ((t (:foreground ,red :underline t))))
         (emacs-wiki-link-face ((t (:foreground ,blue :underline t))))
         (emacs-wiki-verbatim-face ((t (:foreground ,base00 :underline t))))
         ;; font-lock
         (font-lock-builtin-face ((t (:foreground ,green))))
         (font-lock-comment-face ((t (:foreground ,base01 :italic t))))
         (font-lock-constant-face ((t (:foreground ,cyan))))
         (font-lock-function-name-face ((t (:foreground ,blue))))
         (font-lock-keyword-face ((t (:foreground ,green))))
         (font-lock-string-face ((t (:foreground ,cyan))))
         (font-lock-type-face ((t (:foreground ,yellow))))
         (font-lock-variable-name-face ((t (:foreground ,blue))))
         (font-lock-warning-face ((t (:foreground ,red :bold t))))
         (font-lock-doc-face ((t (:foreground ,cyan :italic t))))
         ;; info
         (info-xref ((t (:foreground ,blue :underline t))))
         (info-xref-visited ((t (:inherit info-xref :foreground ,magenta))))
         ;; org
         (org-hide ((t (:foreground ,base03))))
         (org-todo ((t (:foreground ,red :bold t))))
         (org-done ((t (:foreground ,green :bold t))))
         ;; show-paren
         (show-paren-match-face ((t (:background ,cyan :foreground ,base3))))
         (show-paren-mismatch-face ((t (:background ,red :foreground ,base3))))
         ;; extra modules
         ;; -------------
         ;; gnus
         (gnus-cite-1 ((t (:foreground ,magenta))))
         (gnus-cite-2 ((t (:foreground ,base2))))
         (gnus-cite-3 ((t (:foreground ,base3))))
         (gnus-cite-4 ((t (:foreground ,base1))))
         (gnus-cite-5 ((t (:foreground ,magenta))))
         (gnus-cite-6 ((t (:foreground ,base2))))
         (gnus-cite-7 ((t (:foreground ,base3))))
         (gnus-cite-8 ((t (:foreground ,base1))))
         (gnus-cite-9 ((t (:foreground ,base2))))
         (gnus-cite-10 ((t (:foreground ,base3))))
         (gnus-cite-11 ((t (:foreground ,blue))))
         (gnus-group-mail-1 ((t (:foreground ,base3 :bold t))))
         (gnus-group-mail-1-empty ((t (:foreground ,base3))))
         (gnus-group-mail-2 ((t (:foreground ,base2 :bold t))))
         (gnus-group-mail-2-empty ((t (:foreground ,base2))))
         (gnus-group-mail-3 ((t (:foreground ,magenta :bold t))))
         (gnus-group-mail-3-empty ((t (:foreground ,magenta))))
         (gnus-group-mail-low ((t (:foreground ,base00 :bold t))))
         (gnus-group-mail-low-empty ((t (:foreground ,base00))))
         (gnus-group-news-1 ((t (:foreground ,base1 :bold t))))
         (gnus-group-news-1-empty ((t (:foreground ,base1))))
         (gnus-group-news-2 ((t (:foreground ,blue :bold t))))
         (gnus-group-news-2-empty ((t (:foreground ,blue))))
         (gnus-group-news-low ((t (:foreground ,violet :bold t))))
         (gnus-group-news-low-empty ((t (:foreground ,violet))))
         (gnus-header-content ((t (:foreground ,cyan :italic t))))
         (gnus-header-from ((t (:foreground ,base2))))
         (gnus-header-name ((t (:foreground ,blue))))
         (gnus-header-newsgroups ((t (:foreground ,green :italic t))))
         (gnus-header-subject ((t (:foreground ,base1))))
         (gnus-server-agent ((t (:foreground ,base3 :bold t))))
         (gnus-server-closed ((t (:foreground ,base1 :italic t))))
         (gnus-server-denied ((t (:foreground ,base2 :bold t))))
         (gnus-server-offline ((t (:foreground ,green :bold t))))
         (gnus-server-opened ((t (:foreground ,cyan :bold t))))
         (gnus-splash ((t (:foreground ,base2))))
         (gnus-summary-high-ancient ((t (:foreground ,magenta :bold t))))
         (gnus-summary-high-read ((t (:foreground ,base1 :bold t))))
         (gnus-summary-high-ticked ((t (:foreground ,base3 :bold t))))
         (gnus-summary-high-undownloaded ((t (:foreground ,base2 :bold t))))
         (gnus-summary-low-ancient ((t (:foreground ,magenta :italic t))))
         (gnus-summary-low-read ((t (:foreground ,base1 :italic t))))
         (gnus-summary-low-ticked ((t (:foreground ,base3 :italic t))))
         (gnus-summary-low-undownloaded ((t (:foreground ,base2 :italic t))))
         (gnus-summary-normal-ancient ((t (:foreground ,magenta))))
         (gnus-summary-normal-read ((t (:foreground ,base1))))
         (gnus-summary-normal-ticked ((t (:foreground ,base3))))
         (gnus-summary-normal-undownloaded ((t (:foreground ,base2))))
         ;; Message
         (message-mml ((t (:foreground ,blue))))
         (message-cited-text ((t (:foreground ,base2))))
         (message-separator ((t (:foreground ,base3))))
         (message-header-xheader ((t (:foreground ,violet))))
         (message-header-name ((t (:foreground ,cyan))))
         (message-header-other ((t (:foreground ,red))))
         (message-header-newsgroups
          ((t (:foreground ,yellow :bold t :italic t))))
         (message-header-subject ((t (:foreground ,base00))))
         (message-header-cc ((t (:foreground ,green :bold t))))
         (message-header-to ((t (:foreground ,base1 :bold t)))))))))

;;;###autoload
(defun color-theme-solarized-dark ()
  (interactive)
  (color-theme-solarized 'dark))

;;;###autoload
(defun color-theme-solarized-light ()
  (interactive)
  (color-theme-solarized 'light))

(add-to-list 'color-themes
             '(color-theme-solarized-light
               "Solarized Light"
               "Ethan Schoonover & Greg Pfeil <greg@technomadic.org>"))
(add-to-list 'color-themes
             '(color-theme-solarized-dark
               "Solarized Dark"
               "Ethan Schoonover & Greg Pfeil <greg@technomadic.org>"))

(provide 'color-theme-solarized)
