(eval-when-compile
  (require 'cl))

(defconst solarized-description
  "Color theme by Ethan Schoonover, created 2011-03-24.
Ported to Emacs by Greg Pfeil, http://ethanschoonover.com/solarized.")

(defcustom solarized-degrade nil
  "For test purposes only; when in GUI mode, forces Solarized to use the 256
degraded color mode to test the approximate color values for accuracy."
  :type 'boolean
  :group 'solarized)

(defcustom solarized-diff-mode 'normal
  "Sets the level of highlighting to use in diff-like modes."
  :type 'symbol
  :options '(high normal low)
  :group 'solarized)

(defcustom solarized-bold t
  "Stops Solarized from displaying bold when nil."
  :type 'boolean
  :group 'solarized)

(defcustom solarized-underline t
  "Stops Solarized from displaying underlines when nil."
  :type 'boolean
  :group 'solarized)

(defcustom solarized-italic t
  "Stops Solarized from displaying italics when nil."
  :type 'boolean
  :group 'solarized)

(defcustom solarized-contrast 'normal
  "Stick with normal! It's been carefully tested. Setting this option to high or
low does use the same Solarized palette but simply shifts some values up or
down in order to expand or compress the tonal range displayed."
  :type 'symbol
  :options '(high normal low)
  :group 'solarized)

(defcustom solarized-broken-srgb (if (and (eq system-type 'darwin)
                                          (eq window-system 'ns))
                                     t
                                   nil)
  "Emacs bug #8402 results in incorrect color handling on Macs. If this is t
\(the default on Macs), Solarized works around it with alternative colors.
However, these colors are not totally portable, so you may be able to edit
the \"Gen RGB\" column in solarized-definitions.el to improve them further."
  :type 'boolean
  :group 'solarized)

;; FIXME: The Generic RGB colors will actually vary from device to device, but
;;        hopefully these are closer to the intended colors than the sRGB values
;;        that Emacs seems to dislike
(defvar solarized-colors           ; ANSI(Solarized terminal)
  ;; name     sRGB      Gen RGB   256       16              8
  '((base03  "#002b36" "#042028" "#1c1c1c" "brightblack"   "black")
    (base02  "#073642" "#0a2832" "#262626" "black"         "black")
    (base01  "#586e75" "#465a61" "#585858" "brightgreen"   "green")
    (base00  "#657b83" "#52676f" "#626262" "brightyellow"  "yellow")
    (base0   "#839496" "#708183" "#808080" "brightblue"    "blue")
    (base1   "#93a1a1" "#81908f" "#8a8a8a" "brightcyan"    "cyan")
    (base2   "#eee8d5" "#e9e2cb" "#e4e4e4" "white"         "white")
    (base3   "#fdf6e3" "#fcf4dc" "#ffffd7" "brightwhite"   "white")
    (yellow  "#b58900" "#a57705" "#af8700" "yellow"        "yellow")
    (orange  "#cb4b16" "#bd3612" "#d75f00" "brightred"     "red")
    (red     "#dc322f" "#c60007" "#d70000" "red"           "red")
    (magenta "#d33682" "#c61b6e" "#af005f" "magenta"       "magenta")
    (violet  "#6c71c4" "#5859b7" "#5f5faf" "brightmagenta" "magenta")
    (blue    "#268bd2" "#2075c7" "#0087ff" "blue"          "blue")
    (cyan    "#2aa198" "#259185" "#00afaf" "cyan"          "cyan")
    (green   "#859900" "#728a05" "#5f8700" "green"         "green"))
  "This is a table of all the colors used by the Solarized color theme. Each
   column is a different set, one of which will be chosen based on term
   capabilities, etc.")

(defun solarized-color-definitions (mode)
  (flet ((find-color (name)
           (let* ((index (if window-system
                             (if solarized-degrade
                                 3
                               (if solarized-broken-srgb 2 1))
                           (case (display-color-cells)
                             (16 4)
                             (8  5)
                             (otherwise 3)))))
             (nth index (assoc name solarized-colors)))))
    (let ((base03      (find-color 'base03))
          (base02      (find-color 'base02))
          (base01      (find-color 'base01))
          (base00      (find-color 'base00))
          (base0       (find-color 'base0))
          (base1       (find-color 'base1))
          (base2       (find-color 'base2))
          (base3       (find-color 'base3))
          (yellow      (find-color 'yellow))
          (orange      (find-color 'orange))
          (red         (find-color 'red))
          (magenta     (find-color 'magenta))
          (violet      (find-color 'violet))
          (blue        (find-color 'blue))
          (cyan        (find-color 'cyan))
          (green       (find-color 'green))
          (bold        (if solarized-bold 'bold 'normal))
          (bright-bold (if solarized-bold 'normal 'bold))
          (underline   (if solarized-underline t nil))
          (opt-under   nil)
          (italic      (if solarized-italic 'italic 'normal)))
      (when (eq 'light mode)
        (rotatef base03 base3)
        (rotatef base02 base2)
        (rotatef base01 base1)
        (rotatef base00 base0))
      (let ((back base03))
        (cond ((< (display-color-cells) 16)
               (setf back nil))
              ((eq 'high solarized-contrast)
               (let ((orig-base3 base3))
                 (rotatef base01 base00 base0 base1 base2 base3)
                 (setf base3 orig-base3)))
              ((eq 'low solarized-contrast)
               (setf back      base02
                     opt-under t)))
        ;; NOTE: We try to turn an 8-color term into a 10-color term by not
        ;;       using default background and foreground colors, expecting the
        ;;       user to have the right colors set for them.
        (let ((bg-back   `(:background ,back))
              (bg-base03 `(:background ,base03))
              (bg-base02 `(:background ,base02))
              (bg-base01 `(:background ,base01))
              (bg-base00 `(:background ,base00))
              (bg-base0 `(:background ,base0))
              (bg-base1 `(:background ,base1))
              (bg-base2 `(:background ,base2))
              (bg-base3 `(:background ,base3))
              (bg-green `(:background ,green))
              (bg-yellow `(:background ,yellow))
              (bg-orange `(:background ,orange))
              (bg-red `(:background ,red))
              (bg-magenta `(:background ,magenta))
              (bg-violet `(:background ,violet))
              (bg-blue `(:background ,blue))
              (bg-cyan `(:background ,cyan))
              
              (fg-base03 `(:foreground ,base03))
              (fg-base02 `(:foreground ,base02))
              (fg-base01 `(:foreground ,base01))
              (fg-base00 `(:foreground ,base00))
              (fg-base0 `(:foreground ,(when (<= 16 (display-color-cells))
                                         base0)))
              (fg-base1 `(:foreground ,(when (<= 16 (display-color-cells))
                                         base1)))
              (fg-base2 `(:foreground ,base2))
              (fg-base3 `(:foreground ,base3))
              (fg-green `(:foreground ,green))
              (fg-yellow `(:foreground ,yellow))
              (fg-orange `(:foreground ,orange))
              (fg-red `(:foreground ,red))
              (fg-magenta `(:foreground ,magenta))
              (fg-violet `(:foreground ,violet))
              (fg-blue `(:foreground ,blue))
              (fg-cyan `(:foreground ,cyan))

              (fmt-none `(:weight normal :slant normal  :underline nil        :inverse-video nil))
              (fmt-bold `(:weight ,bold  :slant normal  :underline nil        :inverse-video nil))
              (fmt-bldi `(:weight ,bold                 :underline nil        :inverse-video nil))
              (fmt-undr `(:weight normal :slant normal  :underline ,underline :inverse-video nil))
              (fmt-undb `(:weight ,bold  :slant normal  :underline ,underline :inverse-video nil))
              (fmt-undi `(:weight normal                :underline ,underline :inverse-video nil))
              (fmt-uopt `(:weight normal :slant normal  :underline ,opt-under :inverse-video nil))
              ;; FIXME: not quite the same
              (fmt-curl `(:weight normal :slant normal  :underline t          :inverse-video nil))
              (fmt-ital `(:weight normal :slant ,italic :underline nil        :inverse-video nil))
              ;; FIXME: not quite the same
              (fmt-stnd `(:weight normal :slant normal  :underline nil        :inverse-video t))
              (fmt-revr `(:weight normal :slant normal  :underline nil        :inverse-video t))
              (fmt-revb `(:weight ,bold  :slant normal  :underline nil        :inverse-video t))
              (fmt-revbb `(:weight ,bright-bold :slant normal :underline nil  :inverse-video t))
              (fmt-revbbu `(:weight ,bright-bold :slant normal  :underline ,underline :inverse-video t)))
          `((;; basic
             (default ((t (,@fg-base0 ,@bg-back)))) ; Normal
             (cursor ((t (,@fg-base03 ,@bg-base0)))) ; Cursor
             (error ((t (,@fmt-bold ,@fg-red)))) ; Error
             (escape-glyph-face ((t (,@fg-red))))
             (fringe ((t (,@fg-base01 ,@bg-base02))))
             (linum ((t (,@fg-base01 ,@bg-base02))))
             (header-line ((t (,@fg-base0 ,@bg-base02 ,@fmt-revbb)))) ; Pmenu
             (highlight ((t (,@bg-base02))))
             (hl-line ((t (:underline ,opt-under ,@bg-base02)))) ; CursorLine
             (isearch ((t (,@fmt-stnd ,@fg-orange ,@bg-back)))) ; IncSearch
             (isearch-fail ((t (,@fmt-stnd ,@fg-orange ,@bg-back)))) ; IncSearch
             (lazy-highlight ((t (,@fmt-revr ,@fg-yellow ,@bg-back)))) ; Search
             (link ((t (,@fmt-undr ,@fg-violet))))
             (link-visited ((t (,@fmt-undr ,@fg-magenta))))
             (menu ((t (,@fg-base0 ,@bg-base02))))
             (minibuffer-prompt ((t (,@fmt-bold ,@fg-cyan)))) ; Question
             (mode-line  ; StatusLine
              ((t (,@fg-base1 ,@bg-base02 ,@fmt-revbb :box nil))))
             (mode-line-inactive ; StatusLineNC
              ((t (,@fg-base00 ,@bg-base02 ,@fmt-revbb :box nil))))
             (region ((t (,@fg-base01 ,@bg-base03 ,@fmt-revbb)))) ; Visual
             (secondary-selection ((t (,@bg-base02))))
             (shadow ((t (,@fg-base01))))
             (trailing-whitespace ((t (,@fmt-revr ,@fg-red))))
             (vertical-border ((t (,@fg-base0))))
             ;; comint
             (comint-highlight-prompt ((t (,@fg-blue))))
             ;; compilation
             (compilation-info ((t (,@fmt-bold ,@fg-green))))
             (compilation-warning ((t (,@fmt-bold ,@fg-orange))))
             ;; custom
             (custom-button
              ((t (,@fg-base1 ,@bg-base02
                              :box (:line-width 2 :style released-button)))))
             (custom-button-mouse
              ((t (,@fmt-revr ,@fg-base1 ,@bg-base02 :inherit custom-button))))
             (custom-button-pressed
              ((t (,@fmt-revr ,@fg-base1 ,@bg-base02
                              :box (:line-width 2 :style pressed-button)
                              :inherit custom-button-mouse))))
             (custom-changed ((t (,@fmt-revr ,@fg-blue ,@bg-base3))))
             (custom-comment ((t (,@fg-base1 ,@bg-base02))))
             (custom-comment-tag ((t (,@fg-base1 ,@bg-base02))))
             (custom-documentation ((t (:inherit default))))
             (custom-group-tag ((t (,@fg-base1))))
             (custom-group-tag-1 ((t (,fmt-bold ,@fg-base1))))
             (custom-invalid ((t (,@fmt-revr ,@fg-red ,@bg-back))))
             (custom-link ((t (,@fg-violet))))
             (custom-state ((t (,@fg-green))))
             (custom-variable-tag ((t (,@fg-base1))))
             ;; diff - DiffAdd, DiffChange, DiffDelete, and DiffText
             ,@(case solarized-diff-mode
                 (high
                  `((diff-added ((t (,@fmt-revr ,@fg-green))))
                    (diff-changed ((t (,@fmt-revr ,@fg-yellow))))
                    (diff-removed ((t (,@fmt-revr ,@fg-red))))
                    (diff-refine-change
                     ((t (,@fmt-revr ,@fg-blue ,@bg-back))))))
                 (low
                  `((diff-added ((t (,@fmt-undr ,@fg-green))))
                    (diff-changed ((t (,@fmt-undr ,@fg-yellow))))
                    (diff-removed ((t (,@fmt-bold ,@fg-red))))
                    (diff-refine-change
                     ((t (,@fmt-undr ,@fg-blue ,@bg-back))))))
                 (normal
                  (if window-system
                      `((diff-added ((t (,@fmt-bold ,@fg-green))))
                        (diff-changed ((t (,@fmt-bold ,@fg-yellow))))
                        (diff-removed ((t (,@fmt-bold ,@fg-red))))
                        (diff-refine-change
                         ((t (,@fmt-bold ,@fg-blue ,@bg-back)))))
                    `((diff-added ((t (,@fg-green))))
                      (diff-changed ((t (,@fg-yellow))))
                      (diff-removed ((t (,@fg-red))))
                      (diff-refine-change ((t (,@fg-blue ,@bg-back))))))))
             (diff-file-header ((t (,@bg-back))))
             (diff-header ((t (,@fg-base1 ,@bg-back))))
             ;; IDO
             (ido-only-match ((t (,@fg-green))))
             (ido-subdir ((t (,@fg-blue))))
             (ido-first-match ((t (,@fmt-bold ,@fg-green))))
             ;; emacs-wiki
             (emacs-wiki-bad-link-face ((t (,@fmt-undr ,@fg-red))))
             (emacs-wiki-link-face ((t (,@fmt-undr ,@fg-blue))))
             (emacs-wiki-verbatim-face ((t (,@fmt-undr ,@fg-base00))))
             ;; eshell
             (eshell-ls-archive ((t (,@fg-magenta))))
             (eshell-ls-backup ((t (,@fg-yellow))))
             (eshell-ls-clutter ((t (,@fg-orange))))
             (eshell-ls-directory ((t (,@fg-blue)))) ; Directory
             (eshell-ls-executable ((t (,@fg-green))))
             (eshell-ls-missing ((t (,@fg-red))))
             (eshell-ls-product ((t (,@fg-yellow))))
             (eshell-ls-readonly ((t (,@fg-base1))))
             (eshell-ls-special ((t (,@fg-violet))))
             (eshell-ls-symlink ((t (,@fg-cyan))))
             (eshell-ls-unreadable ((t (,@fg-base00))))
             (eshell-prompt ((t (,@fmt-bold ,@fg-green))))
             ;; font-lock
             (font-lock-builtin-face ((t (,@fmt-none ,@fg-green)))) ; Statement
             (font-lock-comment-face ((t (,@fmt-ital ,@fg-base01)))) ; Comment
             (font-lock-constant-face ((t (,@fmt-none ,@fg-cyan)))) ; Constant
             (font-lock-function-name-face ; Identifier
              ((t (,@fmt-none ,@fg-blue))))
             (font-lock-keyword-face ((t (,@fmt-none ,@fg-green)))) ; Statement
             (font-lock-string-face ((t (,@fmt-none ,@fg-cyan)))) ; Constant
             (font-lock-type-face ((t (,@fmt-none ,@fg-yellow)))) ; Type
             (font-lock-variable-name-face ; Identifier
              ((t (,@fmt-none ,@fg-blue))))
             (font-lock-warning-face ((t (,@fmt-bold ,@fg-red)))) ; Error
             (font-lock-doc-face ((t (,@fmt-ital ,@fg-base01)))) ; Comment
             (font-lock-doc-string-face  ; Comment (XEmacs-only)
              ((t (,@fmt-ital ,@fg-base01))))
             (font-lock-color-constant-face ((t (,@fmt-none ,@fg-green))))
             (font-lock-comment-delimiter-face ; Comment
              ((t (,@fmt-ital ,@fg-base01))))
             (font-lock-preprocessor-face ; PreProc
              ((t (,@fmt-none ,@fg-orange))))
             (font-lock-reference-face ((t (,@fmt-none ,@fg-cyan))))
             (font-lock-negation-char-face ((t (,@fmt-none ,@fg-red))))
             (font-lock-other-type-face ((t (,@fmt-ital ,@fg-blue))))
             (font-lock-regexp-grouping-construct
              ((t (,@fmt-none ,@fg-orange))))
             (font-lock-special-keyword-face ; Special
              ((t (,@fmt-none ,@fg-red))))
             (font-lock-exit-face ((t (,@fmt-none ,@fg-red))))
             (font-lock-other-emphasized-face ((t (,@fmt-bldi ,@fg-violet))))
             (font-lock-regexp-grouping-backslash
              ((t (,@fmt-none ,@fg-yellow))))
             ;; info
             (info-xref ((t (,@fmt-undr ,@fg-blue))))
             (info-xref-visited ((t (,@fg-magenta :inherit info-xref))))
             ;; org
             (org-hide ((t (,@fg-base03))))
             (org-todo ((t (,@fmt-bold ,@fg-base03 ,@bg-red))))
             (org-done ((t (,@fmt-bold ,@fg-green))))
             (org-todo-kwd-face ((t (,@fg-red ,@bg-base03))))
             (org-done-kwd-face ((t (,@fg-green ,@bg-base03))))
             (org-project-kwd-face ((t (,@fg-violet ,@bg-base03))))
             (org-waiting-kwd-face ((t (,@fg-orange ,@bg-base03))))
             (org-someday-kwd-face ((t (,@fg-blue ,@bg-base03))))
             (org-started-kwd-face ((t (,@fg-yellow ,@bg-base03))))
             (org-cancelled-kwd-face ((t (,@fg-green ,@bg-base03))))
             (org-delegated-kwd-face ((t (,@fg-cyan ,@bg-base03))))
             ;; table
             (table-cell ((t (,@fmt-none ,@fg-base0 ,@bg-back))))
             ;; outline - pandocBlockQuoteLeader*
             (outline-1 ((t (,@fmt-none ,@fg-blue))))
             (outline-2 ((t (,@fmt-none ,@fg-cyan))))
             (outline-3 ((t (,@fmt-none ,@fg-yellow))))
             (outline-4 ((t (,@fmt-none ,@fg-red))))
             (outline-5 ((t (,@fmt-none ,@fg-base0))))
             (outline-6 ((t (,@fmt-none ,@fg-base01))))
             (outline-7 ((t (,@fmt-none ,@fg-orange))))
             (outline-8 ((t (,@fmt-none ,@fg-violet))))
             ;; speedbar
             (speedbar-button-face ((t (,@fmt-none ,@fg-base1))))
             (speedbar-directory-face ((t (,@fmt-none ,@fg-orange))))
             (speedbar-file-face ((t (,@fmt-none ,@fg-green))))
             (speedbar-highlight-face ((t (,@bg-base02))))
             (speedbar-selected-face ((t (,@fmt-undr ,@fg-yellow))))
             (speedbar-separator-face ((t (,@fmt-stnd))))
             (speedbar-tag-face ((t (,@fmt-none ,@fg-blue))))
             ;; show-paren - MatchParen
             (show-paren-match ((t (,@fmt-bold ,@fg-cyan ,@bg-base02))))
             (show-paren-mismatch ((t (,@fmt-bold ,@fg-red ,@bg-base01))))
             ;; widgets
             (widget-field
              ((t (,@fg-base1 ,@bg-base02 :box (:line-width 1)
                              :inherit default))))
             (widget-single-line-field ((t (:inherit widget-field))))
             ;; extra modules
             ;; -------------
             ;; Flymake
             (flymake-errline ((t (,@fmt-revr ,@fg-red ,@bg-back)))) ; ErrorMsg
             (flymake-warnline ; WarningMsg
              ((t (,@fmt-bold ,@fg-red ,@bg-back))))
             ;; column-marker
             (column-marker-1 ((t (,@bg-base01))))
             (column-marker-2 ((t (,@bg-cyan))))
             (column-marker-3 ((t (,@bg-violet))))
             ;; jabber
             (jabber-activity-face ((t (,@fmt-bold ,@fg-red))))
             (jabber-activity-personal-face ((t (,@fmt-bold ,@fg-blue))))
             (jabber-chat-error ((t (,@fmt-bold ,@fg-red))))
             (jabber-chat-prompt-foreign ((t (,@fmt-bold ,@fg-red))))
             (jabber-chat-prompt-local ((t (,@fmt-bold ,@fg-blue))))
             (jabber-chat-prompt-system ((t (,@fmt-bold ,@fg-green))))
             (jabber-chat-text-foreign ((t (,@fg-base1))))
             (jabber-chat-text-local ((t (,@fg-base0))))
             (jabber-chat-rare-time-face ((t (,@fmt-undr ,@fg-green))))
             (jabber-roster-user-away ((t (,@fmt-ital ,@fg-green))))
             (jabber-roster-user-chatty ((t (,@fmt-bold ,@fg-orange))))
             (jabber-roster-user-dnd ((t (,@fmt-ital ,@fg-red))))
             (jabber-roster-user-error ((t (:weight light :slant italic ,@fg-red))))
             (jabber-roster-user-offline ((t (,@fg-base01))))
             (jabber-roster-user-online ((t (,@fmt-bold ,@fg-blue))))
             (jabber-roster-user-xa ((t (,@fmt-ital ,@fg-magenta))))
             ;; gnus - these are taken from mutt, not VIM
             (gnus-cite-1 ((t (,@fmt-none ,@fg-blue)))) ; quoted
             (gnus-cite-2 ((t (,@fmt-none ,@fg-cyan)))) ; quoted1
             (gnus-cite-3 ((t (,@fmt-none ,@fg-yellow)))) ; quoted2
             (gnus-cite-4 ((t (,@fmt-none ,@fg-red)))) ; quoted3
             (gnus-cite-5 ((t (,@fmt-none ,@fg-orange)))) ; quoted4
             (gnus-cite-6 ((t (,@fmt-none ,@fg-violet))))
             (gnus-cite-7 ((t (,@fmt-none ,@fg-green))))
             (gnus-cite-8 ((t (,@fmt-none ,@fg-magenta))))
             (gnus-cite-9 ((t (,@fmt-none ,@fg-base00))))
             (gnus-cite-10 ((t (,@fmt-none ,@fg-base01))))
             (gnus-cite-11 ((t (,@fmt-none ,@fg-base02))))
             (gnus-group-mail-1 ((t (,@fmt-bold ,@fg-base3))))
             (gnus-group-mail-1-empty ((t (,@fg-base3))))
             (gnus-group-mail-2 ((t (,@fmt-bold ,@fg-base2))))
             (gnus-group-mail-2-empty ((t (,@fg-base2))))
             (gnus-group-mail-3 ((t (,@fmt-bold ,@fg-magenta))))
             (gnus-group-mail-3-empty ((t (,@fg-magenta))))
             (gnus-group-mail-low ((t (,@fmt-bold ,@fg-base00))))
             (gnus-group-mail-low-empty ((t (,@fg-base00))))
             (gnus-group-news-1 ((t (,@fmt-bold ,@fg-base1))))
             (gnus-group-news-1-empty ((t (,@fg-base1))))
             (gnus-group-news-2 ((t (,@fmt-bold ,@fg-blue))))
             (gnus-group-news-2-empty ((t (,@fg-blue))))
             (gnus-group-news-low ((t (,@fmt-bold ,@fg-violet))))
             (gnus-group-news-low-empty ((t (,@fg-violet))))
             (gnus-emphasis-highlight-words ; highlight
              ((t (,@fmt-none ,fg-yellow))))
             (gnus-header-content ((t (,@fmt-none ,@fg-base01)))) ; hdrdefault
             (gnus-header-from ((t (,@fmt-none ,@fg-base00)))) ; header ^From
             (gnus-header-name ((t (,@fmt-none ,@fg-base01)))) ; hdrdefault
             (gnus-header-newsgroups ; hdrdefault
              ((t (,@fmt-none ,@fg-base02))))
             (gnus-header-subject ; header ^Subject
              ((t (,@fmt-none ,@fg-blue))))
             (gnus-server-agent ((t (,@fmt-bold ,@fg-base3))))
             (gnus-server-closed ((t (,@fmt-ital ,@fg-base1))))
             (gnus-server-denied ((t (,@fmt-bold ,@fg-base2))))
             (gnus-server-offline ((t (,@fmt-bold ,@fg-green))))
             (gnus-server-opened ((t (,@fmt-bold ,@fg-cyan))))
             (gnus-signature ((t (,@fmt-none ,@fg-base01)))) ; signature
             (gnus-splash ((t (,@fg-base2))))
             (gnus-summary-cancelled ; deleted messages
              ((t (,@fmt-none ,@fg-red))))
             (gnus-summary-high-ancient
              ((t (,@fmt-bold :inherit gnus-summary-normal-ancient))))
             (gnus-summary-high-read
              ((t (,@fmt-bold :inherit gnus-summary-normal-read))))
             (gnus-summary-high-ticked
              ((t (,@fmt-bold :inherit gnus-summary-normal-ticked))))
             (gnus-summary-high-undownloaded
              ((t (,@fmt-bold :inherit gnus-summary-normal-undownloaded))))
             (gnus-summary-high-unread
              ((t (,@fmt-bold :inherit gnus-summary-normal-unread))))
             (gnus-summary-low-ancient
              ((t (,@fmt-ital :inherit gnus-summary-normal-ancient))))
             (gnus-summary-low-read
              ((t (,@fmt-ital :inherit gnus-summary-normal-ancient))))
             (gnus-summary-low-unread
              ((t (,@fmt-ital :inherit gnus-summary-normal-unread))))
             (gnus-summary-low-ticked
              ((t (,@fmt-ital :inherit gnus-summary-normal-ancient))))
             (gnus-summary-low-undownloaded
              ((t (,@fmt-ital :inherit gnus-summary-normal-ancient))))
             (gnus-summary-normal-ancient ; old messages
              ((t (,@fmt-none ,@fg-blue))))
             (gnus-summary-normal-read ; read messages
              ((t (,@fmt-none ,@fg-base01))))
             (gnus-summary-normal-ticked ; flagged
              ((t (,@fmt-none ,@fg-red))))
             (gnus-summary-normal-undownloaded ((t (,@fmt-none ,@fg-base2))))
             (gnus-summary-normal-unread ; unread messages
              ((t (,@fmt-none ,@fg-blue))))
             (gnus-summary-selected ; indicator
              ((t (,@fmt-none ,@fg-base03 ,@bg-yellow))))
             ;; Message
             (message-mml ((t (,@fg-blue))))
             (message-cited-text ((t (,@fg-base2))))
             (message-separator ((t (,@fg-base3))))
             (message-header-xheader ((t (,@fg-violet))))
             (message-header-name ((t (,@fg-cyan))))
             (message-header-other ((t (,@fg-red))))
             (message-header-newsgroups ((t (,@fmt-bldi ,@fg-yellow))))
             (message-header-subject ((t (,@fg-base00))))
             (message-header-cc ((t (,@fmt-bold ,@fg-green))))
             (message-header-to ((t (,@fmt-bold ,@fg-base1))))
             ;; parenface
             (paren-face ((t (,@fg-base01))))
             ;; rainbow-delimiters
             (rainbow-delimiters-depth-1-face ((t (,@fg-cyan))))
             (rainbow-delimiters-depth-2-face ((t (,@fg-yellow))))
             (rainbow-delimiters-depth-3-face ((t (,@fg-blue))))
             (rainbow-delimiters-depth-4-face ((t (,@fg-red))))
             (rainbow-delimiters-depth-5-face ((t (,@fg-green))))
             (rainbow-delimiters-depth-6-face ((t (,@fg-blue))))
             (rainbow-delimiters-depth-7-face ((t (,@fg-orange))))
             (rainbow-delimiters-depth-8-face ((t (,@fg-magenta))))
             (rainbow-delimiters-depth-9-face ((t (,@fg-base0))))
             ;; slime
             (slime-error-face ((t (,@fmt-revr ,@fg-red)))) ; ErrorMsg
             (slime-note-face ((t (,@fg-yellow))))
             (slime-repl-inputted-output-face ((t (,@fg-red))))
             (slime-repl-output-mouseover-face ((t (:box (:color ,base3)))))
             (slime-style-warning-face ((t (,@fmt-bold ,@fg-orange))))
             (slime-warning-face ((t (,@fmt-bold ,@fg-red)))) ; WarningMsg
             ;; whitespace
             (whitespace-empty ((t (,@fg-red))))
             (whitespace-hspace ((t (,@fg-orange))))
             (whitespace-indentation ((t (,@fg-base02))))
             (whitespace-space ((t (,@fg-base02))))
             (whitespace-space-after-tab ((t (,@fg-cyan))))
             (whitespace-space-before-tab ((t (,@fmt-bold ,@fg-red))))
             (whitespace-tab ((t (,@fg-base02))))
             (whitespace-trailing ((t (,@fmt-bold ,@fg-red ,@bg-base02))))
             (whitespace-highlight-face ((t (,@fg-red ,@bg-blue))))
             (whitespace-line ((t (,@fg-magenta ,@bg-base03))))
             ;; rcirc
             (rcirc-my-nick ((t (:foreground ,blue))))
             (rcirc-nick-in-message ((t (:foreground ,orange))))
             (rcirc-other-nick ((t (:foreground ,green))))
             (rcirc-prompt ((t (:foreground ,yellow))))
             (rcirc-bright-nick ((t (:foreground ,magenta))))
             (rcirc-server ((t (:foreground ,base1))))
             (rcirc-timestamp ((t (:foreground ,base01))))
             ;; ERC
             (erc-input-face ((t (:foreground ,base01))))
             (erc-keyword-face ((t (,@fmt-bldi ,@fg-yellow))))
             (erc-my-nick-face ((t (:foreground ,blue))))
             (erc-nick-default-face ((t (,@fmt-none ,@fg-cyan))))
             (erc-notice-face ((t (,@fmt-none ,@fg-blue))))
             (erc-timestamp-face ((t (:foreground ,base01))))
             ;;font-latex
             (font-latex-warning-face ((t (,@fg-red))))
             (font-latex-sectioning-5-face ((t (,@fg-violet))))
             ;;flyspell
             (flyspell-incorrect ((t (,@fg-red))))
             (flyspell-duplicate ((t (,@fg-yellow)))))

            ((foreground-color . ,(when (<= 16 (display-color-cells)) base0))
             (background-color . ,back)
             (background-mode . ,mode)
             (cursor-color . ,(when (<= 16 (display-color-cells))
                                base0)))))))))

(defmacro create-solarized-theme (mode)
  (let* ((theme-name (intern (concat "solarized-" (symbol-name mode))))
         (defs (solarized-color-definitions mode))
         (theme-vars (mapcar (lambda (def) (list (car def) (cdr def)))
                             (second defs)))
         (theme-faces (first defs)))
    `(progn
       (deftheme ,theme-name ,solarized-description)
       (apply 'custom-theme-set-variables ',theme-name ',theme-vars)
       (apply 'custom-theme-set-faces ',theme-name ',theme-faces)
       (provide-theme ',theme-name))))

;;;###autoload
(when (boundp 'custom-theme-load-path)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide 'solarized-definitions)
