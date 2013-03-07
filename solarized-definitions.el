(eval-when-compile
  (require 'cl))

(defconst solarized-description
  "Color theme by Ethan Schoonover, created 2011-03-24.
Ported to Emacs by Greg Pfeil, http://ethanschoonover.com/solarized.")

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

(defcustom solarized-terminal-themed t
  "Non-nil when the terminal emulator has been themed with Solarized.
In this scenario, we do not set the background color, in favor of the more
accurate version of the color in the default terminal background."
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

(defun solarized-face-for-index (facespec index &optional back fg)
  "Creates a face from facespec where the colors use the names of
  the `solarized-colors'."
  (let ((new-fontspec (copy-list facespec)))
    (dolist (property '(:foreground :background :color))
      (let ((color (plist-get new-fontspec property)))
        (when color
          (cond ((and solarized-terminal-themed
                      (eq property :background) (memq color back))
                 (plist-put new-fontspec property "unspecified-bg"))
                ((and solarized-terminal-themed
                      (eq property :foreground) (memq color fg))
                 (plist-put new-fontspec property "unspecified-fg"))
                (t (plist-put new-fontspec property
                              (nth index (assoc color solarized-colors))))))))
    (when (plist-get new-fontspec :box)
      (plist-put new-fontspec :box (solarized-face-for-index
                                    (plist-get new-fontspec :box) index)))
    new-fontspec))

(defun solarized-flip (facespec)
  "Convert a facespec to its lightened or darkened counterpart"
  (let* ((reversing-alist '((base03 . base3) (base02 . base2) (base01 . base1)
                            (base00 . base0) (base0 . base00) (base1 . base01)
                            (base2 . base02) (base3 . base03))))
    (mapcar (lambda (term) (cond ((listp term) (solarized-flip term))
                            ((assoc term reversing-alist)
                             (cdr (assoc term reversing-alist)))
                            (t term))) facespec)))

(defun solarized-faces (back fg facespecs mode)
  (mapcar (lambda (facespec-with-name)
            (let* ((name (car facespec-with-name))
                   (facespec (funcall
                              (if (eq mode 'light) 'solarized-flip 'identity)
                              (second facespec-with-name)))
                   (facespec-tty-256 (solarized-face-for-index facespec 3 back fg))
                   (facespec-tty-term (solarized-face-for-index facespec 4 back fg))
                   (facespec-tty-8 (solarized-face-for-index facespec 5 back fg))
                   (facespec-default (if solarized-broken-srgb
                                         (solarized-face-for-index facespec 2)
                                       (solarized-face-for-index facespec 1))))
              `(,name
                ((((min-colors 257)) ,facespec-default)
                 (((min-colors 256)) ,facespec-tty-256)
                 (((min-colors 16)) ,facespec-tty-term)
                 (((min-colors 8)) ,facespec-tty-8)
                 ;; We should rarely if ever fall to the default.  If so, let's
                 ;; set it to the default spec and hope for the best.
                 (t ,facespec-default))))) facespecs))

(defun solarized-color-definitions (mode)
  "Define colors that make up Solarized."
  ;; We define everything for dark mode, but we generate light mode
  ;; automatically in `solarized-faces.'
  ;;
  ;; See http://ethanschoonover.com/solarized#features for an explanation
  ;; of what the base00/base0 names stand for
  (let* ((bold        (if solarized-bold 'bold 'normal))
         (bright-bold (if solarized-bold 'normal 'bold))
         (underline   (if solarized-underline t nil))
         (opt-under   (eq 'low solarized-contrast))
         (italic      (if solarized-italic 'italic 'normal))
         (back        (if (eq 'low solarized-contrast) 'base02 'base03))

         (bg-back     `(:background ,back))
         (bg-base03   `(:background base03))
         (bg-base02   `(:background base02))
         (bg-base01   `(:background base01))
         (bg-base00   `(:background base00))
         (bg-base0    `(:background base0))
         (bg-base1    `(:background base1))
         (bg-base2    `(:background base2))
         (bg-base3    `(:background base3))
         (bg-green    `(:background green))
         (bg-yellow   `(:background yellow))
         (bg-orange   `(:background orange))
         (bg-red      `(:background red))
         (bg-magenta  `(:background magenta))
         (bg-violet   `(:background violet))
         (bg-blue     `(:background blue))
         (bg-cyan     `(:background cyan))

         (fg-base03   `(:foreground base03))
         (fg-base02   `(:foreground base02))
         (fg-base01   `(:foreground base01))
         (fg-base00   `(:foreground base00))
         (fg-base0    `(:foreground base0))
         (fg-base1    `(:foreground base1))
         (fg-base2    `(:foreground base2))
         (fg-base3    `(:foreground base3))
         (fg-green    `(:foreground green))
         (fg-yellow   `(:foreground yellow))
         (fg-orange   `(:foreground orange))
         (fg-red      `(:foreground red))
         (fg-magenta  `(:foreground magenta))
         (fg-violet   `(:foreground violet))
         (fg-blue     `(:foreground blue))
         (fg-cyan     `(:foreground cyan))
         (fg-none     `(:foreground "unspecified-fg"))
         (fmt-none    `(:weight normal :slant normal  :underline nil        :inverse-video nil))
         (fmt-bold    `(:weight ,bold  :slant normal  :underline nil        :inverse-video nil))
         (fmt-bldi    `(:weight ,bold                 :underline nil        :inverse-video nil))
         (fmt-undr    `(:weight normal :slant normal  :underline ,underline :inverse-video nil))
         (fmt-undb    `(:weight ,bold  :slant normal  :underline ,underline :inverse-video nil))
         (fmt-undi    `(:weight normal                :underline ,underline :inverse-video nil))
         (fmt-uopt    `(:weight normal :slant normal  :underline ,opt-under :inverse-video nil))
         ;; FIXME: not quite the same
         (fmt-curl    `(:weight normal :slant normal  :underline t          :inverse-video nil))
         (fmt-ital    `(:weight normal :slant ,italic :underline nil        :inverse-video nil))
         ;; FIXME: not quite the same
         (fmt-stnd    `(:weight normal :slant normal  :underline nil        :inverse-video t))
         (fmt-revr    `(:weight normal :slant normal  :underline nil        :inverse-video t))
         (fmt-revb    `(:weight ,bold  :slant normal  :underline nil        :inverse-video t))
         (fmt-revbb   `(:weight ,bright-bold :slant normal :underline nil  :inverse-video t))
         (fmt-revbbu  `(:weight ,bright-bold :slant normal  :underline ,underline :inverse-video t)))
    (list
     ;; First list is for faces
     (solarized-faces `(,back) '(base0 base1)
      `(;; basic
        (default (,@fg-base0 ,@bg-back)) ; Normal
        (cursor (,@fg-base03 ,@bg-base0)) ; Cursor
        (error (,@fmt-bold ,@fg-red)) ; Error
        (escape-glyph-face (,@fg-red))
        (fringe (,@fg-base01 ,@bg-base02))
        (linum (,@fg-base01 ,@bg-base02))
        (header-line (,@fg-base0 ,@bg-base02 ,@fmt-revbb)) ; Pmenu
        (highlight (,@fg-none ,@bg-base02))
        (hl-line (:underline ,opt-under ,@bg-base02)) ; CursorLine
        (isearch (,@fmt-stnd ,@fg-orange ,@bg-back)) ; IncSearch
        (isearch-fail (,@fmt-stnd ,@fg-orange ,@bg-back)) ; IncSearch
        (lazy-highlight (,@fmt-revr ,@fg-yellow ,@bg-back)) ; Search
        (link (,@fmt-undr ,@fg-violet))
        (link-visited (,@fmt-undr ,@fg-magenta))
        (menu (,@fg-base0 ,@bg-base02))
        (minibuffer-prompt (,@fmt-bold ,@fg-cyan)) ; Question
        (mode-line  ; StatusLine
         (,@fg-base1 ,@bg-base02 ,@fmt-revbb :box nil))
        (mode-line-inactive ; StatusLineNC
         (,@fg-base00 ,@bg-base02 ,@fmt-revbb :box nil))
        (region (,@fg-base01 ,@bg-base03 ,@fmt-revbb)) ; Visual
        (secondary-selection (,@bg-base02))
        (shadow (,@fg-base01))
        (trailing-whitespace (,@fmt-revr ,@fg-red))
        (vertical-border (,@fg-base0))
        ;; comint
        (comint-highlight-prompt (,@fg-blue))
        ;; compilation
        (compilation-info (,@fmt-bold ,@fg-green))
        (compilation-warning (,@fmt-bold ,@fg-orange))
        ;; custom
        (custom-button
         (,@fg-base1 ,@bg-base02
                         :box (:line-width 2 :style released-button)))
        (custom-button-mouse
         (,@fmt-revr ,@fg-base1 ,@bg-base02 :inherit custom-button))
        (custom-button-pressed
         (,@fmt-revr ,@fg-base1 ,@bg-base02
                         :box (:line-width 2 :style pressed-button)
                         :inherit custom-button-mouse))
        (custom-changed (,@fmt-revr ,@fg-blue ,@bg-base3))
        (custom-comment (,@fg-base1 ,@bg-base02))
        (custom-comment-tag (,@fg-base1 ,@bg-base02))
        (custom-documentation (:inherit default))
        (custom-group-tag (,@fg-base1))
        (custom-group-tag-1 (,fmt-bold ,@fg-base1))
        (custom-invalid (,@fmt-revr ,@fg-red ,@bg-back))
        (custom-link (,@fg-violet))
        (custom-state (,@fg-green))
        (custom-variable-tag (,@fg-base1))
        ;; diff - DiffAdd, DiffChange, DiffDelete, and DiffText
        ,@(case solarized-diff-mode
            (high
             `((diff-added (,@fmt-revr ,@fg-green))
               (diff-changed (,@fmt-revr ,@fg-yellow))
               (diff-removed (,@fmt-revr ,@fg-red))
               (diff-refine-change
                (,@fmt-revr ,@fg-blue ,@bg-back))))
            (low
             `((diff-added (,@fmt-undr ,@fg-green))
               (diff-changed (,@fmt-undr ,@fg-yellow))
               (diff-removed (,@fmt-bold ,@fg-red))
               (diff-refine-change
                (,@fmt-undr ,@fg-blue ,@bg-back))))
            (normal
             (if window-system
                 `((diff-added (,@fmt-bold ,@fg-green))
                   (diff-changed (,@fmt-bold ,@fg-yellow))
                   (diff-removed (,@fmt-bold ,@fg-red))
                   (diff-refine-change
                    (,@fmt-bold ,@fg-blue ,@bg-back)))
               `((diff-added (,@fg-green))
                 (diff-changed (,@fg-yellow))
                 (diff-removed (,@fg-red))
                 (diff-refine-change (,@fg-blue ,@bg-back))))))
        (diff-file-header (,@bg-back))
        (diff-header (,@fg-base1 ,@bg-back))
        ;; IDO
        (ido-only-match (,@fg-green))
        (ido-subdir (,@fg-blue))
        (ido-first-match (,@fmt-bold ,@fg-green))
        ;; emacs-wiki
        (emacs-wiki-bad-link-face (,@fmt-undr ,@fg-red))
        (emacs-wiki-link-face (,@fmt-undr ,@fg-blue))
        (emacs-wiki-verbatim-face (,@fmt-undr ,@fg-base00))
        ;; eshell
        (eshell-ls-archive (,@fg-magenta))
        (eshell-ls-backup (,@fg-yellow))
        (eshell-ls-clutter (,@fg-orange))
        (eshell-ls-directory (,@fg-blue)) ; Directory
        (eshell-ls-executable (,@fg-green))
        (eshell-ls-missing (,@fg-red))
        (eshell-ls-product (,@fg-yellow))
        (eshell-ls-readonly (,@fg-base1))
        (eshell-ls-special (,@fg-violet))
        (eshell-ls-symlink (,@fg-cyan))
        (eshell-ls-unreadable (,@fg-base00))
        (eshell-prompt (,@fmt-bold ,@fg-green))
        ;; font-lock
        (font-lock-builtin-face (,@fmt-none ,@fg-green)) ; Statement
        (font-lock-comment-face (,@fmt-ital ,@fg-base01)) ; Comment
        (font-lock-constant-face (,@fmt-none ,@fg-cyan)) ; Constant
        (font-lock-function-name-face ; Identifier
         (,@fmt-none ,@fg-blue))
        (font-lock-keyword-face (,@fmt-none ,@fg-green)) ; Statement
        (font-lock-string-face (,@fmt-none ,@fg-cyan)) ; Constant
        (font-lock-type-face (,@fmt-none ,@fg-yellow)) ; Type
        (font-lock-variable-name-face ; Identifier
         (,@fmt-none ,@fg-blue))
        (font-lock-warning-face (,@fmt-bold ,@fg-red)) ; Error
        (font-lock-doc-face (,@fmt-ital ,@fg-base01)) ; Comment
        (font-lock-doc-string-face  ; Comment (XEmacs-only)
         (,@fmt-ital ,@fg-base01))
        (font-lock-color-constant-face (,@fmt-none ,@fg-green))
        (font-lock-comment-delimiter-face ; Comment
         (,@fmt-ital ,@fg-base01))
        (font-lock-preprocessor-face ; PreProc
         (,@fmt-none ,@fg-orange))
        (font-lock-reference-face (,@fmt-none ,@fg-cyan))
        (font-lock-negation-char-face (,@fmt-none ,@fg-red))
        (font-lock-other-type-face (,@fmt-ital ,@fg-blue))
        (font-lock-regexp-grouping-construct
         (,@fmt-none ,@fg-orange))
        (font-lock-special-keyword-face ; Special
         (,@fmt-none ,@fg-red))
        (font-lock-exit-face (,@fmt-none ,@fg-red))
        (font-lock-other-emphasized-face (,@fmt-bldi ,@fg-violet))
        (font-lock-regexp-grouping-backslash
         (,@fmt-none ,@fg-yellow))
        ;; info
        (info-xref (,@fmt-undr ,@fg-blue))
        (info-xref-visited (,@fg-magenta :inherit info-xref))
        ;; org
        (org-hide (,@fg-base03))
        (org-todo (,@fmt-bold ,@fg-base03 ,@bg-red))
        (org-done (,@fmt-bold ,@fg-green))
        (org-todo-kwd-face (,@fg-red ,@bg-base03))
        (org-done-kwd-face (,@fg-green ,@bg-base03))
        (org-project-kwd-face (,@fg-violet ,@bg-base03))
        (org-waiting-kwd-face (,@fg-orange ,@bg-base03))
        (org-someday-kwd-face (,@fg-blue ,@bg-base03))
        (org-started-kwd-face (,@fg-yellow ,@bg-base03))
        (org-cancelled-kwd-face (,@fg-green ,@bg-base03))
        (org-delegated-kwd-face (,@fg-cyan ,@bg-base03))
        ;; table
        (table-cell (,@fmt-none ,@fg-base0 ,@bg-back))
        ;; outline - pandocBlockQuoteLeader*
        (outline-1 (,@fmt-none ,@fg-blue))
        (outline-2 (,@fmt-none ,@fg-cyan))
        (outline-3 (,@fmt-none ,@fg-yellow))
        (outline-4 (,@fmt-none ,@fg-red))
        (outline-5 (,@fmt-none ,@fg-base0))
        (outline-6 (,@fmt-none ,@fg-base01))
        (outline-7 (,@fmt-none ,@fg-orange))
        (outline-8 (,@fmt-none ,@fg-violet))
        ;; speedbar
        (speedbar-button-face (,@fmt-none ,@fg-base1))
        (speedbar-directory-face (,@fmt-none ,@fg-orange))
        (speedbar-file-face (,@fmt-none ,@fg-green))
        (speedbar-highlight-face (,@fg-none ,@bg-base02))
        (speedbar-selected-face (,@fmt-undr ,@fg-yellow))
        (speedbar-separator-face (,@fmt-stnd))
        (speedbar-tag-face (,@fmt-none ,@fg-blue))
        ;; show-paren - MatchParen
        (show-paren-match (,@fmt-bold ,@fg-cyan ,@bg-base02))
        (show-paren-mismatch (,@fmt-bold ,@fg-red ,@bg-base01))
        ;; widgets
        (widget-field
         (,@fg-base1 ,@bg-base02 :box (:line-width 1)
                         :inherit default))
        (widget-single-line-field (:inherit widget-field))
        ;; extra modules
        ;; -------------
        ;; Flymake
        (flymake-errline (,@fmt-revr ,@fg-red ,@bg-back)) ; ErrorMsg
        (flymake-warnline ; WarningMsg
         (,@fmt-bold ,@fg-red ,@bg-back))
        ;; column-marker
        (column-marker-1 (,@bg-base01))
        (column-marker-2 (,@bg-cyan))
        (column-marker-3 (,@bg-violet))
        ;; jabber
        (jabber-activity-face (,@fmt-bold ,@fg-red))
        (jabber-activity-personal-face (,@fmt-bold ,@fg-blue))
        (jabber-chat-error (,@fmt-bold ,@fg-red))
        (jabber-chat-prompt-foreign (,@fmt-bold ,@fg-red))
        (jabber-chat-prompt-local (,@fmt-bold ,@fg-blue))
        (jabber-chat-prompt-system (,@fmt-bold ,@fg-green))
        (jabber-chat-text-foreign (,@fg-base1))
        (jabber-chat-text-local (,@fg-base0))
        (jabber-chat-rare-time-face (,@fmt-undr ,@fg-green))
        (jabber-roster-user-away (,@fmt-ital ,@fg-green))
        (jabber-roster-user-chatty (,@fmt-bold ,@fg-orange))
        (jabber-roster-user-dnd (,@fmt-ital ,@fg-red))
        (jabber-roster-user-error (:weight light :slant italic ,@fg-red))
        (jabber-roster-user-offline (,@fg-base01))
        (jabber-roster-user-online (,@fmt-bold ,@fg-blue))
        (jabber-roster-user-xa (,@fmt-ital ,@fg-magenta))
        ;; gnus - these are taken from mutt, not VIM
        (gnus-cite-1 (,@fmt-none ,@fg-blue)) ; quoted
        (gnus-cite-2 (,@fmt-none ,@fg-cyan)) ; quoted1
        (gnus-cite-3 (,@fmt-none ,@fg-yellow)) ; quoted2
        (gnus-cite-4 (,@fmt-none ,@fg-red)) ; quoted3
        (gnus-cite-5 (,@fmt-none ,@fg-orange)) ; quoted4
        (gnus-cite-6 (,@fmt-none ,@fg-violet))
        (gnus-cite-7 (,@fmt-none ,@fg-green))
        (gnus-cite-8 (,@fmt-none ,@fg-magenta))
        (gnus-cite-9 (,@fmt-none ,@fg-base00))
        (gnus-cite-10 (,@fmt-none ,@fg-base01))
        (gnus-cite-11 (,@fmt-none ,@fg-base02))
        (gnus-group-mail-1 (,@fmt-bold ,@fg-base3))
        (gnus-group-mail-1-empty (,@fg-base3))
        (gnus-group-mail-2 (,@fmt-bold ,@fg-base2))
        (gnus-group-mail-2-empty (,@fg-base2))
        (gnus-group-mail-3 (,@fmt-bold ,@fg-magenta))
        (gnus-group-mail-3-empty (,@fg-magenta))
        (gnus-group-mail-low (,@fmt-bold ,@fg-base00))
        (gnus-group-mail-low-empty (,@fg-base00))
        (gnus-group-news-1 (,@fmt-bold ,@fg-base1))
        (gnus-group-news-1-empty (,@fg-base1))
        (gnus-group-news-2 (,@fmt-bold ,@fg-blue))
        (gnus-group-news-2-empty (,@fg-blue))
        (gnus-group-news-low (,@fmt-bold ,@fg-violet))
        (gnus-group-news-low-empty (,@fg-violet))
        (gnus-emphasis-highlight-words ; highlight
         (,@fmt-none ,fg-yellow))
        (gnus-header-content (,@fmt-none ,@fg-base01)) ; hdrdefault
        (gnus-header-from (,@fmt-none ,@fg-base00)) ; header ^From
        (gnus-header-name (,@fmt-none ,@fg-base01)) ; hdrdefault
        (gnus-header-newsgroups ; hdrdefault
         (,@fmt-none ,@fg-base02))
        (gnus-header-subject ; header ^Subject
         (,@fmt-none ,@fg-blue))
        (gnus-server-agent (,@fmt-bold ,@fg-base3))
        (gnus-server-closed (,@fmt-ital ,@fg-base1))
        (gnus-server-denied (,@fmt-bold ,@fg-base2))
        (gnus-server-offline (,@fmt-bold ,@fg-green))
        (gnus-server-opened (,@fmt-bold ,@fg-cyan))
        (gnus-signature (,@fmt-none ,@fg-base01)) ; signature
        (gnus-splash (,@fg-base2))
        (gnus-summary-cancelled ; deleted messages
         (,@fmt-none ,@fg-red))
        (gnus-summary-high-ancient
         (,@fmt-bold :inherit gnus-summary-normal-ancient))
        (gnus-summary-high-read
         (,@fmt-bold :inherit gnus-summary-normal-read))
        (gnus-summary-high-ticked
         (,@fmt-bold :inherit gnus-summary-normal-ticked))
        (gnus-summary-high-undownloaded
         (,@fmt-bold :inherit gnus-summary-normal-undownloaded))
        (gnus-summary-high-unread
         (,@fmt-bold :inherit gnus-summary-normal-unread))
        (gnus-summary-low-ancient
         (,@fmt-ital :inherit gnus-summary-normal-ancient))
        (gnus-summary-low-read
         (,@fmt-ital :inherit gnus-summary-normal-ancient))
        (gnus-summary-low-unread
         (,@fmt-ital :inherit gnus-summary-normal-unread))
        (gnus-summary-low-ticked
         (,@fmt-ital :inherit gnus-summary-normal-ancient))
        (gnus-summary-low-undownloaded
         (,@fmt-ital :inherit gnus-summary-normal-ancient))
        (gnus-summary-normal-ancient ; old messages
         (,@fmt-none ,@fg-blue))
        (gnus-summary-normal-read ; read messages
         (,@fmt-none ,@fg-base01))
        (gnus-summary-normal-ticked ; flagged
         (,@fmt-none ,@fg-red))
        (gnus-summary-normal-undownloaded (,@fmt-none ,@fg-base2))
        (gnus-summary-normal-unread ; unread messages
         (,@fmt-none ,@fg-blue))
        (gnus-summary-selected ; indicator
         (,@fmt-none ,@fg-base03 ,@bg-yellow))
        ;; Message
        (message-mml (,@fg-blue))
        (message-cited-text (,@fg-base2))
        (message-separator (,@fg-base3))
        (message-header-xheader (,@fg-violet))
        (message-header-name (,@fg-cyan))
        (message-header-other (,@fg-red))
        (message-header-newsgroups (,@fmt-bldi ,@fg-yellow))
        (message-header-subject (,@fg-base00))
        (message-header-cc (,@fmt-bold ,@fg-green))
        (message-header-to (,@fmt-bold ,@fg-base1))
        ;; parenface
        (paren-face (,@fg-base01))
        ;; rainbow-delimiters
        (rainbow-delimiters-depth-1-face (,@fg-cyan))
        (rainbow-delimiters-depth-2-face (,@fg-yellow))
        (rainbow-delimiters-depth-3-face (,@fg-blue))
        (rainbow-delimiters-depth-4-face (,@fg-red))
        (rainbow-delimiters-depth-5-face (,@fg-green))
        (rainbow-delimiters-depth-6-face (,@fg-blue))
        (rainbow-delimiters-depth-7-face (,@fg-orange))
        (rainbow-delimiters-depth-8-face (,@fg-magenta))
        (rainbow-delimiters-depth-9-face (,@fg-base0))
        ;; slime
        (slime-error-face (,@fmt-revr ,@fg-red)) ; ErrorMsg
        (slime-note-face (,@fg-yellow))
        (slime-repl-inputted-output-face (,@fg-red))
        (slime-repl-output-mouseover-face (:box (:color base3)))
        (slime-style-warning-face (,@fmt-bold ,@fg-orange))
        (slime-warning-face (,@fmt-bold ,@fg-red)) ; WarningMsg
        ;; whitespace
        (whitespace-empty (,@fg-red))
        (whitespace-hspace (,@fg-orange))
        (whitespace-indentation (,@fg-base02))
        (whitespace-space (,@fg-base02))
        (whitespace-space-after-tab (,@fg-cyan))
        (whitespace-space-before-tab (,@fmt-bold ,@fg-red))
        (whitespace-tab (,@fg-base02))
        (whitespace-trailing (,@fmt-bold ,@fg-red ,@bg-base02))
        (whitespace-highlight-face (,@fg-red ,@bg-blue))
        (whitespace-line (,@fg-magenta ,@bg-base03))
        ;; rcirc
        (rcirc-my-nick (:foreground blue))
        (rcirc-nick-in-message (:foreground orange))
        (rcirc-other-nick (:foreground green))
        (rcirc-prompt (:foreground yellow))
        (rcirc-bright-nick (:foreground magenta))
        (rcirc-server (:foreground base1))
        (rcirc-timestamp (:foreground base01))
        ;; ERC
        (erc-input-face (:foreground base01))
        (erc-keyword-face (,@fmt-bldi ,@fg-yellow))
        (erc-my-nick-face (:foreground blue))
        (erc-nick-default-face (,@fmt-none ,@fg-cyan))
        (erc-notice-face (,@fmt-none ,@fg-blue))
        (erc-timestamp-face (:foreground base01))
        ;;font-latex
        (font-latex-warning-face (,@fg-red))
        (font-latex-sectioning-5-face (,@fg-violet))
        ;;flyspell
        (flyspell-incorrect (,@fg-red))
        (flyspell-duplicate (,@fg-yellow)))
      mode)
     ;; Second list is for vars
     `((background-mode . ,mode)))))

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
