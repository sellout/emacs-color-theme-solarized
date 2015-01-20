(eval-when-compile
  (unless (require 'cl-lib nil t)
    (require 'cl)
    (defalias 'cl-case 'case)
    (defalias 'cl-copy-list 'copy-list))
  )

(defconst solarized-description
  "Color theme by Ethan Schoonover, created 2011-03-24.
Ported to Emacs by Greg Pfeil, http://ethanschoonover.com/solarized.")

(defcustom solarized-termcolors 16
  "This is set to 16 by default, meaning that Solarized will attempt to use the
standard 16 colors of your terminal emulator. You will need to set those colors
to the correct Solarized values either manually or by importing one of the many
colorscheme available for popular terminal emulators and Xdefaults."
  :type 'integer
  :options '(16 256)
  :group 'solarized)

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

(defcustom solarized-broken-srgb
  (if (and (eq system-type 'darwin) (eq window-system 'ns))
      (not (and (boundp 'ns-use-srgb-colorspace)
                ns-use-srgb-colorspace))
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

(defun solarized-face-for-index (facespec index &optional light)
  "Creates a face from facespec where the colors use the names from
  `solarized-colors`."
  (let ((new-fontspec (cl-copy-list facespec)))
    (dolist (property '(:foreground :background :color))
      (let ((color-name (plist-get new-fontspec property)))
        (when color-name
          ;; NOTE: We try to turn an 8-color term into a 10-color term by not
          ;;       using default background and foreground colors, expecting the
          ;;       user to have the right colors set for them.
          (when (and (= index 5)
                     (or (and (eq property :background)
                              (eq color-name 'back))
                         (and (eq property :foreground)
                              (member color-name '(base0 base1)))))
            (setf color-name nil))
          (when (eq color-name 'back)
            (setf color-name 'base03))
          (when light
            (setf color-name
                  (cl-case color-name
                    (base03 'base3)
                    (base02 'base2)
                    (base01 'base1)
                    (base00 'base0)
                    (base0 'base00)
                    (base1 'base01)
                    (base2 'base02)
                    (base3 'base03)
                    (otherwise color-name))))
          (plist-put new-fontspec
                     property
                     (nth index (assoc color-name solarized-colors))))))
    (when (plist-get new-fontspec :box)
      (plist-put new-fontspec
                 :box
                 (solarized-face-for-index (plist-get new-fontspec :box) index
                                           light)))
    new-fontspec))

(defun create-face-spec (name facespec)
  `(,name ((((background dark) (type graphic))
            ,(solarized-face-for-index facespec
                                       (cond (solarized-degrade     3)
                                             (solarized-broken-srgb 2)
                                             (t                     1))))
           (((background dark) (type tty) (min-colors 256))
            ,(solarized-face-for-index facespec
                                       (if (= solarized-termcolors 16) 4 3)))
           (((background dark) (type tty) (min-colors  16))
            ,(solarized-face-for-index facespec 4))
           (((background dark) (type tty) (min-colors   8))
            ,(solarized-face-for-index facespec 5))
           (((background light) (type graphic))
            ,(solarized-face-for-index facespec
                                       (cond (solarized-degrade     3)
                                             (solarized-broken-srgb 2)
                                             (t                     1))
                                       t))
           (((background light) (type tty) (min-colors 256))
            ,(solarized-face-for-index facespec
                                       (if (= solarized-termcolors 16) 4 3)
                                       t))
           (((background light) (type tty) (min-colors  16))
            ,(solarized-face-for-index facespec 4 t))
           (((background light) (type tty) (min-colors   8))
            ,(solarized-face-for-index facespec 5 t)))))

(defun solarized-color-definitions ()
  (let ((bold        (if solarized-bold 'bold 'normal))
        (bright-bold (if solarized-bold 'normal 'bold))
        (underline   (if solarized-underline t nil))
        (opt-under   nil)
        (italic      (if solarized-italic 'italic 'normal)))
    (cond ((eq 'high solarized-contrast)
           (let ((orig-base3 base3))
             (rotatef base01 base00 base0 base1 base2 base3)
             (setf base3 orig-base3)))
          ((eq 'low solarized-contrast)
           (setf back      base02
                 opt-under t)))
    (let ((bg-back   '(:background back))
          (bg-base03 '(:background base03))
          (bg-base02 '(:background base02))
          (bg-base01 '(:background base01))
          (bg-base00 '(:background base00))
          (bg-base0 '(:background base0))
          (bg-base1 '(:background base1))
          (bg-base2 '(:background base2))
          (bg-base3 '(:background base3))
          (bg-green '(:background green))
          (bg-yellow '(:background yellow))
          (bg-orange '(:background orange))
          (bg-red '(:background red))
          (bg-magenta '(:background magenta))
          (bg-violet '(:background violet))
          (bg-blue '(:background blue))
          (bg-cyan '(:background cyan))

          (fg-base03 '(:foreground base03))
          (fg-base02 '(:foreground base02))
          (fg-base01 '(:foreground base01))
          (fg-base00 '(:foreground base00))
          (fg-base0 '(:foreground base0))
          (fg-base1 '(:foreground base1))
          (fg-base2 '(:foreground base2))
          (fg-base3 '(:foreground base3))
          (fg-green '(:foreground green))
          (fg-yellow '(:foreground yellow))
          (fg-orange '(:foreground orange))
          (fg-red '(:foreground red))
          (fg-magenta '(:foreground magenta))
          (fg-violet '(:foreground violet))
          (fg-blue '(:foreground blue))
          (fg-cyan '(:foreground cyan))

          (fmt-none `(:weight normal :slant normal  :underline nil        :inverse-video nil))
          (fmt-bold `(:weight ,bold  :slant normal  :underline nil        :inverse-video nil))
          (fmt-bldi `(:weight ,bold                 :underline nil        :inverse-video nil))
          (fmt-undr `(:weight normal :slant normal  :underline ,underline :inverse-video nil))
          (fmt-undb `(:weight ,bold  :slant normal  :underline ,underline :inverse-video nil))
          (fmt-undi `(:weight normal                :underline ,underline :inverse-video nil))
          (fmt-uopt `(:weight normal :slant normal  :underline ,opt-under :inverse-video nil))
          ;; FIXME: don’t hardcode the SRGB color names
          (fmt-curl-red    `(:weight normal :slant normal :underline (:color "#dc322f" :style wave) :inverse-video nil))
          (fmt-curl-yellow `(:weight normal :slant normal :underline (:color "#b58900" :style wave) :inverse-video nil))
          (fmt-ital `(:weight normal :slant ,italic :underline nil        :inverse-video nil))
          ;; FIXME: not quite the same
          (fmt-stnd `(:weight normal :slant normal  :underline nil        :inverse-video t))
          (fmt-revr `(:weight normal :slant normal  :underline nil        :inverse-video t))
          (fmt-revb `(:weight ,bold  :slant normal  :underline nil        :inverse-video t))
          (fmt-revbb `(:weight ,bright-bold :slant normal :underline nil  :inverse-video t))
          (fmt-revbbu `(:weight ,bright-bold :slant normal :underline ,underline :inverse-video t)))
      (eval-after-load 'ansi-color
        '(setf ansi-color-names-vector [,base02 ,red ,green ,yellow ,blue ,magenta ,cyan ,base00]))
      (mapcar (lambda (face) (apply 'create-face-spec face))
              `(;; basic
                (default (,@fg-base0 ,@bg-back))   ; Normal
                (cursor (,@fg-base03 ,@bg-base0))  ; Cursor
                (error (,@fmt-bold ,@fg-red))      ; Error
                (escape-glyph-face (,@fg-red))
                (fringe (,@fg-base01 ,@bg-base02))
                (linum (,@fmt-none ,@fg-base01 ,@bg-base02))
                (header-line (,@fg-base0 ,@bg-base02 ,@fmt-revbb)) ; Pmenu
                (highlight (,@bg-base02))
                (hl-line (:underline ,opt-under ,@bg-base02)) ; CursorLine
                (isearch (,@fmt-stnd ,@fg-orange ,@bg-back)) ; IncSearch
                (isearch-fail (,@fmt-stnd ,@fg-orange ,@bg-back)) ; IncSearch
                (lazy-highlight (,@fmt-revr ,@fg-yellow ,@bg-back)) ; Search
                (link (,@fmt-undr ,@fg-violet))
                (link-visited (,@fmt-undr ,@fg-magenta))
                (match ((t (,@fmt-revr ,@fg-yellow ,@bg-back)))) ; Occur
                (menu (,@fg-base0 ,@bg-base02))
                (minibuffer-prompt (,@fmt-bold ,@fg-cyan)) ; Question
                (mode-line ; StatusLine
                 (,@fg-base1 ,@bg-base02 ,@fmt-revbb :box nil))
                (mode-line-inactive    ; StatusLineNC
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
                 (,@fg-base1 ,@bg-base02 :box (:line-width 2 :style released-button)))
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
                (custom-group-tag-1 (,@fmt-bold ,@fg-base1))
                (custom-invalid (,@fmt-revr ,@fg-red ,@bg-back))
                (custom-link (,@fg-violet))
                (custom-state (,@fg-green))
                (custom-variable-tag (,@fg-base1))
                ;; diff - DiffAdd, DiffChange, DiffDelete, and DiffText
                ,@(cl-case solarized-diff-mode
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
                       (diff-refine-change (,@fmt-undr ,@fg-blue ,@bg-back))))
                    (normal
                     (if window-system
                         `((diff-added (,@fmt-bold ,@fg-green ,@bg-base02))
                           (diff-changed (,@fmt-bold ,@fg-yellow ,@bg-base02))
                           (diff-removed (,@fmt-bold ,@fg-red ,@bg-base02))
                           (diff-refine-change (,@fmt-bold ,@fg-blue ,@bg-base02)))
                       `((diff-added (,@fg-green ,@bg-base02))
                         (diff-changed (,@fg-yellow ,@bg-base02))
                         (diff-removed (,@fg-red ,@bg-base02))
                         (diff-refine-change (,@fg-blue ,@bg-base02))))))
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
                (font-lock-doc-string-face ; Comment (XEmacs-only)
                 (,@fmt-ital ,@fg-base01))
                (font-lock-color-constant-face (,@fmt-none ,@fg-green))
                (font-lock-comment-delimiter-face ; Comment
                 (,@fmt-ital ,@fg-base01))
                (font-lock-preprocessor-face (,@fmt-none ,@fg-orange)) ; PreProc
                (font-lock-reference-face (,@fmt-none ,@fg-cyan))
                (font-lock-negation-char-face (,@fmt-none ,@fg-red))
                (font-lock-other-type-face (,@fmt-ital ,@fg-blue))
                (font-lock-regexp-grouping-construct (,@fmt-none ,@fg-orange))
                (font-lock-special-keyword-face (,@fmt-none ,@fg-red)) ; Special
                (font-lock-exit-face (,@fmt-none ,@fg-red))
                (font-lock-other-emphasized-face (,@fmt-bldi ,@fg-violet))
                (font-lock-regexp-grouping-backslash (,@fmt-none ,@fg-yellow))
                ;; info
                (info-xref (,@fmt-undr ,@fg-blue))
                (info-xref-visited (,@fg-magenta :inherit info-xref))
                ;; org
                (org-block-background (,@bg-base02))
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
                ;; show-paren - MatchParen
                (show-paren-match (,@fmt-bold ,@fg-cyan ,@bg-base02))
                (show-paren-mismatch (,@fmt-bold ,@fg-red ,@bg-base01))
                ;; speedbar
                ;; (speedbar-button-face (,@fmt-none ,@fg-base1))
                (speedbar-button-face
                 (,@fg-base1 ,@bg-base02
                             :box (:line-width 2 :style released-button)))
                (speedbar-directory-face (,@fmt-none ,@fg-blue))
                (speedbar-file-face (,@fmt-none ,@fg-green))
                ;; (speedbar-highlight-face (,@bg-base02))
                (speedbar-highlight-face
                 (,@fmt-revr ,@fg-base1 ,@bg-base02
                             :inherit speedbar-button-face))
                ;; (speedbar-selected-face (,@fmt-undr ,@fg-yellow))
                (speedbar-selected-face (,@fmt-none ,@bg-base02 ,@fg-green))
                (speedbar-separator-face (,@fmt-stnd))
                (speedbar-tag-face (,@fmt-none ,@fg-blue))
                ;; widgets
                (widget-field
                 (,@fg-base1 ,@bg-base02 :box (:line-width 1 :color base2)
                             :inherit default))
                (widget-single-line-field (:inherit widget-field))
                ;; extra modules
                ;; -------------
                ;; col-highlight -- Highlight the current column.
                ;; http://www.emacswiki.org/emacs/col-highlight.el
                (col-highlight (,@bg-base02))
                ;; ace-jump-mode
                (ace-jump-face-background (,@fmt-none ,@fg-base01))
                (ace-jump-face-foreground (,@fmt-bold ,@fg-red))
                ;; bm visual bookmarks
                (bm-fringe-face (,@bg-orange ,@fg-base03))
                (bm-fringe-persistent-face (,@bg-blue ,@fg-base03))
                ;; Flymake
                (flymake-errline (,@fmt-bold ,@fg-red)) ; Error
                (flymake-warnline (,@fmt-bold ,@fg-red))
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
                (jabber-roster-user-error
                 (:weight light :slant italic ,@fg-red))
                (jabber-roster-user-offline (,@fg-base01))
                (jabber-roster-user-online (,@fmt-bold ,@fg-blue))
                (jabber-roster-user-xa (,@fmt-ital ,@fg-magenta))
                ;; git-gutter
                (git-gutter:modified (,@fg-violet))
                (git-gutter:added (,@fg-green))
                (git-gutter:deleted (,@fg-red))
                ;; gnus - these are taken from mutt, not VIM
                (gnus-cite-1 (,@fmt-none ,@fg-blue))    ; quoted
                (gnus-cite-2 (,@fmt-none ,@fg-cyan))    ; quoted1
                (gnus-cite-3 (,@fmt-none ,@fg-yellow))  ; quoted2
                (gnus-cite-4 (,@fmt-none ,@fg-red))     ; quoted3
                (gnus-cite-5 (,@fmt-none ,@fg-orange))  ; quoted4
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
                (gnus-header-newsgroups (,@fmt-none ,@fg-base02)) ; hdrdefault
                (gnus-header-subject (,@fmt-none ,@fg-blue)) ; header ^Subject
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
                (gnus-summary-normal-ticked (,@fmt-none ,@fg-red)) ; flagged
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
                ;; minimap
                (minimap-active-region-background (,@bg-base02))
                (minimap-semantic-function-face (,bg-base3))
                (minimap-semantic-type-face (,bg-base3))
                (minimap-semantic-variable-face (,bg-base3))
                ;; parenface
                (paren-face (,@fg-base01)) ; NB: local, don’t commit
                (parenthesis (,@fg-base01)) ; NB: local, don’t commit
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
                ;;magit next branch
                (magit-diff-added (,@bg-back ,@fg-green))
                (magit-diff-removed (,@bg-back ,@fg-red))
                (magit-diff-context (,@bg-back ,@fg-base1))
                (magit-diff-added-highlight (,@bg-base02 ,@fg-green))
                (magit-diff-removed-highlight (,@bg-base02 ,@fg-red))
                (magit-diff-context-highlight (,@bg-base02 ,@fg-base1))
                (magit-hunk-heading (,@bg-base03 ,@fg-base1))
                (magit-section-highlight (,@bg-base02))
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
                (whitespace-line (,@fg-magenta))
                ;; rcirc
                (rcirc-my-nick (,@fg-blue))
                (rcirc-nick-in-message (,@fg-orange))
                (rcirc-other-nick (,@fg-green))
                (rcirc-prompt (,@fg-yellow))
                (rcirc-bright-nick (,@fg-magenta))
                (rcirc-server (,@fg-base1))
                (rcirc-timestamp (,@fg-base01))
                ;;font-latex
                (font-latex-warning-face (,@fg-red))
                (font-latex-sectioning-5-face (,@fg-violet))
                ;;flyspell
                (flyspell-incorrect (,@fmt-curl-red)) ; SpellBad
                (flyspell-duplicate (,@fmt-curl-yellow))
                ;;ansi-term
                (term-color-black (,@fg-base02))
                (term-color-red (,@fg-red))
                (term-color-green (,@fg-green))
                (term-color-yellow (,@fg-yellow))
                (term-color-blue (,@fg-blue))
                (term-color-magenta (,@fg-magenta))
                (term-color-cyan (,@fg-cyan))
                (term-color-white (,@fg-base00)))))))

;;;###autoload
(when (boundp 'custom-theme-load-path)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(defmacro create-solarized-theme (name description color-definitions)
  `(progn
     (deftheme ,name ,description)
     (apply 'custom-theme-set-faces
            ',name ,color-definitions)
     (provide-theme ',name)))

(provide 'solarized-definitions)
