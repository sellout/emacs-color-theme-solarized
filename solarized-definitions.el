;;; solarized-definitions.el --- Solarized theme color assignments -*- lexical-binding: t -*-

(eval-when-compile
  (unless (require 'cl-lib nil t)
    (require 'cl)
    (defalias 'cl-case 'case)))
(require 'interim-faces)

(defconst solarized-description
  "Color theme by Ethan Schoonover, created 2011-03-24.
Ported to Emacs by Greg Pfeil, http://ethanschoonover.com/solarized.")

(defgroup solarized nil
  "Customizations for the Solarized theme."
  :group 'faces
  :prefix "solarized-")

(defcustom solarized-termtrans '(:background :foreground)
  "Whether to avoid specifying the background color in certain frames.
Terminals which support less than 16 colors can sometimes set the default
background and/or foreground to colors distinct from the 8 used otherwise,
potentially expanding the palette to 9 or 10 colors.

However, enabling this also means that you can’t switch to a background mode
other than the one your terminal is set to. I.e., if the terminal background is
dark, then you can only use the theme in dark mode, and if the terminal
background is light, you can only use the theme in light mode."
  :type '(set (const :background) (const :foreground))
  :group 'solarized)

(defcustom solarized-termcolors 16
  "The number of colors to use on 256-color terminals.
This is set to 16 by default, meaning that Solarized will attempt to use the
standard 16 colors of your terminal emulator. You will need to set those colors
to the correct Solarized values either manually or by importing one of the many
colorschemes available for popular terminal emulators and Xdefaults."
  :type '(choice (const 16) (const 256))
  :group 'solarized)

(defcustom solarized-degrade nil
  "Use only 256 colors on graphic displays.
For test purposes only; when in GUI mode, forces Solarized to use the 256
degraded color mode to test the approximate color values for accuracy."
  :type 'boolean
  :group 'solarized)

(defcustom solarized-diff-mode 'normal
  "Sets the level of highlighting to use in diff-like modes."
  :type '(choice (const high) (const normal) (const low))
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
  "Adjust the contrast level of Solarized.
Stick with normal! It's been carefully tested. Setting this option to high or
low does use the same Solarized palette but simply shifts some values up or down
in order to expand or compress the tonal range displayed."
  :type '(choice (const high) (const normal) (const low))
  :group 'solarized)

(defcustom solarized-broken-srgb
  (if (and (eq system-type 'darwin) (eq window-system 'ns))
      (not (and (boundp 'ns-use-srgb-colorspace)
                ns-use-srgb-colorspace))
    nil)
  "Whether sRGB is broken on your system.
If you are on a Mac and have either Emacs prior to 24.4 or Mac OS prior to 10.7
this should be t (the default is usually correct). Solarized works around this
issue by using with alternative colors. However, these colors are not totally
portable, so you may be able to edit the “Gen RGB” column in
solarized-definitions.el to improve them further."
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
  "This is a table of all the colors used by the Solarized color theme.
Each column is a different set, one of which will be chosen based on term
capabilities, etc.")

(defun solarized--current-colors (light)
  "Attempt to mimic the Vim version’s color configuration.
If LIGHT is non-nil, invert the base faces."
  (let ((current-colors
         (cons
          (cons 'back (copy-sequence (cdr (assoc 'base03 solarized-colors))))
          (mapcar #'copy-sequence (copy-sequence solarized-colors)))))
    (if light
        (setf (cdr (assoc 'base03 current-colors)) (cdr (assoc 'base3 solarized-colors))
              (cdr (assoc 'base02 current-colors)) (cdr (assoc 'base2 solarized-colors))
              (cdr (assoc 'base01 current-colors)) (cdr (assoc 'base1 solarized-colors))
              (cdr (assoc 'base00 current-colors)) (cdr (assoc 'base0 solarized-colors))
              (cdr (assoc 'base0 current-colors)) (cdr (assoc 'base00 solarized-colors))
              (cdr (assoc 'base1 current-colors)) (cdr (assoc 'base01 solarized-colors))
              (cdr (assoc 'base2 current-colors)) (cdr (assoc 'base02 solarized-colors))
              (cdr (assoc 'base3 current-colors)) (cdr (assoc 'base03 solarized-colors))
              (cdr (assoc 'back current-colors)) (cdr (assoc 'base03 current-colors))))
    (cond ((eq 'high solarized-contrast)
           (setf (cdr (assoc 'base01 current-colors)) (cdr (assoc 'base00 current-colors))
                 (cdr (assoc 'base00 current-colors)) (cdr (assoc 'base0 current-colors))
                 (cdr (assoc 'base0 current-colors)) (cdr (assoc 'base1 current-colors))
                 (cdr (assoc 'base1 current-colors)) (cdr (assoc 'base2 current-colors))
                 (cdr (assoc 'base2 current-colors)) (cdr (assoc 'base3 current-colors))
                 (cdr (assoc 'back current-colors)) (cdr (assoc 'back current-colors))))
          ((eq 'low solarized-contrast)
           (setf (cdr (assoc 'back current-colors)) (cdr (assoc 'base02 current-colors)))))
    current-colors))

(defun solarized-face-for-index (facespec index &optional light)
  "Replace the Solarized symbols in FACESPEC with the colors in column INDEX.
The colors are looked up in ‘solarized-colors’, and base colors are inverted if
LIGHT is non-nil."
  (let ((new-fontspec (copy-sequence facespec)))
    (dolist (property '(:background :box :color :foreground :overline :strike-through :underline))
      (let ((color-name (plist-get new-fontspec property)))
        (when (and color-name (symbolp color-name))
          (plist-put new-fontspec
                     property
                     ;; NOTE: We try to turn an 8-color term into a 10-color term by not
                     ;;       using default background and foreground colors, expecting the
                     ;;       user to have the right colors set for them.
                     (unless (and (= index 5)
                                  (or (and (eq property :background)
                                           (eq color-name 'back)
                                           (member :background solarized-termtrans))
                                      (and (eq property :foreground)
                                           (member color-name '(base0 base1))
                                           (member :foreground solarized-termtrans))))
                       (nth index
                            (assoc color-name
                                   (solarized--current-colors light))))))))
    (when (consp (plist-get new-fontspec :box))
      (plist-put new-fontspec
                 :box
                 (solarized-face-for-index (plist-get new-fontspec :box)
                                           index
                                           light)))
    (when (consp (plist-get new-fontspec :underline))
      (plist-put new-fontspec
                 :underline
                 (solarized-face-for-index (plist-get new-fontspec :underline)
                                           index
                                           light)))
    new-fontspec))

(defun dark-and-light (display plist index)
  "Return a list of faces, distinguishing between dark and light if necessary."
  (let ((dark (solarized-face-for-index plist index))
        (light (solarized-face-for-index plist index t)))
    (if (equal dark light)
        (list (list display dark))
      (list (list (cons '(background dark) display) dark)
            (list (cons '(background light) display) light)))))

(defun 8-and-16 (plist)
  "Return a list of faces, distinguishing between dark and light if necessary."
  (let ((eight (dark-and-light '() plist 5))
        (sixteen (dark-and-light '() plist 4)))
    (append
     (unless (equal eight sixteen)
       (mapcar (lambda (spec)
                 (setf (car spec)
                       (append '((type tty) (min-colors 16)) (car spec)))
                 spec)
               sixteen))
     (mapcar (lambda (spec)
               (setf (car spec)
                     (append '((type tty) (min-colors 8)) (car spec)))
               spec)
             eight))))

(defun create-face-spec (name facespec)
  "Generate a full face-spec for face NAME from the Solarized FACESPEC.
This generates the spec across a variety of displays from the FACESPEC, which
contains Solarized symbols."
  `(,name (,@(dark-and-light '((type graphic))
                             facespec
                             (cond (solarized-degrade     3)
                                   (solarized-broken-srgb 2)
                                   (t                     1)))
           ;; only produce 256-color term-specific settings if ‘solarized-termcolors’ is 256
           ,@(when (= solarized-termcolors 256)
               (dark-and-light '((type tty) (min-colors 256)) facespec 3))
           ,@(8-and-16 facespec))))

;; Stolen from https://emacs.stackexchange.com/a/19076
(defun plist-merge (&rest plists)
  "Create a single property list from all PLISTS.
Inspired by `org-combine-plists'."
  (let ((rtn (copy-sequence (pop plists))))
    (dolist (plist plists rtn)
      (setq rtn (plist-put rtn (pop plist) (pop plist))))))

(defun solarized-color-definitions ()
  "Produces the set of face-specs for all faces defined by this theme."
  (let ((reverse     '(:inverse-video t))
        (standout    '(:box t)) ; FIXME: not sure what standout means in Vim
        (bold        (if solarized-bold '(:weight bold) '()))
        (bright-bold (if solarized-bold '() '(:weight bold)))
        (italic      (if solarized-italic '(:slant italic) '()))
        (NONE '(:inverse-video nil :underline nil :weight normal :slant normal :box nil)))
    (cl-flet ((undercurl (sp) (list :underline (append sp '(:style wave))))
              (underline (sp) (when solarized-underline
                                (list :underline (append sp '(:style line)))))
              (opt-under (sp) (when (eq solarized-contrast 'low)
                                (list :underline (append sp '(:style line)))))
              (link (face) (list :inherit (list face))))
      (let ((bg-none   '())
            (bg-back   '(:background back))
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

            (fg-none   '())
            (fg-back   '(:foreground back))
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

            (fmt-none  (plist-merge NONE))
            (fmt-bold  (plist-merge NONE         bold))
            (fmt-bldi  (plist-merge NONE         bold))
            (fmt-ital  (plist-merge NONE                     italic))
            (fmt-stnd  (plist-merge NONE                            standout))
            (fmt-revr  (plist-merge NONE reverse))
            (fmt-revb  (plist-merge NONE reverse bold))
            (fmt-revbb (plist-merge NONE reverse bright-bold))

            ;; TODO: instead of checking ‘window-system’, generate multiple
            ;;       face-spec elements.
            (sp-none                      '())
            (sp-back    (if window-system '(:color back)    '()))
            (sp-base03  (if window-system '(:color base03)  '()))
            (sp-base02  (if window-system '(:color base02)  '()))
            (sp-base01  (if window-system '(:color base01)  '()))
            (sp-base00  (if window-system '(:color base00)  '()))
            (sp-base0   (if window-system '(:color base0)   '()))
            (sp-base1   (if window-system '(:color base1)   '()))
            (sp-base2   (if window-system '(:color base2)   '()))
            (sp-base3   (if window-system '(:color base3)   '()))
            (sp-green   (if window-system '(:color green)   '()))
            (sp-yellow  (if window-system '(:color yellow)  '()))
            (sp-orange  (if window-system '(:color orange)  '()))
            (sp-red     (if window-system '(:color red)     '()))
            (sp-magenta (if window-system '(:color magenta) '()))
            (sp-violet  (if window-system '(:color violet)  '()))
            (sp-blue    (if window-system '(:color blue)    '()))
            (sp-cyan    (if window-system '(:color cyan)    '())))
        (cl-flet ((fmt-undr   (sp) (plist-merge NONE         (underline sp)))
                  (fmt-undb   (sp) (plist-merge NONE         (underline sp) bold))
                  (fmt-undi   (sp) (plist-merge NONE         (underline sp)))
                  (fmt-uopt   (sp) (plist-merge NONE         (opt-under sp)))
                  (fmt-curl   (sp) (plist-merge NONE         (undercurl sp)))
                  (fmt-revbbu (sp) (plist-merge NONE reverse (underline sp) bright-bold)))
          (mapcar (lambda (face) (apply 'create-face-spec face))
                  `(;; basic
                    (default (,@fmt-none ,@fg-base0 ,@bg-back)) ; Normal
                    (cursor (,@fmt-none ,@fg-base03 ,@bg-base0))           ; Cursor
                    (shadow (,@fg-base01))
                    (underline (,@fmt-none ,@fg-violet ,@bg-none)) ; Underlined
                    (link (,@fg-blue ,@bg-none ,@(fmt-undb ()))) ; pandocLinkText
                    (link-visited (,@fg-magenta ,@(fmt-undr ())))
                    (match   (,@fmt-revr ,@fg-yellow ,@bg-none)) ; Search
                    (error   (,@fmt-revr ,@fg-red    ,@bg-none)) ; ErrorMsg
                    (warning (,@fmt-bold ,@fg-red    ,@bg-none)) ; WarningMsg
                    (success (,@fmt-none ,@fg-blue   ,@bg-none)) ; MoreMsg
                    (escape-glyph (,@fg-red))
                    (fringe (,@fg-base01 ,@bg-base02))
                    (linum (,@fg-base01 ,@bg-base02))
                    (header-line (,@fg-base0 ,@bg-base02 ,@fmt-revbb)) ; Pmenu
                    (highlight (,@bg-base02))
                    (hl-line (,@(fmt-uopt sp-base1) ,@fg-none ,@bg-base02)) ; CursorLine
                    (isearch (,@fmt-stnd ,@fg-orange ,@bg-none)) ; IncSearch
                    (menu (,@fg-base0 ,@bg-base02))
                    (mode-line (,@fg-base1 ,@bg-base02 ,@fmt-revbb)) ; StatusLine
                    (mode-line-inactive (,@fg-base00 ,@bg-base02 ,@fmt-revbb)) ; StatusLineNC
                    ;; NB: gets layered on mode-line, so shouldn’t inherit.
                    (mode-line-buffer-id (:weight bold))
                    (region (,@fg-base01 ,@bg-base03 ,@fmt-revbb)) ; Visual
                    (secondary-selection (,@bg-base02))
                    (shadow (,@fg-base01))
                    (trailing-whitespace (,@fmt-revr ,@fg-red))
                    (vertical-border (,@fg-base0))
                    ;; compilation
                    (compilation-info (,@fmt-bold ,@fg-green))
                    (compilation-warning (,@fmt-bold ,@fg-orange))
                    ;; custom
                    (custom-button
                     (,@fg-base1 ,@bg-base02 :box (:line-width 2 :style released-button)))
                    (custom-button-mouse
                     (,@fmt-revr ,@fg-base1 ,@bg-base02 :inherit (custom-button)))
                    (custom-button-pressed
                     (,@fmt-revr ,@fg-base1 ,@bg-base02
                                 :box (:line-width 2 :style pressed-button)
                                 :inherit (custom-button-mouse)))
                    (custom-changed (,@fmt-revr ,@fg-blue ,@bg-base3))
                    (custom-comment (,@fg-base1 ,@bg-base02))
                    (custom-comment-tag (,@fg-base1 ,@bg-base02))
                    (custom-group-tag (,@fg-base1))
                    (custom-group-tag-1 (,@fmt-bold ,@fg-base1))
                    (custom-invalid (,@fmt-revr ,@fg-red ,@bg-back))
                    (custom-state (,@fg-green))
                    (custom-variable-tag (,@fg-base1))
                    ;; diff - DiffAdd, DiffChange, DiffDelete, and DiffText
                    ,@(cl-case solarized-diff-mode
                        (high
                         `((diff-added          (,@fmt-revr ,@fg-green  ,@bg-none))
                           (diff-changed        (,@fmt-revr ,@fg-yellow ,@bg-none))
                           (diff-removed        (,@fmt-revr ,@fg-red    ,@bg-none))
                           (diff-refine-added   (,@fmt-revr ,@fg-blue   ,@bg-none))
                           (diff-refine-changed (,@fmt-revr ,@fg-blue   ,@bg-none))
                           (diff-refine-removed (,@fmt-revr ,@fg-blue   ,@bg-none))))
                        (low
                         `((diff-added          (,@(fmt-undr sp-green)  ,@fg-green  ,@bg-none))
                           (diff-changed        (,@(fmt-undr sp-yellow) ,@fg-yellow ,@bg-none))
                           (diff-removed        (,@fmt-bold             ,@fg-red    ,@bg-none))
                           (diff-refine-added   (,@(fmt-undr sp-blue)   ,@fg-blue   ,@bg-none))
                           (diff-refine-changed (,@(fmt-undr sp-blue)   ,@fg-blue   ,@bg-none))
                           (diff-refine-removed (,@(fmt-undr sp-blue)   ,@fg-blue   ,@bg-none))))
                        (normal
                         ;; TODO: Handle this by creating separate face-specs for
                         ;;       ‘graphic’ and ‘tty’ displays.
                         (if window-system
                             `((diff-added          (,@fmt-bold ,@fg-green  ,@bg-base02)) ; sp-green
                               (diff-changed        (,@fmt-bold ,@fg-yellow ,@bg-base02)) ; sp-yellow
                               (diff-removed        (,@fmt-bold ,@fg-red    ,@bg-base02))
                               (diff-refine-added   (,@fmt-bold ,@fg-blue   ,@bg-base02))
                               (diff-refine-changed (,@fmt-bold ,@fg-blue   ,@bg-base02))
                               (diff-refine-removed (,@fmt-bold ,@fg-blue   ,@bg-base02))) ; sp-blue
                           `((diff-added          (,@fmt-none ,@fg-green  ,@bg-base02)) ; sp-green
                             (diff-changed        (,@fmt-none ,@fg-yellow ,@bg-base02)) ; sp-yellow
                             (diff-removed        (,@fmt-none ,@fg-red    ,@bg-base02))
                             (diff-refine-added   (,@fmt-none ,@fg-blue   ,@bg-base02))
                             (diff-refine-changed (,@fmt-none ,@fg-blue   ,@bg-base02))
                             (diff-refine-removed (,@fmt-none ,@fg-blue   ,@bg-base02)))))) ; sp-blue
                    (diff-header (,@fg-base1))
                    ;; dired
                    (dired-ignored (,@fg-orange))
                    (dired-special (,@fg-violet))
                    ;; IDO
                    (ido-only-match (,@fg-green))
                    (ido-subdir (,@fg-blue))
                    (ido-first-match (,@fmt-bold ,@fg-green))
                    ;; eshell
                    (eshell-ls-archive (,@fg-magenta))
                    (eshell-ls-backup (,@fg-yellow))
                    (eshell-ls-product (,@fg-yellow))
                    (eshell-ls-readonly (,@fg-base1))
                    (eshell-ls-unreadable (,@fg-base00))
                    ;; font-lock
                    (font-lock-builtin-face           (,@fmt-none ,@fg-green  ,@bg-none)) ; Statement
                    (font-lock-constant-face          (,@fmt-none ,@fg-cyan   ,@bg-none)) ; Constant
                    (font-lock-doc-face               (,@fmt-ital ,@fg-base01 ,@bg-none)) ; Comment
                    (font-lock-function-name-face     (,@(link font-lock-variable-name-face))) ; Function
                    (font-lock-keyword-face           (,@(link font-lock-builtin-face)))  ; Keyword
                    (font-lock-string-face            (,@(link font-lock-constant-face))) ; String
                    (font-lock-type-face              (,@fmt-none ,@fg-yellow ,@bg-none)) ; Type
                    (font-lock-variable-name-face     (,@fmt-none ,@fg-blue   ,@bg-none)) ; Identifier
                    (font-lock-warning-face           (,@fmt-bold ,@fg-red    ,@bg-none)) ; Error
                    (font-lock-preprocessor-face      (,@fmt-none ,@fg-orange ,@bg-none)) ; PreProc
                    (font-lock-reference-face (,@fmt-none ,@fg-cyan))
                    (font-lock-negation-char-face (,@fmt-none ,@fg-red))
                    (font-lock-other-type-face (,@fmt-ital ,@fg-blue))
                    (font-lock-regexp-grouping-construct (,@fmt-none ,@fg-orange))
                    (font-lock-special-keyword-face (,@fmt-none ,@fg-red ,@bg-none)) ; Special
                    (font-lock-exit-face (,@fmt-none ,@fg-red))
                    (font-lock-other-emphasized-face (,@fmt-bldi ,@fg-violet))
                    (font-lock-regexp-grouping-backslash (,@fmt-none ,@fg-yellow))
                    ;; help
                    (help-key-binding (,@fmt-none ,@fg-orange))
                    ;; org
                    (org-block-background (,@bg-base02))
                    (org-hide (,@fg-base03))
                    (org-todo (,@fmt-bold ,@fg-base03 ,@bg-red))
                    (org-done (,@fmt-bold ,@fg-green))
                    (org-todo-kwd-face (,@fmt-bold ,@fg-base03 ,@bg-red))
                    (org-done-kwd-face (,@fmt-bold ,@fg-green))
                    (org-project-kwd-face (,@fg-violet ,@bg-base03))
                    (org-waiting-kwd-face (,@fg-orange ,@bg-base03))
                    (org-someday-kwd-face (,@fg-blue ,@bg-base03))
                    (org-started-kwd-face (,@fg-yellow ,@bg-base03))
                    (org-cancelled-kwd-face (,@fg-green ,@bg-base03))
                    (org-delegated-kwd-face (,@fg-cyan ,@bg-base03))
                    (org-special-keyword (,@fmt-ital ,@fg-base01))
                    (org-drawer (,@fmt-bold ,@fg-blue))
                    (org-column (,@fmt-revr ,@fg-cyan))
                    (org-column-title (,@fmt-bold ,@fmt-revr))
                    (org-warning (,@fmt-bold ,@fg-red))
                    (org-archived (,@fg-base01))
                    (org-footnote (,@fg-green ,@bg-none ,@fmt-none)) ; pandocFootnote
                    (org-ellipses (,@fg-yellow :strike-through t))
                    (org-target (,@(fmt-undr ())))
                    (org-date (,@(fmt-undr ()) ,@fg-violet))
                    (org-date-selected (,@fmt-revr ,@fg-red))
                    (org-sexp-date (,@(fmt-undr ()) ,@fg-violet))
                    (org-tag (,@fmt-bold))
                    (org-list-dt (,@fmt-bold))
                    (org-agenda-done (,@fg-green))
                    (org-headline-done (,@fg-base01))
                    (org-priority (,@fmt-ital ,@fg-base01))
                    (org-checkbox (,@fmt-bold))
                    (org-formula (,@fmt-bldi ,@fg-red))
                    (org-document-info (,@fg-cyan))
                    (org-document-info-keyword (,@fg-base01))
                    (org-block (,@fg-base01))
                    (org-verbatim (,@fg-yellow ,@bg-none ,@fmt-none)) ; pandocVerbatimBlock
                    (org-clock-overlay (,@fmt-revr ,@bg-cyan ,@fg-base03))
                    (org-agenda-structure (,@fmt-bold ,@fg-blue))
                    (org-scheduled (,@fmt-bldi ,@fg-green))
                    (org-scheduled-today (,@fmt-bldi ,@fg-green))
                    (org-agenda-dimmed-todo-face (,@fg-base00))
                    (org-scheduled-previously (,@fmt-bold ,@fg-red))
                    (org-upcoming-deadline (,@fmt-bold ,@fg-red))
                    (org-agenda-restriction-lock (,@fmt-revr ,@fg-base03 ,@bg-cyan))
                    (org-time-grid (,@fg-yellow))
                    (org-latex-and-related (,@fg-orange))
                    ;; show-paren
                    (show-paren-match    (,@fmt-bold ,@fg-cyan ,@bg-base01))
                    (show-paren-mismatch (,@fmt-bold ,@fg-red  ,@bg-base01)) ; MatchParen
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
                                 :inherit (speedbar-button-face)))
                    ;; (speedbar-selected-face (,@(fmt-undr ()) ,@fg-yellow))
                    (speedbar-selected-face (,@fmt-none ,@bg-base02 ,@fg-green))
                    (speedbar-separator-face (,@fmt-stnd))
                    (speedbar-tag-face (,@fmt-none ,@fg-blue))
                    ;; widgets
                    (widget-field
                     (,@fg-base1 ,@bg-base02 :box (:line-width 1 :color base2)
                                 :inherit ()))

                    ;; third-party packages

                    ;; alert
                    (alert-urgent (,@fg-red))
                    (alert-high (,@fg-orange))
                    (alert-moderate (,@fg-yellow))
                    (alert-normal (,@fg-green))
                    (alert-low (,@fg-blue))
                    (alert-trivial (,@fg-violet))
                    ;; auto-dim-other-buffers
                    (auto-dim-other-buffers-face (,@bg-base02)) ; match fringe
                    (auto-dim-other-buffers-hide-face (,@fg-base02))
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
                    (flymake-errline (,@fmt-bold ,@fg-red ,@bg-none)) ; Error
                    (flymake-warnline (,@fmt-bold ,@fg-red))
                    ;; column-marker
                    (column-marker-1 (,@bg-base01))
                    (column-marker-2 (,@bg-cyan))
                    (column-marker-3 (,@bg-violet))
                    ;; extended-faces
                    (black (,@fg-base02))
                    (blue (,@fg-blue))
                    (cyan (,@fg-cyan))
                    (fs-directory (,@fmt-none ,@fg-blue ,@bg-none)) ; Directory
                    (fs-executable (,@fg-green))
                    (fs-broken-symlink (,@fg-red))
                    (fs-symlink (,@fg-cyan))
                    (green (,@fg-green))
                    (level-1  (,@fg-blue    ,@bg-none ,@fmt-none)) ; pandocBlockQuoteLeader1
                    (level-2  (,@fg-cyan    ,@bg-none ,@fmt-none)) ; pandocBlockQuoteLeader2
                    (level-3  (,@fg-yellow  ,@bg-none ,@fmt-none)) ; pandocBlockQuoteLeader3
                    (level-4  (,@fg-red     ,@bg-none ,@fmt-none)) ; pandocBlockQuoteLeader4
                    (level-5  (,@fg-base0   ,@bg-none ,@fmt-none)) ; pandocBlockQuoteLeader5
                    (level-6  (,@fg-base01  ,@bg-none ,@fmt-none)) ; pandocBlockQuoteLeader6
                    (level-7  (,@fg-orange  ,@bg-none ,@fmt-none))
                    (level-8  (,@fg-violet  ,@bg-none ,@fmt-none))
                    (level-9  (,@fg-green   ,@bg-none ,@fmt-none))
                    (level-10 (,@fg-magenta ,@bg-none ,@fmt-none))
                    (level-11 (,@fg-base02  ,@bg-none ,@fmt-none))
                    (magenta (,@fg-magenta))
                    (prompt (,@fmt-bold ,@fg-cyan ,@bg-none)) ; Question
                    (red (,@fg-red))
                    (table (,@fg-blue ,@bg-none ,@fmt-none)) ; pandocTable
                    (text-verbatim (,@fg-yellow ,@bg-none ,@fmt-none)) ; pandocVerbatimBlock
                    (white (,@fg-base2))
                    (yellow (,@fg-yellow))
                    ;; jabber
                    (jabber-activity-face (,@fmt-bold ,@fg-red))
                    (jabber-activity-personal-face (,@fmt-bold ,@fg-blue))
                    (jabber-chat-error (,@fmt-bold ,@fg-red))
                    (jabber-chat-prompt-foreign (,@fmt-bold ,@fg-red))
                    (jabber-chat-prompt-local (,@fmt-bold ,@fg-blue))
                    (jabber-chat-prompt-system (,@fmt-bold ,@fg-green))
                    (jabber-chat-text-foreign (,@fg-base1))
                    (jabber-chat-text-local (,@fg-base0))
                    (jabber-chat-rare-time-face (,@(fmt-undr ()) ,@fg-green))
                    (jabber-roster-user-away (,@fmt-ital ,@fg-green))
                    (jabber-roster-user-chatty (,@fmt-bold ,@fg-orange))
                    (jabber-roster-user-dnd (,@fmt-ital ,@fg-red))
                    (jabber-roster-user-error (,@fmt-bold ,@fg-red ,@bg-none)) ; Error
                    (jabber-roster-user-offline (,@fg-base01))
                    (jabber-roster-user-online (,@fmt-bold ,@fg-blue))
                    (jabber-roster-user-xa (,@fmt-ital ,@fg-magenta))
                    ;; git-commit
                    (git-commit-branch-local    (,@fmt-bold ,@fg-magenta ,@bg-none)) ; gitcommitBranch
                    (git-commit-branch-remote   (,@fmt-bold ,@fg-magenta ,@bg-none)) ; gitcommitBranch
                    (git-commit-comment-file    (,@fmt-bold ,@fg-base0   ,@bg-none)) ; gitcommitFile
                    (git-commit-comment-heading (,@fmt-ital ,@fg-base01  ,@bg-none)) ; gitcommitComment
                    ;; git-gutter
                    (git-gutter:modified (,@fg-violet))
                    (git-gutter:added (,@fg-green))
                    (git-gutter:deleted (,@fg-red))
                    ;; gnus - these are mostly taken from mutt, not VIM
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
                    (gnus-header-from (,@fmt-none ,@fg-base00))    ; header ^From
                    (gnus-header-name (,@fmt-none ,@fg-base01))    ; hdrdefault
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
                    ;;helm
                    (helm-apt-deinstalled (,@fg-base01))
                    (helm-apt-installed (,@fg-green))
                    (helm-bookmark-addressbook (,@fg-blue))
                    (helm-bookmark-gnus (,@fg-cyan))
                    (helm-bookmark-info (,@fg-green))
                    (helm-bookmark-man (,@fg-violet))
                    (helm-bookmark-w3m (,@fg-yellow))
                    (helm-bookmarks-su (,@fg-orange))
                    (helm-buffer-directory (,@bg-back ,@fg-blue))
                    (helm-buffer-not-saved (,@fg-orange))
                    (helm-buffer-process (,@fg-magenta))
                    (helm-buffer-saved-out (,@fmt-revr ,@fg-red ,@bg-back))
                    (helm-buffer-size (,@fg-base01))
                    (helm-candidate-number (,@fmt-bold ,@bg-base02 ,@fg-base1))
                    (helm-emms-playlist (,@fmt-none ,@fg-base01))
                    (helm-ff-executable (,@fmt-bold ,@fg-green))
                    (helm-ff-invalid-symlink (,@bg-base02 ,@fg-red))
                    (helm-ff-prefix (,@fmt-revr ,@fg-yellow))
                    (helm-ff-symlink (,@fmt-bold ,@fg-cyan))
                    (helm-grep-cmd-line (:inherit (diff-added)))
                    (helm-grep-file (,@(fmt-undr ()) ,@fg-cyan))
                    (helm-grep-finish (,@fg-green))
                    (helm-grep-lineno (,@fg-orange))
                    (helm-grep-running (,@fg-red))
                    (helm-helper (:inherit (helm-header)))
                    (helm-history-deleted (:inherit (helm-ff-invalid-symlink)))
                    (helm-history-remote (,@fg-red))
                    (helm-lisp-completion-info (,@fg-base0))
                    (helm-lisp-show-completion (,@fmt-bold ,@fg-yellow ,@bg-base02))
                    (helm-ls-git-added-copied-face (,@fg-green))
                    (helm-ls-git-conflict-face (,@fmt-bold ,@fg-red))
                    (helm-ls-git-deleted-and-staged-face (,@fmt-ital ,@fg-base01))
                    (helm-ls-git-deleted-not-staged-face (,@fmt-bold ,@fg-green))
                    (helm-ls-git-modified-and-staged-face (,@fmt-ital ,@fg-base01))
                    (helm-ls-git-modified-not-staged-face (,@fmt-ital ,@fg-base01))
                    (helm-ls-git-renamed-modified-face (,@fg-green))
                    (helm-ls-git-untracked-face (,@fg-red))
                    (helm-moccur-buffer (,@(fmt-undr ()) ,@fg-cyan))
                    (helm-separator (,@fg-red))
                    (helm-time-zone-current (,@fg-green))
                    (helm-time-zone-home (,@fg-red))
                    (helm-visible-mark (,@fmt-bold ,@bg-back ,@fg-magenta))
                    ;; lsp-headerline
                    (lsp-headerline-breadcrumb-path-error-face (,@(fmt-curl sp-red)))
                    (lsp-headerline-breadcrumb-path-hint-face (,@(fmt-curl sp-green)))
                    (lsp-headerline-breadcrumb-path-info-face (,@(fmt-curl sp-green)))
                    (lsp-headerline-breadcrumb-path-warning-face (,@(fmt-curl sp-yellow)))
                    (lsp-headerline-breadcrumb-symbols-error-face (,@(fmt-curl sp-red)))
                    (lsp-headerline-breadcrumb-symbols-hint-face (,@(fmt-curl sp-green)))
                    (lsp-headerline-breadcrumb-symbols-info-face (,@(fmt-curl sp-green)))
                    (lsp-headerline-breadcrumb-symbols-warning-face (,@(fmt-curl sp-yellow)))
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
                    ;; powerline
                    (powerline-active1 (,@fg-base00))
                    (powerline-active2 (,@fg-base0))
                    (powerline-inactive1 (,@fg-base02 ,@bg-base1))
                    (powerline-inactive2 (,@fg-base01))
                    ;; slime
                    (slime-error-face (,@fmt-revr ,@fg-red ,@bg-none)) ; ErrorMsg
                    (slime-note-face (,@fg-yellow))
                    (slime-repl-inputted-output-face (,@fg-red))
                    (slime-repl-output-mouseover-face (:box (:color base3)))
                    (slime-style-warning-face (,@fmt-bold ,@fg-orange))
                    (slime-warning-face (,@fmt-bold ,@fg-red ,@bg-none)) ; WarningMsg
                    ;; smartparens
                    (sp-pair-overlay-face (,@bg-base02))
                    (sp-wrap-overlay-face (,@bg-base02))
                    (sp-wrap-tag-overlay-face (,@bg-base02))
                    (sp-show-pair-match-face (,@fg-magenta ,@bg-back))
                    (sp-show-pair-mismatch-face (,@bg-red ,@fg-base02))
                    ;; which-key
                    (which-key-group-description-face (,@fg-yellow))
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
                    (whitespace-newline (:inherit (shadow) :slant normal))
                    ;; writegood
                    (writegood-weasels-face (,@(fmt-curl ()) ,@fg-cyan))
                    (writegood-passive-voice-face (,@fg-magenta))
                    ;; rcirc
                    (rcirc-my-nick (,@fg-blue))
                    (rcirc-nick-in-message (,@fg-orange))
                    (rcirc-other-nick (,@fg-green))
                    (rcirc-bright-nick (,@fg-magenta))
                    (rcirc-server (,@fg-base1))
                    (rcirc-timestamp (,@fg-base01))
                    ;;font-latex
                    (font-latex-sectioning-5-face (,@fg-violet))
                    ;;flyspell
                    ;; NB: kill inheritance here, to get rid of the overpowering
                    ;;     ‘error’/‘warning’ faces from ‘inheritance’.
                    (flyspell-incorrect (,@(fmt-curl sp-red)    ,@fg-none ,@bg-none :inherit ())) ; SpellBad
                    (flyspell-duplicate (,@(fmt-curl sp-yellow) ,@fg-none ,@bg-none :inherit ()))
                    ;;ansi-color
                    (ansi-color-cyan (,@fg-cyan ,@bg-cyan))
                    (ansi-color-blue (,@fg-blue ,@bg-blue))
                    (ansi-color-magenta (,@fg-magenta ,@bg-magenta))
                    (ansi-color-red (,@fg-red ,@bg-red))
                    (ansi-color-yellow (,@fg-yellow ,@bg-yellow))
                    (ansi-color-green (,@fg-green ,@bg-green))
                    (ansi-color-black (,@fg-base02 ,@bg-base02))
                    (ansi-color-white (,@fg-base2 ,@bg-base2))
                    (ansi-color-bright-cyan (,@fg-base1 ,@bg-base1))
                    (ansi-color-bright-blue (,@fg-base0 ,@bg-base0))
                    (ansi-color-bright-magenta (,@fg-violet ,@bg-violet))
                    (ansi-color-bright-red (,@fg-orange ,@bg-orange))
                    (ansi-color-bright-yellow (,@fg-base00 ,@bg-base00))
                    (ansi-color-bright-green (,@fg-base01 ,@bg-base01))
                    (ansi-color-bright-black (,@fg-base03 ,@bg-base03))
                    (ansi-color-bright-white (,@fg-base3 ,@bg-base3))
                    ;; company
                    (company-tooltip (,@fg-base00 ,@bg-base02))
                    (company-tooltip-selection (,@fg-green ,@bg-base02))
                    (company-tooltip-mouse (,@fg-base1 ,@bg-base02))
                    (company-tooltip-common (,@fg-blue ,@bg-base02 ,@(fmt-undr ())))
                    (company-tooltip-common-selection (,@fg-green ,@bg-base02 ,@(fmt-undr ())))
                    (company-tooltip-annotation (,@fg-yellow ,@bg-base02))
                    (company-scrollbar-fg (,@bg-base0))
                    (company-scrollbar-bg (,@bg-base02))
                    (company-preview (,@bg-green))
                    (company-preview-common (,@fg-base01 ,@bg-base02))
                    (company-template-field (,@fg-base03 ,@bg-yellow))
                    ;; hydra
                    ;; NB: take advantage of the two extra colors between blue and
                    ;;     yellow.
                    (hydra-face-teal     (,@fmt-bold ,@fg-cyan))
                    (hydra-face-blue     (,@fmt-bold ,@fg-blue))
                    (hydra-face-pink     (,@fmt-bold ,@fg-violet))
                    (hydra-face-amaranth (,@fmt-bold ,@fg-magenta))
                    (hydra-face-red      (,@fmt-bold ,@fg-red))
                    ;; guide-key
                    (guide-key/prefix-command-face (,@fg-blue))
                    (guide-key/highlight-command-face (,@fg-orange))
                    ;; magit
                    (magit-branch-local          (,@fmt-bold ,@fg-magenta ,@bg-none)) ; gitcommitBranch
                    (magit-branch-remote         (           ,@fg-green))
                    (magit-diff-file-heading     (,@fmt-none ,@fg-base01  ,@bg-none)) ; gitcommitHeader
                    (magit-diff-revision-summary (,@fmt-ital ,@fg-base01  ,@bg-none)) ; gitcommitComment
                    (magit-log-sha1 (,@fg-red))
                    (magit-tag (,@fg-green))
                    (magit-log-author (,@fg-cyan))
                    (magit-log-head-label-remote (,@fg-green))
                    (magit-log-head-label-tags (,@fg-orange))
                    (magit-log-head-label-local (,@fg-yellow))
                    (magit-log-head-label-head (,@fg-violet))
                    ;; transient
                    ;; NB: take advantage of the two extra colors between blue and
                    ;;     yellow.
                    (transient-teal     (,@fg-cyan))
                    (transient-blue     (,@fg-blue))
                    (transient-purple   (,@fg-violet))
                    (transient-pink     (,@fg-magenta))
                    (transient-amaranth (,@fg-red))
                    (transient-red      (,@fg-orange))
                    ;; undo-tree
                    (undo-tree-visualizer-current-face (,@fg-orange))
                    (undo-tree-visualizer-default-face (:inherit (shadow)))
                    (undo-tree-visualizer-active-branch-face (:inherit ()))
                    (undo-tree-visualizer-unmodified-face (,@fg-cyan))
                    (undo-tree-visualizer-register-face (,@fg-yellow))
                    ;; haskell
                    (haskell-keyword-face (,@fg-cyan)))))))))

(provide 'solarized-definitions)
;;; solarized-definitions.el ends here
