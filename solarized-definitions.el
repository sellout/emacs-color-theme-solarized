(eval-when-compile
  (require 'cl))

(defconst solarized-description
  "Color theme by Ethan Schoonover, created 2011-03-24.
Ported to Emacs by Greg Pfeil, http://ethanschoonover.com/solarized.")

(defcustom solarized-degrade nil
  "For test purposes only; when in GUI mode, forces Solarized to use the 256 degraded color mode
to test the approximate color values for accuracy."
  :type 'boolean
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

(defcustom solarized-termcolors 16
  "This setting applies to emacs in terminal (non-GUI) mode.
If set to 16, emacs will use the terminal emulator's colorscheme
(best option as long as you've set your emulator's colors to the Solarized palette).
If set to 256 and your terminal is capable of displaying 256 colors, emacs
will use the 256 degraded color mode."
  :type 'integer
  :group 'solarized)

;; FIXME: The Generic RGB colors will actually vary from device to device, but
;;        hopefully these are closer to the intended colors than the sRGB values
;;        that Emacs seems to dislike
(defvar solarized-colors
  ;; name    sRGB      Gen RGB   degraded  ANSI(Solarized terminal)
  '((base03  "#002b36" "#042028" "#1c1c1c" "#7f7f7f")
    (base02  "#073642" "#0a2832" "#262626" "#000000")
    (base01  "#586e75" "#465a61" "#4e4e4e" "#00ff00")
    (base00  "#657b83" "#52676f" "#585858" "#ffff00")
    (base0   "#839496" "#708183" "#808080" "#5c5cff")
    (base1   "#93a1a1" "#81908f" "#8a8a8a" "#00ffff")
    (base2   "#eee8d5" "#e9e2cb" "#d7d7af" "#e5e5e5")
    (base3   "#fdf6e3" "#fcf4dc" "#ffffd7" "#ffffff")
    (yellow  "#b58900" "#a57705" "#af8700" "#cdcd00")
    (orange  "#cb4b16" "#bd3612" "#d75f00" "#ff0000")
    (red     "#dc322f" "#c60007" "#af0000" "#cd0000")
    (magenta "#d33682" "#c61b6e" "#af005f" "#cd00cd")
    (violet  "#6c71c4" "#5859b7" "#5f5faf" "#ff00ff")
    (blue    "#268bd2" "#2075c7" "#0087ff" "#0000ee")
    (cyan    "#2aa198" "#259185" "#00afaf" "#00cdcd")
    (green   "#859900" "#728a05" "#5f8700" "#00cd00"))
  "This is a table of all the colors used by the Solarized color theme. Each
   column is a different set, one of which will be chosen based on term
   capabilities, etc.")

(defun solarized-face-for-index (facespec index)
  "Creates a face from FACESPEC where the colors use the names of
  the `solarized-colors'."
  (let ((new-fontspec (copy-list facespec)))
    (dolist (property '(:foreground :background :color))
      (when (plist-get new-fontspec property)
       (plist-put new-fontspec property
                  (nth index (assoc (plist-get new-fontspec property)
                                    solarized-colors)))))
    (when (plist-get new-fontspec :box)
      (plist-put new-fontspec :box (solarized-face-for-index
                                    (plist-get new-fontspec :box) index)))
    new-fontspec))

(defun solarized-faces (facespecs)
  (mapcar (lambda (facespec-with-name)
            (let* ((name (car facespec-with-name))
                   (facespec (second facespec-with-name))
                   (facespec-tty-256 (solarized-face-for-index facespec 3))
                   (facespec-tty-term (solarized-face-for-index facespec 4))
                   (facespec-default (solarized-face-for-index facespec 2)))
              `(,name
                ((((type tty) (min-colors 256)) ,facespec-tty-256)
                 (((type tty)) ,facespec-tty-term)
                 (t ,facespec-default))))) facespecs))

(defun solarized-color-definitions (mode)
  (let ((background-extreme (if (eq 'light mode) 'base3 'base03))
        (background-medium  (if (eq 'light mode) 'base2 'base02))
        (background-subtle  (if (eq 'light mode) 'base1 'base01))
        (background-grey    (if (eq 'light mode) 'base0 'base00))
        (foreground-extreme (if (eq 'light mode) 'base03 'base3))
        (foreground-medium  (if (eq 'light mode) 'base02 'base2))
        (foreground-subtle  (if (eq 'light mode) 'base01 'base1))
        (foreground-grey    (if (eq 'light mode) 'base00 'base0))
        (bold               (if solarized-bold 'bold 'normal))
        (underline          (if solarized-underline t nil))
        (italic             (if solarized-italic 'italic 'normal)))
    (list
     ;; First list is for faces
     (append
      (solarized-faces
       `( ;; basic
         (default (:foreground ,foreground-grey :background ,background-extreme))
         (cursor (:foreground ,foreground-grey :background ,background-extreme :inverse-video t))
         (fringe (:foreground ,background-subtle :background ,background-medium))
         (header-line (:foreground ,foreground-grey :background ,foreground-medium))
         (highlight (:background ,background-medium))
         (hl-line (:background ,background-medium))
         (isearch (:foreground yellow :inverse-video t))
         (lazy-highlight (:background ,foreground-medium :foreground ,background-grey))
         (link (:foreground violet :,underline ,underline))
         (link-visited (:foreground magenta :,underline ,underline))
         (menu (:foreground ,foreground-grey :background ,background-medium))
         (minibuffer-prompt (:foreground blue))
         (mode-line (:foreground ,foreground-subtle :background ,background-medium
                                 :box (:line-width 1 :color ,foreground-subtle)))
         (mode-line-buffer-id (:foreground ,foreground-subtle))
         (mode-line-inactive (:foreground ,foreground-grey  :background ,background-medium
                                          :box (:line-width 1 :color ,background-medium)))
         (region (:background ,background-medium))
         (secondary-selection (:background ,background-medium))
         (trailing-whitespace (:foreground red :inverse-video t))
         (vertical-border (:foreground ,foreground-grey))
         ;; comint
         (comint-highlight-prompt (:foreground blue))
         ;; compilation
         (compilation-info (:foreground green :weight ,bold))
         (compilation-warning (:foreground orange :weight ,bold))
         ;; customize
         (custom-button (:background ,background-medium
                                     :box (:line-width 2 :style released-button)))
         (custom-button-mouse (:inherit custom-button :foreground ,foreground-subtle))
         (custom-button-pressed (:inherit custom-button-mouse
                                          :box (:line-width 2 :style pressed-button)))
         (custom-comment-tag (:background ,background-medium))
         (custom-comment-tag (:background ,background-medium))
         (custom-documentation (:inherit default))
         (custom-group-tag (:foreground orange :weight ,bold))
         (custom-link (:foreground violet))
         (custom-state (:foreground green))
         (custom-variable-tag (:foreground orange :weight ,bold))
         ;; diff
         (diff-added (:foreground green :inverse-video t))
         (diff-changed (:foreground yellow :inverse-video t))
         (diff-removed (:foreground red :inverse-video t))
         (diff-header (:background ,background-subtle))
         (diff-file-header (:background ,foreground-subtle :foreground ,background-subtle :weight ,bold))
         (diff-refine-change (:background ,foreground-subtle))
         ;; emacs-wiki
         (emacs-wiki-bad-link-face (:foreground red :,underline ,underline))
         (emacs-wiki-link-face (:foreground blue :,underline ,underline))
         (emacs-wiki-verbatim-face (:foreground ,background-grey :,underline ,underline))
         ;; font-lock
         (font-lock-builtin-face (:foreground green))
         (font-lock-comment-face (:foreground ,background-subtle :slant ,italic))
         (font-lock-constant-face (:foreground cyan))
         (font-lock-function-name-face (:foreground blue))
         (font-lock-keyword-face (:foreground green))
         (font-lock-string-face (:foreground cyan))
         (font-lock-type-face (:foreground yellow))
         (font-lock-variable-name-face (:foreground blue))
         (font-lock-warning-face (:foreground red :weight ,bold))
         (font-lock-doc-face (:foreground cyan :slant ,italic))
         (font-lock-color-constant-face (:foreground green))
         (font-lock-comment-delimiter-face (:foreground ,background-subtle :weight ,bold))
         (font-lock-doc-string-face (:foreground green))
         (font-lock-preprocessor-face (:foreground orange))
         (font-lock-reference-face (:foreground cyan))
         (font-lock-negation-char-face (:foreground red))
         (font-lock-other-type-face (:foreground blue :slant ,italic))
         (font-lock-regexp-grouping-construct    (:foreground orange))
         (font-lock-special-keyword-face (:foreground magenta))
         (font-lock-exit-face (:foreground red))
         (font-lock-other-emphasized-face (:foreground violet :weight ,bold :slant ,italic))
         (font-lock-regexp-grouping-backslash (:foreground yellow))
         ;; info
         (info-xref (:foreground blue :,underline ,underline))
         (info-xref-visited (:inherit info-xref :foreground magenta))
         ;; org
         (org-hide (:foreground ,background-extreme))
         (org-todo (:foreground ,background-extreme :background red :weight ,bold))
         (org-done (:foreground green :weight ,bold))
         (org-todo-kwd-face (:foreground red :background ,background-extreme))
         (org-done-kwd-face (:foreground green :background ,background-extreme))
         (org-project-kwd-face (:foreground violet :background ,background-extreme))
         (org-waiting-kwd-face (:foreground orange :background ,background-extreme))
         (org-someday-kwd-face (:foreground blue :background ,background-extreme))
         (org-started-kwd-face (:foreground yellow :background ,background-extreme))
         (org-cancelled-kwd-face (:foreground green :background ,background-extreme))
         (org-delegated-kwd-face (:foreground cyan :background ,background-extreme))
         ;; show-paren
         (show-paren-match-face (:background cyan :foreground ,foreground-extreme))
         (show-paren-mismatch-face (:background red :foreground ,foreground-extreme))
         ;; widgets
         (widget-field (:box (:line-width 1 :color ,background-grey) :inherit default))
         (widget-single-line-field (:inherit widget-field))
         ;; extra modules
         ;; -------------
         ;; gnus
         (gnus-cite-1 (:foreground magenta))
         (gnus-cite-2 (:foreground ,foreground-medium))
         (gnus-cite-3 (:foreground ,foreground-extreme))
         (gnus-cite-4 (:foreground ,foreground-subtle))
         (gnus-cite-5 (:foreground magenta))
         (gnus-cite-6 (:foreground ,foreground-medium))
         (gnus-cite-7 (:foreground ,foreground-extreme))
         (gnus-cite-8 (:foreground ,foreground-subtle))
         (gnus-cite-9 (:foreground ,foreground-medium))
         (gnus-cite-10 (:foreground ,foreground-extreme))
         (gnus-cite-11 (:foreground blue))
         (gnus-group-mail-1 (:foreground ,foreground-extreme :weight ,bold))
         (gnus-group-mail-1-empty (:foreground ,foreground-extreme))
         (gnus-group-mail-2 (:foreground ,foreground-medium :weight ,bold))
         (gnus-group-mail-2-empty (:foreground ,foreground-medium))
         (gnus-group-mail-3 (:foreground magenta :weight ,bold))
         (gnus-group-mail-3-empty (:foreground magenta))
         (gnus-group-mail-low (:foreground ,background-grey :weight ,bold))
         (gnus-group-mail-low-empty (:foreground ,background-grey))
         (gnus-group-news-1 (:foreground ,foreground-subtle :weight ,bold))
         (gnus-group-news-1-empty (:foreground ,foreground-subtle))
         (gnus-group-news-2 (:foreground blue :weight ,bold))
         (gnus-group-news-2-empty (:foreground blue))
         (gnus-group-news-low (:foreground violet :weight ,bold))
         (gnus-group-news-low-empty (:foreground violet))
         (gnus-header-content (:foreground cyan :slant ,italic))
         (gnus-header-from (:foreground ,foreground-medium))
         (gnus-header-name (:foreground blue))
         (gnus-header-newsgroups (:foreground green :slant ,italic))
         (gnus-header-subject (:foreground ,foreground-subtle))
         (gnus-server-agent (:foreground ,foreground-extreme :weight ,bold))
         (gnus-server-closed (:foreground ,foreground-subtle :slant ,italic))
         (gnus-server-denied (:foreground ,foreground-medium :weight ,bold))
         (gnus-server-offline (:foreground green :weight ,bold))
         (gnus-server-opened (:foreground cyan :weight ,bold))
         (gnus-splash (:foreground ,foreground-medium))
         (gnus-summary-high-ancient (:foreground magenta :weight ,bold))
         (gnus-summary-high-read (:foreground ,foreground-subtle :weight ,bold))
         (gnus-summary-high-ticked (:foreground ,foreground-extreme :weight ,bold))
         (gnus-summary-high-undownloaded (:foreground ,foreground-medium :weight ,bold))
         (gnus-summary-low-ancient (:foreground magenta :slant ,italic))
         (gnus-summary-low-read (:foreground ,foreground-subtle :slant ,italic))
         (gnus-summary-low-ticked (:foreground ,foreground-extreme :slant ,italic))
         (gnus-summary-low-undownloaded (:foreground ,foreground-medium :slant ,italic))
         (gnus-summary-normal-ancient (:foreground magenta))
         (gnus-summary-normal-read (:foreground ,foreground-subtle))
         (gnus-summary-normal-ticked (:foreground ,foreground-extreme))
         (gnus-summary-normal-undownloaded (:foreground ,foreground-medium))
         ;; Flymake
         (flymake-errline (:background orange))
         (flymake-warnline (:background violet))
         ;; whitespace
         (whitespace-empty (:foreground red))
         (whitespace-hspace (:foreground orange))
         (whitespace-indentation (:foreground ,background-medium))
         (whitespace-space (:foreground ,background-medium))
         (whitespace-space-after-tab (:foreground cyan))
         (whitespace-space-before-tab (:foreground red :weight ,bold))
         (whitespace-tab (:foreground ,background-medium))
         (whitespace-trailing
          (:background ,background-medium :foreground red :weight ,bold))
         (whitespace-highlight-face (:background blue :foreground red))
         ;; Message
         (message-mml (:foreground blue))
         (message-cited-text (:foreground ,foreground-medium))
         (message-separator (:foreground ,foreground-extreme))
         (message-header-xheader (:foreground violet))
         (message-header-name (:foreground cyan))
         (message-header-other (:foreground red))
         (message-header-newsgroups
          (:foreground yellow :weight ,bold :slant ,italic))
         (message-header-subject (:foreground ,background-grey))
         (message-header-cc (:foreground green :weight ,bold))
         (message-header-to (:foreground ,foreground-subtle :weight ,bold)))))
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

(provide 'solarized-definitions)
