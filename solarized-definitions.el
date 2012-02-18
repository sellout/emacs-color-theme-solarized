(eval-when-compile
  (require 'cl))

(defconst solarized-description
  "Color theme by Ethan Schoonover, created 2011-03-24.
Ported to Emacs by Greg Pfeil, http://ethanschoonover.com/solarized.")

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
  "This is a table of all the colors used by the solarized color theme. each
   column is a different set, one of which will be chosen based on term
   capabilities, etc.")

(defun solarized-face-for-index (facespec index)
  "Creates a face from facespec where the colors use the names of
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

(defun solarized-flip (facespec)
  "Convert a facespec to its lightened or darkened counterpart"
  (let* ((reversing-alist '((base03 . base3) (base02 . base2) (base01 . base1)
                            (base00 . base0) (base0 . base00) (base1 . base01)
                            (base2 . base02) (base3 . base03))))
    (mapcar (lambda (term) (cond ((listp term) (solarized-flip term)) 
                            ((assoc term reversing-alist)
                             (cdr (assoc term reversing-alist)))
                            (t term))) facespec)))

(defun solarized-faces (facespecs mode)
  (mapcar (lambda (facespec-with-name)
            (let* ((name (car facespec-with-name))
                   (facespec (funcall
                              (if (eq mode 'dark) 'solarized-flip 'identity)
                              (second facespec-with-name)))
                   (flipped-facespec (solarized-flip facespec))
                   (facespec-tty-256 (solarized-face-for-index facespec 3))
                   (facespec-tty-term (solarized-face-for-index facespec 4))
                   (facespec-default (solarized-face-for-index facespec 2)))
              `(,name
                ((((min-colors 257)) ,facespec-default)
                 (((min-colors 256)) ,facespec-tty-256)
                 (((min-colors 16)) ,facespec-tty-term)
                 ;; We should rarely if ever fall to the default.  If
                 ;; so, let's set it to the default light spec and
                 ;; hope for the best.
                 (t ,facespec-default))))) facespecs))

(defun solarized-color-definitions (mode)
  "Define colors that make up Solarized."
  ;; We define everything for light mode, but we generate dark mode
  ;; automatically in `solarized-faces.'
  ;;
  ;; For light mode, here's what the colors mean,
  ;; from http://ethanschoonover.com/solarized#features:
  ;; base3 - background
  ;; base2 - background highlights
  ;; base1 - comments / secondary content
  ;; base0
  ;; base00 - body text / default code / primary content
  ;; base01 - optional enhanced content
  ;; base02
  ;; base03
  (let ((bold               (if solarized-bold 'bold 'normal))
        (underline          (if solarized-underline t nil))
        (italic             (if solarized-italic 'italic 'normal)))
    (list
     ;; First list is for faces
     (append
      (solarized-faces
       `( ;; basic
         (default (:foreground base00 :background base3))
         (cursor (:foreground base00 :background base3 :inverse-video t))
         (fringe (:foreground base1 :background base2))
         (header-line (:foreground base00 :background base02))
         (highlight (:background base2))
         (hl-line (:background base2))
         (isearch (:foreground yellow :inverse-video t))
         (lazy-highlight (:background base02 :foreground base0))
         (link (:foreground violet :,underline ,underline))
         (link-visited (:foreground magenta :,underline ,underline))
         (menu (:foreground base00 :background base2))
         (minibuffer-prompt (:foreground blue))
         (mode-line (:foreground base01 :background base2
                                 :box (:line-width 1 :color base01)))
         (mode-line-buffer-id (:foreground base01))
         (mode-line-inactive (:foreground base00  :background base2
                                          :box (:line-width 1 :color base2)))
         (region (:background base2))
         (secondary-selection (:background base2))
         (trailing-whitespace (:foreground red :inverse-video t))
         (vertical-border (:foreground base00))
         ;; comint
         (comint-highlight-prompt (:foreground blue))
         ;; compilation
         (compilation-info (:foreground green :weight ,bold))
         (compilation-warning (:foreground orange :weight ,bold))
         ;; customize
         (custom-button (:background base2
                                     :box (:line-width 2 :style released-button)))
         (custom-button-mouse (:inherit custom-button :foreground base01))
         (custom-button-pressed (:inherit custom-button-mouse
                                          :box (:line-width 2 :style pressed-button)))
         (custom-comment-tag (:background base2))
         (custom-comment-tag (:background base2))
         (custom-documentation (:inherit default))
         (custom-group-tag (:foreground orange :weight ,bold))
         (custom-link (:foreground violet))
         (custom-state (:foreground green))
         (custom-variable-tag (:foreground orange :weight ,bold))
         ;; diff
         (diff-added (:foreground green :inverse-video t))
         (diff-changed (:foreground yellow :inverse-video t))
         (diff-removed (:foreground red :inverse-video t))
         (diff-header (:background base1))
         (diff-file-header (:background base01 :foreground base1 :weight ,bold))
         (diff-refine-change (:background base01))
         ;; emacs-wiki
         (emacs-wiki-bad-link-face (:foreground red :,underline ,underline))
         (emacs-wiki-link-face (:foreground blue :,underline ,underline))
         (emacs-wiki-verbatim-face (:foreground base0 :,underline ,underline))
         ;; font-lock
         (font-lock-builtin-face (:foreground green))
         (font-lock-comment-face (:foreground base1 :slant ,italic))
         (font-lock-constant-face (:foreground cyan))
         (font-lock-function-name-face (:foreground blue))
         (font-lock-keyword-face (:foreground green))
         (font-lock-string-face (:foreground cyan))
         (font-lock-type-face (:foreground yellow))
         (font-lock-variable-name-face (:foreground blue))
         (font-lock-warning-face (:foreground red :weight ,bold))
         (font-lock-doc-face (:foreground cyan :slant ,italic))
         (font-lock-color-constant-face (:foreground green))
         (font-lock-comment-delimiter-face (:foreground base1 :weight ,bold))
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
         (org-hide (:foreground base3))
         (org-todo (:foreground base3 :background red :weight ,bold))
         (org-done (:foreground green :weight ,bold))
         (org-todo-kwd-face (:foreground red :background base3))
         (org-done-kwd-face (:foreground green :background base3))
         (org-project-kwd-face (:foreground violet :background base3))
         (org-waiting-kwd-face (:foreground orange :background base3))
         (org-someday-kwd-face (:foreground blue :background base3))
         (org-started-kwd-face (:foreground yellow :background base3))
         (org-cancelled-kwd-face (:foreground green :background base3))
         (org-delegated-kwd-face (:foreground cyan :background base3))
         ;; show-paren
         (show-paren-match-face (:background cyan :foreground base03))
         (show-paren-mismatch-face (:background red :foreground base03))
         ;; widgets
         (widget-field (:box (:line-width 1 :color base0) :inherit default))
         (widget-single-line-field (:inherit widget-field))
         ;; extra modules
         ;; -------------
         ;; gnus
         (gnus-cite-1 (:foreground magenta))
         (gnus-cite-2 (:foreground base02))
         (gnus-cite-3 (:foreground base03))
         (gnus-cite-4 (:foreground base01))
         (gnus-cite-5 (:foreground magenta))
         (gnus-cite-6 (:foreground base02))
         (gnus-cite-7 (:foreground base03))
         (gnus-cite-8 (:foreground base01))
         (gnus-cite-9 (:foreground base02))
         (gnus-cite-10 (:foreground base03))
         (gnus-cite-11 (:foreground blue))
         (gnus-group-mail-1 (:foreground base03 :weight ,bold))
         (gnus-group-mail-1-empty (:foreground base03))
         (gnus-group-mail-2 (:foreground base02 :weight ,bold))
         (gnus-group-mail-2-empty (:foreground base02))
         (gnus-group-mail-3 (:foreground magenta :weight ,bold))
         (gnus-group-mail-3-empty (:foreground magenta))
         (gnus-group-mail-low (:foreground base0 :weight ,bold))
         (gnus-group-mail-low-empty (:foreground base0))
         (gnus-group-news-1 (:foreground base01 :weight ,bold))
         (gnus-group-news-1-empty (:foreground base01))
         (gnus-group-news-2 (:foreground blue :weight ,bold))
         (gnus-group-news-2-empty (:foreground blue))
         (gnus-group-news-low (:foreground violet :weight ,bold))
         (gnus-group-news-low-empty (:foreground violet))
         (gnus-header-content (:foreground cyan :slant ,italic))
         (gnus-header-from (:foreground base02))
         (gnus-header-name (:foreground blue))
         (gnus-header-newsgroups (:foreground green :slant ,italic))
         (gnus-header-subject (:foreground base01))
         (gnus-server-agent (:foreground base03 :weight ,bold))
         (gnus-server-closed (:foreground base01 :slant ,italic))
         (gnus-server-denied (:foreground base02 :weight ,bold))
         (gnus-server-offline (:foreground green :weight ,bold))
         (gnus-server-opened (:foreground cyan :weight ,bold))
         (gnus-splash (:foreground base02))
         (gnus-summary-high-ancient (:foreground magenta :weight ,bold))
         (gnus-summary-high-read (:foreground base01 :weight ,bold))
         (gnus-summary-high-ticked (:foreground base03 :weight ,bold))
         (gnus-summary-high-undownloaded (:foreground base02 :weight ,bold))
         (gnus-summary-low-ancient (:foreground magenta :slant ,italic))
         (gnus-summary-low-read (:foreground base01 :slant ,italic))
         (gnus-summary-low-ticked (:foreground base03 :slant ,italic))
         (gnus-summary-low-undownloaded (:foreground base02 :slant ,italic))
         (gnus-summary-normal-ancient (:foreground magenta))
         (gnus-summary-normal-read (:foreground base01))
         (gnus-summary-normal-ticked (:foreground base03))
         (gnus-summary-normal-undownloaded (:foreground base02))
         ;; Flymake
         (flymake-errline (:background orange))
         (flymake-warnline (:background violet))
         ;; whitespace
         (whitespace-empty (:foreground red))
         (whitespace-hspace (:foreground orange))
         (whitespace-indentation (:foreground base2))
         (whitespace-space (:foreground base2))
         (whitespace-space-after-tab (:foreground cyan))
         (whitespace-space-before-tab (:foreground red :weight ,bold))
         (whitespace-tab (:foreground base2))
         (whitespace-trailing
          (:background base2 :foreground red :weight ,bold))
         (whitespace-highlight-face (:background blue :foreground red))
         ;; Message
         (message-mml (:foreground blue))
         (message-cited-text (:foreground base02))
         (message-separator (:foreground base03))
         (message-header-xheader (:foreground violet))
         (message-header-name (:foreground cyan))
         (message-header-other (:foreground red))
         (message-header-newsgroups
          (:foreground yellow :weight ,bold :slant ,italic))
         (message-header-subject (:foreground base0))
         (message-header-cc (:foreground green :weight ,bold))
         (message-header-to (:foreground base01 :weight ,bold)))
       mode)))))

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
