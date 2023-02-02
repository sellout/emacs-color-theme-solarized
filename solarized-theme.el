;; -*- lexical-binding: t; -*-

(require 'solarized-definitions
         (locate-file "solarized-definitions.el" custom-theme-load-path
                      '("c" "")))

(create-solarized-theme 'solarized
                        solarized-description
                        (solarized-color-definitions))

(cl-defun solarized-update-background-mode
    (appearance &optional (frames (frame-list)))
  "Set the APPEARANCE of all frames to either 'light or 'dark.
This is not specific to Solarized – it will update the appearance of any theme
that observes the background characteristic."
  (setq frame-background-mode appearance)
  (mapc #'frame-set-background-mode frames)
  ;; Supposedly #'frame-set-background-mode updates the faces, but it doesn’t
  ;; seem to actually., so re-enable all the themes.
  (mapc #'enable-theme (reverse custom-enabled-themes))
  ;; For some reason, ‘enable-theme’ (or maybe ‘solarized’?) is resetting the
  ;; ‘frame-background-mode’, so reset it here.
  (setq frame-background-mode appearance))

(defun solarized-toggle-background-mode ()
  "Toggle between light and dark background modes.
This is not specific to Solarized – it will update the appearance of any theme
that observes the background characteristic."
  (interactive)
  (let ((new-mode (pcase frame-background-mode
                    ('dark 'light)
                    (_ 'dark))))
    (solarized-update-background-mode new-mode)))
