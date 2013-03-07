;;; Author: Ethan Schoonover, Solarized; Greg Pfeil, Emacs adaptation
;;; URL: http://ethanschoonover.com/solarized

;;; This file is not (YET) part of GNU Emacs.

;;; # Usage

;;; 1. Install the color-theme package
;;;   (http://www.emacswiki.org/cgi-bin/wiki/ColorTheme)
;;; 2. Load this file
;;; 3. M-x color-theme-solarized-[dark|light]

(let ((current-file-name (or load-file-name buffer-file-name)))
  (if current-file-name
    (let* ((reqname (concat (file-name-directory current-file-name)
                            "solarized-definitions.el"))
           (compreqname (concat reqname "c")))
      (require 'solarized-definitions
               (if (file-exists-p compreqname) compreqname reqname)))
    (require 'solarized-definitions)))

(eval-when-compile
  (require 'color-theme))

;;;###autoload
(defun color-theme-solarized ()
  "Color theme by Ethan Schoonover, created 2011-03-24.
Ported to Emacs by Greg Pfeil, http://ethanschoonover.com/solarized."
  (interactive)
  (color-theme-install
   `(color-theme-solarized () () ,@(solarized-color-definitions))))

(add-to-list 'color-themes
             `(color-theme-solarized
               "Solarized"
               ,solarized-description))

(provide 'color-theme-solarized)
