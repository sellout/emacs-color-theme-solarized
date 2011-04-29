;;; Author: Ethan Schoonover, Solarized; Greg Pfeil, Emacs adaptation
;;; URL: http://ethanschoonover.com/solarized

;;; This file is not (YET) part of GNU Emacs.

;;; # Usage

;;; 1. Install the color-theme package
;;;   (http://www.emacswiki.org/cgi-bin/wiki/ColorTheme)
;;; 2. Load this file
;;; 3. M-x color-theme-solarized-[dark|light]

(require 'solarized-definitions)

(eval-when-compile
  (require 'color-theme))

;;;###autoload
(defun color-theme-solarized (mode)
  "Color theme by Ethan Schoonover, created 2011-03-24.
Ported to Emacs by Greg Pfeil, http://ethanschoonover.com/solarized."
  (interactive "Slight or dark? ")
  (color-theme-install
   (let* ((definitions (solarized-color-definitions mode))
          (faces (first definitions))
          (variables (second definitions)))
       (solarized-color-definitions mode)
     `(,(intern (concat "color-theme-solarized-" (symbol-name mode)))
       ,variables
       ,@faces))))

;;;###autoload
(defun color-theme-solarized-dark ()
  (interactive)
  (color-theme-solarized 'dark))

;;;###autoload
(defun color-theme-solarized-light ()
  (interactive)
  (color-theme-solarized 'light))

(add-to-list 'color-themes
             `(color-theme-solarized-light
               "Solarized Light"
               ,solarized-description))
(add-to-list 'color-themes
             `(color-theme-solarized-dark
               "Solarized Dark"
               ,solarized-description))

(provide 'color-theme-solarized)
