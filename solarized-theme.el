;; solarized-theme.el --- Solarized custom theme  -*- lexical-binding: t; -*-

(require 'solarized-definitions
         (locate-file "solarized-definitions.el" custom-theme-load-path
                      '("c" "")))

(deftheme solarized solarized-description)

(apply #'custom-theme-set-faces 'solarized (solarized-color-definitions))

(custom-theme-set-variables
 'solarized
 ;; This is obsolete, but something might still be referencing it.
 `(ansi-color-names-vector
   ,(apply #'vector
           (mapcar (lambda (color-name)
                     (nth 1 (assoc color-name solarized-colors)))
                   '(base02 red green yellow blue magenta cyan base2)))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'solarized)
;;; solarized-theme.el ends here
