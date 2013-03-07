(require 'solarized-definitions
         (locate-file "solarized-definitions.el" custom-theme-load-path
                      '("c" "")))

(defmacro create-solarized-theme ()
  (let ((theme-name 'solarized))
    `(progn
       (deftheme ,theme-name ,solarized-description)
       (apply 'custom-theme-set-faces
              ',theme-name ',(solarized-color-definitions))
       (provide-theme ',theme-name))))

(create-solarized-theme)
