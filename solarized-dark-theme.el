(if (not (null (symbol-plist 'custom-theme-load-path)))
    (require 'solarized-definitions
             (locate-file "solarized-definitions.el" custom-theme-load-path
                          '("c" "")))
  (require 'solarized-definitions))

(load-into-load-path)

(create-solarized-theme dark)
