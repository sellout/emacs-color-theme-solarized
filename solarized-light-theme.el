(require 'solarized-definitions
         (locate-file "solarized-definitions.el" custom-theme-load-path
                      '("c" "")))

(create-solarized-theme solarized-light
                        (concat "This theme is obsolete since 2013-05-01;
use `solarized' instead.

"
                                solarized-description)
                        (solarized-color-definitions))
