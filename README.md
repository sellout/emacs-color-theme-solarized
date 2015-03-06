Solarized Colorscheme for Emacs
===============================

Stolen from Ethan Schoonover <es@ethanschoonover.com> by Greg Pfeil <greg@technomadic.org>

Visit the [Solarized homepage]
------------------------------

See the [Solarized homepage] for screenshots, 
details and colorscheme versions for Vim, Mutt, popular terminal emulators and 
other applications.

Screenshots
-----------

![solarized dark](https://github.com/altercation/solarized/raw/master/img/solarized-vim.png)

Downloads
---------

If you have come across this colorscheme via the [Emacs-only repository] on 
github, see the link above to the Solarized homepage or visit the main [Solarized repository].

The [Emacs-only repository] is kept in sync with the main [Solarized repository]. Issues, bug reports, changelogs that are not specific to the Emacs implementation should be submitted to the main [Solarized repository].

[Solarized homepage]:    http://ethanschoonover.com/solarized
[Solarized repository]:  https://github.com/altercation/solarized
[Emacs-only repository]:  https://github.com/sellout/emacs-color-theme-solarized
[color-theme]: http://www.nongnu.org/color-theme

Installation & Usage
--------------------

### Emacs 24

1. Add the `emacs-color-theme-solarized` directory to your Emacs `custom-theme-load-path`.
2. Add `(load-theme 'solarized t)` to your Emacs init file.
3. Reload the init file, or restart Emacs.

### [color-theme] \(pre-Emacs 24\)

1. Download and install [color-theme].
2. Add the `emacs-color-theme-solarized` directory to your Emacs `load-path`.
3. Add `(require 'color-theme-solarized)` and `(color-theme-solarized)` to your Emacs init file (usually `~/.emacs`).
3. Reload the init file, or restart Emacs.

### all versions

To switch between the light and dark variations of Solarized, set the frame’s `background-mode`. This can be accomplished globally using `M-x customize-variable frame-background-mode` or on a per-frame basis with `(set-frame-parameter nil 'background-mode 'light)` (or `'dark`).  If you're in a terminal, you must also set the terminal parameter with `(set-terminal-parameter nil 'background-mode 'light)` (or `'dark`). Remember to call `enable-theme` after changing the background mode to update the state of the theme.

This allows you to have a mix of light and dark frames. I tend to use light frames in the GUI and dark frames in my terminal, so I use the following code:

```common-lisp
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (set-frame-parameter frame
                                 'background-mode
                                 (if (display-graphic-p frame) 'light 'dark))
            (enable-theme 'solarized)))
```

### IMPORTANT NOTE FOR TERMINAL USERS:

If you are going to use Solarized in Terminal mode (i.e. not in a GUI version
like Cocoa or X11 Emacs), **please please please** consider setting your
terminal emulator's colorscheme to use the Solarized palette. The [Solarized
repository] includes palettes for some popular terminal emulator as well as
Xdefaults; or you can download them from the official [Solarized homepage].
If you use this emacs color theme *without* having changed your emulator's
palette, you will need to configure Solarized to degrade its colorscheme to
a set compatible with the terminal's default limited 256 color palette
(whereas by using the terminal's 16 ANSI color values, you would
see the correct, specific values for the Solarized palette).

Again, I recommend just changing your terminal colors to Solarized values 
either manually or via one of the many terminal schemes available for import.

Advanced Configuration
----------------------

Solarized will work out of the box with just the instructions specified above
but does include several variables that can be customized.

    variable name            default   optional
    --------------------------------------------
    solarized-termcolors =   16    |   256
    solarized-degrade    =   nil   |   t
    solarized-bold       =   t     |   nil
    solarized-underline  =   t     |   nil
    solarized-italic     =   t     |   nil
    solarized-contrast   =   normal|   high, low
    solarized-visibility =   normal|   high, low
    solarized-broken-srgb=   nil   |   t (see details for Mac behavior)
    --------------------------------------------

### Option Details

*   solarized-termcolors

    Some 256-color terminals also allow you to set and use the standard 16
    colors in addition to the fixed 256-color palette. This option only
    applies when your terminal is in 256-color mode. If set to 16 (the
    default) it will try to use the exact Solarized colors (assuming that
    you've set these colors to the correct Solarized values either manually or
    by importing one of the many colorscheme available for popular
    terminal emulators). If it’s set to 256, then Solarized will use a
    degraded version of the Solarized palette by displaying the closest colors
    in the terminal's default 256 colors as shown in [Xterm's color
    chart](http://en.wikipedia.org/wiki/File:Xterm_color_chart.png).

*   solarized-degrade

    For test purposes only; in GUI mode, this forces Solarized to use the 256
    degraded color mode to test the approximate color values for accuracy.

*   solarized-bold | solarized-underline | solarized-italic

    If you wish to stop Solarized from displaying bold, underlined or 
    italicized typefaces, simply set the appropriate variable to `nil`.

*   solarized-contrast

    Stick with normal! It's been carefully tested. Setting this option to high 
    or low does use the same Solarized palette but simply shifts some values
    up or down in order to expand or compress the tonal range displayed.

*   solarized-visibility

    Special characters such as trailing whitespace, tabs, newlines, when
    displayed using `:set list` can be set to one of three levels depending on 
    your needs. Default value is `normal` with `high` and `low` options.
    
*   solarized-broken-srgb

    Emacs [bug #8402](http://debbugs.gnu.org/cgi/bugreport.cgi?bug=8402)
    results in incorrect color handling on Macs. If you are using Emacs on a
    Mac, we try to determine this value automatically. If this is `t` (the
    default on Macs), Solarized works around it with alternative colors.
    However, these colors are not totally portable, so you may be able to edit
    the "Gen RGB" column in `solarized-definitions.el` to improve them further.

Code Notes
----------

I have attempted to modularize the creation of Emacs colorschemes in this script and, while it could be refactored further, it should be a good foundation for the creation of any color scheme. By simply changing the values in the `solarized-colors` table in `solarized-definitions.el` and testing in a GUI Emacs, you can rapidly prototype new colorschemes without diving into the weeds of line-item editing each syntax highlight declaration.

The Values
----------

L\*a\*b values are canonical (White D65, Reference D50), other values are 
matched in sRGB space.

    SOLARIZED HEX     16/8 TERMCOL  XTERM/HEX   L*A*B      RGB         HSB
    --------- ------- ---- -------  ----------- ---------- ----------- -----------
    base03    #002b36  8/4 brblack  234 #1c1c1c 15 -12 -12   0  43  54 193 100  21
    base02    #073642  0/4 black    235 #262626 20 -12 -12   7  54  66 192  90  26
    base01    #586e75 10/7 brgreen  240 #585858 45 -07 -07  88 110 117 194  25  46
    base00    #657b83 11/7 bryellow 241 #626262 50 -07 -07 101 123 131 195  23  51
    base0     #839496 12/6 brblue   244 #808080 60 -06 -03 131 148 150 186  13  59
    base1     #93a1a1 14/4 brcyan   245 #8a8a8a 65 -05 -02 147 161 161 180   9  63
    base2     #eee8d5  7/7 white    254 #e4e4e4 92 -00  10 238 232 213  44  11  93
    base3     #fdf6e3 15/7 brwhite  230 #ffffd7 97  00  10 253 246 227  44  10  99
    yellow    #b58900  3/3 yellow   136 #af8700 60  10  65 181 137   0  45 100  71
    orange    #cb4b16  9/3 brred    166 #d75f00 50  50  55 203  75  22  18  89  80
    red       #dc322f  1/1 red      160 #d70000 50  65  45 220  50  47   1  79  86
    magenta   #d33682  5/5 magenta  125 #af005f 50  65 -05 211  54 130 331  74  83
    violet    #6c71c4 13/5 brmagenta 61 #5f5faf 50  15 -45 108 113 196 237  45  77
    blue      #268bd2  4/4 blue      33 #0087ff 55 -10 -45  38 139 210 205  82  82
    cyan      #2aa198  6/6 cyan      37 #00afaf 60 -35 -05  42 161 152 175  74  63
    green     #859900  2/2 green     64 #5f8700 60 -20  65 133 153   0  68 100  60
    
### Bug Reporting

Here are some things to keep in mind when submitting a bug report:

*   include the output of `M-x version` in your report,
*   mention whether you’re using color-theme or the Emacs 24 theme,
*   include the names of Emacs faces that you have a problem with (`M-: (face-at-point)` and `M-x describe-face` will tell you the name of the face at point),
*   include the output of `M-: (display-color-cells)` (that lets us know which set of colors your Emacs is using),
*   screenshots are very helpful (before and after if you made a change),
*   if you’re using a terminal, the name of the terminal and (if you can find out) the number of colors the terminal app is using,
*   also if you’re using a terminal, try running Emacs in GUI mode, and see if the problem exists there (if it does, report the bug that way, if not, just mention that it’s a terminal-only bug),
*   it’s very helpful (but not expected) if you can compare it to a similar situation in VIM (especially if you know the VIM highlight name or have a screenshot), and
*  `M-x customize-apropos-faces` can help you find all the relevant faces if you are submitting faces for a mode.
