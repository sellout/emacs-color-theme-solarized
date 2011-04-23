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

1. Add the `emacs-color-theme-solarized` directory to your Emacs `load-path`.
2. `M-x enable-theme`, then either `solarized-light` or `solarized-dark`.

### [color-theme] (pre-Emacs 24)

1. Download and install [color-theme].
2. Add the `emacs-color-theme-solarized` directory to your Emacs `load-path`.
3. Add `(require 'color-theme-solarized)` to your Emacs init file (usually `~/.emacs`).
4. Use the usual [color-theme] mechanism to select one of the Solarized themes, or `M-x color-theme-solarized-[light|dark]`.

### IMPORTANT NOTE FOR TERMINAL USERS:

If you are going to use Solarized in Terminal mode (i.e. not in a GUI version 
like Cocoa or X11 Emacs), **please please please** consider setting your terminal emulator's colorscheme to used the Solarized palette. I've included palettes for some popular terminal emulator as well as Xdefaults in the official Solarized download available from [Solarized homepage]. If you use 
Solarized *without* these colors, Solarized will need to be told to degrade its colorscheme to a set compatible with the limited 256 terminal palette (whereas by using the terminal's 16 ansi color values, you can set the correct, specific values for the Solarized palette).

If you do use the custom terminal colors, Solarized should work out of the
box for you. If you are using a terminal emulator that supports 256 colors and 
don't want to use the custom Solarized terminal colors, you will need to use 
the degraded 256 colorscheme. To do so, simply customize the `solarized-degrade` variable to `t`.

Again, I recommend just changing your terminal colors to Solarized values 
either manually or via one of the many terminal schemes available for import.

Advanced Configuration
----------------------

Solarized will work out of the box with just the instructions specified above but does include several other variables that can be customized.

    variable name           default   optional
    -------------------------------------------
    solarized-degrade   =   nil   |   t
    solarized-bold      =   t     |   nil
    solarized-underline =   t     |   nil
    solarized-italic    =   t     |   nil
    solarized-contrast  =   normal|   high, low
    solarized-visibility=   normal|   high, low
    -------------------------------------------

### Option Details

*   solarized-degrade

    For test purposes only; forces Solarized to use the 256 degraded color
    mode to test the approximate color values for accuracy.

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

Code Notes
----------

I have attempted to modularize the creation of Emacs colorschemes in this script and, while it could be refactored further, it should be a good foundation for the creation of any color scheme. By simply changing the  values in the `solarized-colors` table in `solarized-definitions.el` and testing in a GUI Emacs, you can rapidly prototype new colorschemes without diving into the weeds of line-item editing each syntax highlight declaration.

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
    
