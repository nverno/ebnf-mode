# ebnf-mode - Major mode for EBNF files

*Author:* Noah Peart <noah.v.peart@gmail.com><br>
*URL:* [https://github.com/nverno/ebnf-mode](https://github.com/nverno/ebnf-mode)<br>

[![Build Status](https://travis-ci.org/nverno/ebnf-mode.svg?branch=master)](https://travis-ci.org/nverno/ebnf-mode)

### Description

 Basic indentation and font-locking support for EBNF language files.
 http://en.wikipedia.org/wiki/Extended_Backus-Naur_Form

 Refer to ebnf2ps.el for language description/variables to customize
 syntax.

 This mode uses '.' as rule ender and ';' as comment starter.
 (TODO: support different rule endings [.|;] and comment starters?)

 Notes:
   - conversion to png:
     1. Use ebnf2ps.el to convert to .eps (ebnf-eps-buffer)
     2. convert <output>.eps <output>.png (convert from imagemagick)

### Installation

Add containing directory to load-path and generate autoloads, or
```lisp
(require 'ebnf-mode)
```

Code:


---
Converted from `ebnf-mode.el` by [*el2markdown*](https://github.com/Lindydancer/el2markdown).
