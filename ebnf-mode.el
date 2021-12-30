;;; ebnf-mode.el --- Major mode for EBNF files -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/ebnf-mode
;; Package-Requires: 
;; Created: 29 December 2021

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;; [![Build Status](https://travis-ci.org/nverno/ebnf-mode.svg?branch=master)](https://travis-ci.org/nverno/ebnf-mode)
;;
;;; Description:
;;
;;  Basic support for EBNF language files.
;;  http://en.wikipedia.org/wiki/Extended_Backus-Naur_Form
;;
;;  Refer to ebnf2ps.el for language description/variables to customize
;;  syntax.
;;
;;  This mode uses '.' as rule ender and ';' as comment starter.
;;  (TODO: support different rule endings [.|;] and comment starters?)
;;
;;  Notes:
;;    - conversion to png:
;;      1. Use ebnf2ps.el to convert to .eps (ebnf-eps-buffer)
;;      2. convert <output>.eps <output>.png (convert from imagemagick)
;;
;;; Installation:
;;
;; ```lisp
;; ```
;;
;;; Code:

(defgroup ebnf-mode nil
  "Major mode for EBNF files."
  :group 'languages)

(defvar ebnf-mode-font-lock-keywords
  `(("^\\s-*\\([[:alnum:]_]+\\)\\s-*=" (1 font-lock-variable-name-face))
    ("\\([.]\\)[ \t]*" (1 font-lock-warning-face))))

(defvar ebnf-mode-syntax-table
  (let ((syn (make-syntax-table)))
    (modify-syntax-entry ?\; "<" syn)
    (modify-syntax-entry ?\n ">" syn)
    syn))

;;;###autoload
(define-derived-mode ebnf-mode prog-mode "EBNF"
  "Major mode for editing EBNF files.\n
\\{ebnf-mode-map"
  (setf font-lock-defaults '(ebnf-mode-font-lock-keywords nil nil nil nil))
  (setq-local comment-start ";")
  (setq-local comment-end ""))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ebnf\\'" . ebnf-mode))

(provide 'ebnf-mode)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; ebnf-mode.el ends here
