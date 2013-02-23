;;; dired-colors.el --- Adding syntax highlighting to dired buffers

;; Copyright (C) 2006-2011 Davin Pearson

;; Author/Maintainer: Davin Pearson http://www.davinpearson.com
;; Keywords: Dired Colors Font Lock Syntax Highlighting
;; Version: 1.0

;;; Limitation of Warranty

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Install Instructions:

;; See the following URL for the latest info and a tarball:

;; http://davin.50webs.com/research/2010/mopa2e2.html#dired-colors

;; Extract the file in the above-mentioned tarball and put it
;; somewhere in load-path and load it by putting the following
;; command in your .emacs file:
;;
;; (require 'dired-colors)

;; Note the prefix dc- stands for dired-colors and creates a namespace
;; for the code.

;;; Known Bugs:

;; None!

(progn
;;;
;;; Feel free to add to these lists for fontification of your favorite extensions.
;;; 
  (defvar dc-files-list--archives     '("bz2" "gz" "jar" "rpm" "tar" "taz" "tgz" "torrent" "z" "zip"))
  (defvar dc-files-list--graphics     '("bmp" "dvi" "eps" "fig" "gif" "ico" "iff"  "jpe?g" "obj" "pbm" "pcx" "pdf" "pgm"
                                        "png" "pov" "ps" "tga" "xbm" "xcf"))
  (defvar dc-files-list--movies       '("avi" "mov" "mpe?g" "wmv"))
  (defvar dc-files-list--sounds       '("au" "flac" "mid" "mp3" "wav" "wma"))
  (defvar dc-files-list--binaries     '("a" "dat" "la" "lib"))
  (defvar dc-files-list--web          '("css" "xlsx?" "doc" "hlp" "html?" "m4" "odt" "rtf" "tex" "wiki"))
  (defvar dc-files-list--unimportant  '("aux" "bak" "bbl" "blg" "class" "dvi" "elc" "log" "o" "res" "tmp"))
  (defvar dc-files-list--executable   '("bat" "dll" "exe" "lnk" "pif" "reg" ))
  (defvar dc-files-list--sources      '("h" "hh" "c" "cc" "el" "hs" "java" "js" "php" "pov" "tex"))
  (defvar dc-files-list--text         '("manifest" "txt"))
  )

(progn
;;;
;;; Feel free to change the colours of the items
;;; 
  (make-face 'dc-face-dired-executable)
  (set-face-foreground 'dc-face-dired-executable "#0c0")
  (make-face-bold 'dc-face-dired-executable)
  (make-face 'dc-face-dired-archives)
  (set-face-foreground 'dc-face-dired-archives "#f00")
  (make-face-bold 'dc-face-dired-archives)
  (make-face 'dc-face-dired-binaries)
  (set-face-foreground 'dc-face-dired-binaries "#844")
  (make-face-bold 'dc-face-dired-binaries)
  ;;(make-face 'dc-face-dired-fig)
  ;;(set-face-foreground 'dc-face-dired-fig "#0ff")
  ;;(make-face-bold 'dc-face-dired-fig)
  (make-face 'dc-face-dired-graphics)
  (set-face-foreground 'dc-face-dired-graphics "#f0f")
  (make-face-bold 'dc-face-dired-graphics)
  (make-face 'dc-face-dired-movies)
  (set-face-background 'dc-face-dired-movies "#f0f")
  (set-face-foreground 'dc-face-dired-movies "#fff")
  (make-face-bold 'dc-face-dired-movies)
  (make-face 'dc-face-dired-sounds)
  (set-face-foreground 'dc-face-dired-sounds "#830")
  (make-face-bold 'dc-face-dired-sounds)
  (copy-face 'default 'dc-face-dired-sources)
  (make-face-bold 'dc-face-dired-sources)
  (make-face 'dc-face-dired-web)
  (set-face-background 'dc-face-dired-web "#ffff00")
  (set-face-foreground 'dc-face-dired-web "#000")
  (make-face-bold 'dc-face-dired-web)
  (make-face 'dc-face-dired-unimportant)
  (make-face 'dc-face-dired-text)
  (set-face-foreground 'dc-face-dired-unimportant "#aaa")
  )

;;(copy-face 'bg:yellow               'dc-face-directory)
;;(copy-face 'fg:lightgreen           'dc-face-dired-executable)
;;(copy-face 'fg:lightred             'dc-face-dired-archives)
;;(copy-face 'fg:brown                'dc-face-dired-binaries)
;;(copy-face 'fg:lightcyan            'dc-face-dired-fig)
;;(copy-face 'fg:lightmagenta         'dc-face-dired-graphics)
;;(copy-face 'bg:lightmagenta         'dc-face-dired-movies)
;;(copy-face 'fg:brown                'dc-face-dired-sounds)
;;(copy-face 'fg:yellow               'dc-face-dired-web)
;;(copy-face 'fg:darkgray             'dc-face-dired-unimportant)
;;(copy-face 'default                 'dc-face-dired-text)

(copy-face 'dc-face-dired-executable 'eshell-ls-product-face)

(require 'cl)

(setq emacs-dialect--xemacs-p (and (boundp 'xemacsp) (if (string-match "/usr/share/xemacs-" (car (last load-path))) t)))
(setq emacs-dialect--gnuemacs-p (not emacs-dialect--xemacs-p))
(setq emacs-dialect--dosemacs-p (if (string-match "msdos" (emacs-version)) t))
(setq os-type--msdos-p (if emacs-dialect--dosemacs-p t)) 
(setq os-type--linux-p (if (string-match "redhat" emacs-build-system) t))

(defun dc-dired-file-column ()
  0)

;; (d-quote
;;  (+ 49
;;     (max 8 (length user-login-name))
;;     (if os-type--msdos-p 4 0)
;;     )
;;  )

(defun dired-colors--hook ()
  (interactive)

  (setq dc-files-regexp--archives    (dc-dired--make-choice-regexp dc-files-list--archives))
  (setq dc-files-regexp--graphics    (dc-dired--make-choice-regexp dc-files-list--graphics))
  (setq dc-files-regexp--movies      (dc-dired--make-choice-regexp dc-files-list--movies))
  (setq dc-files-regexp--sounds      (dc-dired--make-choice-regexp dc-files-list--sounds))
  (setq dc-files-regexp--binaries    (dc-dired--make-choice-regexp dc-files-list--binaries))
  (setq dc-files-regexp--web         (dc-dired--make-choice-regexp dc-files-list--web))
  (setq dc-files-regexp--unimportant (dc-dired--make-choice-regexp dc-files-list--unimportant))
  (setq dc-files-regexp--executable  (dc-dired--make-choice-regexp dc-files-list--executable))
  (setq dc-files-regexp--sources     (dc-dired--make-choice-regexp dc-files-list--sources))
  (setq dc-files-regexp--text        (dc-dired--make-choice-regexp dc-files-list--text))

  (when emacs-dialect--gnuemacs-p
    ;;(d-foo)
    (setq dired-omit-files (concat "\\("
                                   "^_.*$\\|"
                                   "^\\..*$\\|"
                                   "^.*~$\\|"
                                   "^#.*#\\|"
                                   "^.*\\."
                                   dc-files-regexp--unimportant
                                   "$\\)"))
    )

  (when emacs-dialect--xemacs-p
    (setq dired-omit-extensions (mapcar '(lambda (extension) (concat "." extension)) dc-files-list--unimportant))
    (setq dired-omit-regexps '("^_.*$" "^\\..*$"  "^.*~$" "^#.*#$"))
    (setq dired-re-raw-boring "^$")
    )

  (if os-type--linux-p
      (setq dc-dired--dotstring (make-string 12 ?.))
    (setq dc-dired--dotstring (make-string (dc-dired-file-column) ?.)))

  (assert (eq major-mode 'dired-mode))
  
  (font-lock-add-keywords nil
                          (list
                           ;; ONLY WINDOWS:
                           '("^  \\([a-zA-Z]:.+:\\)" (1 'dired-directory))
                           ;; ONLY LINUX:
                           '("^  \\(/.+:\\)"         (1 'dired-directory))
                           ;; BOTH:
                           ;;'("^  \\(total.*\\)$" (1 'fg:white))

                           (list (concat "^" dc-dired--dotstring "\\(.*\\." dc-files-regexp--text "\\)[\n\r]")
                                 1 ''dc-face-dired-text t)

                           (list (concat "^" dc-dired--dotstring "\\(.*\\." dc-files-regexp--executable "\\)[\n\r]")
                                 1 ''dc-face-dired-executable t)

                           (list (concat "^" dc-dired--dotstring "\\(.*\\." dc-files-regexp--archives "\\)[\n\r]")
                                 1 ''dc-face-dired-archives t)

                           (list (concat "^" dc-dired--dotstring "\\(.*\\." dc-files-regexp--graphics "\\)[\n\r]")
                                 1 ''dc-face-dired-graphics t)

                           (list (concat "^" dc-dired--dotstring "\\(.*\\." dc-files-regexp--movies "\\)[\n\r]")
                                 1 ''dc-face-dired-movies t)

                           (list (concat "^" dc-dired--dotstring "\\(.*\\." dc-files-regexp--sounds "\\)[\n\r]")
                                 1 ''dc-face-dired-sounds t)

                           (list (concat "^" dc-dired--dotstring "\\(.*\\." dc-files-regexp--binaries "\\)[\n\r]")
                                 1 ''dc-face-dired-binaries t)

                           ;;(list (concat "^" dc-dired--dotstring "\\(.*\\." dc-files-regexp--fig "\\)[\n\r]")
                           ;;      1 ''dc-face-dired-fig t)

                           (list (concat "^" dc-dired--dotstring "\\(Makefile\\)[\n\r]")
                                 1 ''dc-face-dired-sources t)

                           (list (concat "^" dc-dired--dotstring "\\(.*\\." dc-files-regexp--sources "\\)[\n\r]")
                                 1 ''dc-face-dired-sources t)

                           (list (concat "^" dc-dired--dotstring "\\(.*\\." dc-files-regexp--web "\\)[\n\r]")
                                 1 ''dc-face-dired-web t)

                           (list (concat "^" dc-dired--dotstring "\\(.*\\." dc-files-regexp--unimportant "\\)$")
                                 1 ''dc-face-dired-unimportant t)

                           (list (concat "^" dc-dired--dotstring "\\(_.*\\|\\..*\\|.*~\\|#.*#\\)$")
                                 1 ''dc-face-dired-unimportant t)
                           )
                          'end
                          )
  
  (when (string-match "sjs-tutorials" default-directory)
    (font-lock-add-keywords nil
                            '(
                              ("[ ][^ ]*\\.java$" 0 'dc-face-dired-unimportant t)
                              ("[ ]Makefile$"     0 'dc-face-dired-unimportant t)
                              ("[ ]sjs2java.el$"  0 'dc-face-dired-unimportant t)
                              ;;("root" 0 'bg:lightgreen t))
                              )
                            'end))
  
  (font-lock-add-keywords nil
                          `(
                            (,(concat "^\\(..d........." dc-dired--dotstring ".*$\\)") 1 'dired-directory t)
                            ("^\\*.*$" 0 'dired-marked  t)
                            ("^D.*$" 0 'dired-flagged t)
                            ;;("root" 0 'bg:yellow t)
                            )
                          'end)

  ;;(font-lock-mode nil)
  (font-lock-mode 1)
  
  )

(add-hook 'dired-mode-hook 'dired-colors--hook 'APPEND)
;;(add-hook 'dired-after-readin-hook  'dired-colors--hook)
;;(add-hook 'dired-before-readin-hook 'dired-colors--hook)

;; dired-mode-hook
;;(remove-hook 'dired-mode-hook 'd-gfxhook)
;;(remove-hook 'dired-mode-hook 'dired-colors--hook)
;;(add-hook 'dired-mode-hook 'dc-gfx-hook 'APPEND)
  
;; (dc-dired--make-choice-regexp (setq suffix-list '("zip" "jar" "tar" "rpm" "bz2" "gz" "z" "tgz" "taz")))
(defun dc-dired--make-choice-regexp (suffix-list)
  (let ((answer nil)
        (n 0))
    (setq answer "\\(")
    (while suffix-list
        (setq answer (concat answer (if (eq n 0) "" "\\|") (car suffix-list)))
        (incf n)
        (setq suffix-list (cdr suffix-list)))
    (setq answer (concat answer "\\)"))))

(provide 'dired-colors)