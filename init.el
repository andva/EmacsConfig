;; Configure package manager
(require 'package)

;; Add Marmalade repo
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

;; ... and melpa. Melpa packages that exist on marmalade will have
;; precendence.
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

;; And load things!
(package-refresh-contents)
(package-initialize)

(defvar my-pkgs
  '(; Basic functionality
    ace-jump-mode
    ack-and-a-half
    browse-kill-ring
    confluence
    dash
    flx-ido
    flycheck
    idle-highlight-mode
    ido-ubiquitous
    iy-go-to-char
    magit
    multiple-cursors
   
    paredit
    password-store
    projectile
    
    rainbow-delimiters
    rainbow-mode
    nyan-mode
    
    s
    smex
    smart-mode-line
    switch-window
    undo-tree

    ; Programming
    rust-mode
    go-mode
    haskell-mode

    ; Clojure
    ac-cider-compliment
    cider
    clojure-mode

    ; C++
    cpputils-cmake
    cc-mode

    ; Graphics
    glsl-mode
    hlsl-mode
    rib-mode

    ; Version control
    psvn
)
  "A list of packages to install at launch.")

(dolist (p my-pkgs)
  (when (not (package-installed-p p))
    (package-install p)))

;; Are we on a mac?
(setq is-mac (equal system-type 'darwin))

;; Or on Linux?
(setq is-linux (equal system-type 'gnu/linux))

(setq is-dos (equal system-type 'ms-dos))

(setq is-windows (or (eq system-type 'windows-nt) (equal system-type 'ms-dos))) 

;; What's the home folder?
(defvar home-dir)
(setq home-dir (expand-file-name "~"))

(add-to-list 'load-path (concat user-emacs-directory "init"))

(mapc 'require '(functions
                 settings
                 modes
                 hooks
                 bindings
                 eshell-setup))

(add-to-list 'load-path (concat user-emacs-directory "scripts"))

(setq custom-file (concat user-emacs-directory "init/custom.el"))
(load custom-file)

(custom-download-script
 "https://gist.github.com/gongo/1789605/raw/526e3f21dc7d6cef20951cf0ce5d51b90b7821ff/json-reformat.el"
 "json-reformat.el")

(custom-download-script
 "http://accad.osu.edu/~smay/RManNotes/rsl-mode.el"
 "rsl-mode.el")

;; A file with machine specific settings.
(load-file-if-exists "~/.emacs.d/init-local.el")

;; IRC configuration
;; Actual servers and such are loaded from irc.el
(load-file-if-exists "~/.emacs.d/init-irc.el")

;; HLSL mode
(load-file-if-exists "~/.emacs.d/modes/hlsl-mode.el")
;; 

;; Load magnars' string manipulation library
(require 's)

(require 'ack-and-a-half)

;; Seed RNG
(random t)

;; SML should respect theme colours
; (setq sml/theme 'black)
(sml/setup)
