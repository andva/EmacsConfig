;; My config file!
(defconst win32p    (eq system-type 'windows-nt)  "Are we running on a Windows system?")
(defconst linuxp    (or (eq system-type 'gnu/linux)  (eq system-type 'linux))  "Are we running on Linux?")
(defconst ergoemacs 0)

(add-to-list 'load-path "~/EmacsConfig/color-theme/themes")
(add-to-list 'load-path "~/EmacsConfig/color-theme/")

;; ErgoEmacs
(when ergoemacs
	(setenv "ERGOEMACS_KEYBOARD_LAYOUT" "sv") ;
	(load-file "~/EmacsConfig/KeyBindings/ergoemacs_1.9.3.1/site-lisp/site-start.el")
	(xmsi-mode 0))

;; Google c-standard

;; C++ mode
;; (add-hook 'c++-mode-hook
;;  '(lambda ()
;;     (c-set-style "stroustrup")
;;     (setq indent-tabs-mode nil)))

;; Add h to C++ mode
;; (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(load-file "~/EmacsConfig/google-c-style.el")
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

;; Use the GDB visual debugging mode
(setq gdb-many-windows t)

;; Turn Semantic on
(semantic-mode 1)

;; Run compile when you press F5
(global-set-key (kbd "<f5>") 'compile)

;; Move temp files to other dir
(setq backup-directory-alist
`((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
`((".*" ,temporary-file-directory t)))

;; Remove startup screen
(setq inhibit-startup-screen t)

;; Use 10-pt Consolas as default font
;; (set-face-attribute 'default nil
;;                    :family "Consolas" :height 100)

;;font setups
;;(defvar vsc-little-font "" "*My lovely little font")

;; (when linuxp
;;  (setq vsc-little-font "ProggyTinyTT-8"))

(when win32p
	(set-face-attribute 'default nil
                    :family "Consolas" :height 100))

;; (add-to-list 'default-frame-alist (cons 'font vsc-little-font))
;; (add-to-list 'initial-frame-alist (cons 'font vsc-little-font))

;; Column number position
(setq column-number-mode t)
(setq global-linum-mode)

;; Cursor type
(setq-default cursor-type 'bar) 

;; Add color scheme
(require 'color-theme)
(require 'color-theme-sublime)
(setq color-theme-is-global t)
(color-theme-sublime)

;; Set standard indent to 2 rather that 4
(setq standard-indent 2)

;; Scroll one line at a time
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; One line at a time
(setq mouse-wheel-progressive-speed nil) ;; Don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1)  ;; keyboard scroll one line at a time

;; Match paranthesis
(show-paren-mode 1)
(setq show-paren-delay 0)
;;(set-face-foreground 'show-paren-match-face "#cfbfff")

;; (set-face-attribute 'show-paren-match-face nil :weight 'bold)   



;; Make available for init-file!
(provide 'Emacs-Config)



