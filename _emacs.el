;; My config file!

(add-to-list 'load-path "~/EmacsThemes/")
(add-to-list 'load-path "~/color-theme/")

;; ErgoEmacs
(setenv "ERGOEMACS_KEYBOARD_LAYOUT" "sv") ;
(load-file "~/KeyBindings/ergoemacs_1.9.3.1/site-lisp/site-start.el")
(xmsi-mode 0)

;; C++ mode
(add-hook 'c++-mode-hook
  '(lambda ()
     (c-set-style "stroustrup")
     (setq indent-tabs-mode nil)))

;; Add h to C++ mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; Use the GDB visual debugging mode
(setq gdb-many-windows t)

;; Turn Semantic on
(semantic-mode 1)

;; Try to make completions when not typing
;; (global-semantic-idle-completions-mode 1)

;; Use the Semantic speedbar additions
(add-hook 'speedbar-load-hook (lambda () (require 'semantic/sb)))

;; Treat .h files as C++ files (instead of C)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; Return adds a newline and indents
(define-key c-mode-base-map (kbd "RET") 'newline-and-indent)

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
(set-face-attribute 'default nil
                    :family "Consolas" :height 100)

;; Column number position
(setq column-number-mode t)
(setq global-linum-mode)

;; Cursor type
(setq-default cursor-type 'bar) 

;; Add color scheme
(require 'color-theme)
(require 'color-theme-sublime)
;; (setq color-theme-is-global t)
(color-theme-sublime)

;; Set standard indent to 2 rather that 4
(setq standard-indent 2)

;; Scroll one line at a time
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; One line at a time
(setq mouse-wheel-progressive-speed nil) ;; Don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1)  ;; keyboard scroll one line at a time







