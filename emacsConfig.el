;; My config file!
(defconst win32p    (eq system-type 'windows-nt)  
  "Are we running on a Windows system?")
(defconst linuxp    (or (eq system-type 'gnu/linux)  (eq system-type 'linux))  
  "Are we running on Linux?")
(defconst ergoemacs 0)

(defun comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)
        (next-line)))

(add-to-list 'load-path "~/EmacsConfig/color-theme/themes")
(add-to-list 'load-path "~/EmacsConfig/color-theme/")
(add-to-list 'custom-theme-load-path "~/EmacsConfig/color-theme/themes")

;; ErgoEmacs
(when ergoemacs
	(setenv "ERGOEMACS_KEYBOARD_LAYOUT" "sv") ;
	(load-file "~/EmacsConfig/KeyBindings/ergoemacs_1.9.3.1/site-lisp/site-start.el")
	(xmsi-mode 0)
	(add-to-list 'ac-ignores "//"))

;; Latex
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'reftex-load-hook 'imenu-add-menubar-index)
(add-hook 'reftex-mode-hook 'imenu-add-menubar-index)
(global-set-key [down-mouse-3] 'imenu)

(add-hook 'TeX-mode-hook
          '(lambda ()
            (define-key TeX-mode-map (kbd "<f6>")
              (lambda ()
                (interactive)
                (save-buffer)
                (TeX-command-menu "Rubber")
                (TeX-clean)))
	    (visual-line-mode t)
            (define-key TeX-mode-map (kbd "<f7>")
              (lambda ()
                (interactive)
                (TeX-view)
                [return]))))
;; Google c-standard
(load-file "~/EmacsConfig/google-c-style.el")
(add-hook 'c-mode-common-hook 'google-make-newline-indent)
(add-hook 'c++-mode-hook 'google-make-newline-indent)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c++-mode-hook 'google-set-c-style)

;; Add h to C++ mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; CMake
(setq load-path (cons (expand-file-name "~/EmacsConfig/modes") load-path))
(require 'cmake-mode)
(setq auto-mode-alist
      (append '(("CMakeLists\\.txt\\'" . cmake-mode)
		("\\.cmake\\'" . cmake-mode))
	      auto-mode-alist))

;; GLSL
(autoload 'glsl-mode "glsl-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))

;; Use the GDB visual debugging mode
(setq gdb-many-windows t)

;; Line numbers
(global-linum-mode t)

;; Debug config
(setq c-echo-syntactic-information-p t)

;; Run compile when you press F5
(global-set-key (kbd "C-B") 'compile)

;; Bind alt shift 4 to what it should be!
(global-set-key (kbd "M-¤") 'split-window-right)

(global-set-key (kbd "M-J") 'back-to-indentation)
(global-set-key (kbd "M-L") 'end-of-line)

(global-set-key (kbd "C-/") 'comment-or-uncomment-region-or-line)
(global-set-key (kbd "M-;") 'comment-dwim)

;; Byt till h respektive cpp fil
(add-hook 'c-mode-common-hook
  (lambda() 
    (local-set-key  (kbd "M-o") 'ff-find-other-file)))

;; Move temp files to other dir
(setq backup-directory-alist
`((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
`((".*" ,temporary-file-directory t)))
 
;; Remove startup screen
(setq inhibit-startup-screen t)

(when linuxp
  (set-default-font "Inconsolata-11"))

(when win32p	
  (set-face-attribute 'default nil
                    :family "Consolas" :height 100))

;; Cursor type
(setq-default cursor-type 'bar) 

(add-to-list 'load-path "~/EmacsConfig/color-theme/themes")
(require 'color-theme)
(require 'color-theme-molokai)
(color-theme-molokai)

;; Fix file loader screen
(require 'dired-colors)

;; Set standard indent to 2 rather that 4
(setq standard-indent 4)

;; Scroll one line at a time
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; One line at a time
(setq mouse-wheel-progressive-speed nil) ;; Don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1)  ;; keyboard scroll one line at a time

;; Match paranthesis
(show-paren-mode 1)
(setq show-paren-delay 1)

;; Make available for init-file!
(provide 'Emacs-Config)



