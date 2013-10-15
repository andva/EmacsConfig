;; My config file!
(defconst win32p    (eq system-type 'windows-nt)  
  "Are we running on a Windows system?")
(defconst linuxp    (or (eq system-type 'gnu/linux)  (eq system-type 'linux))  
  "Are we running on Linux?")

(defun comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)
        (next-line)))

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

(add-hook 'python-mode
	  (setq compile-command "python "))

(defun my-c-mode-common-hook ()
  ;; my customizations for all of c-mode, c++-mode, objc-mode, java-mode
  (c-set-offset 'substatement-open 0)
  ;; other customizations can go here
  (hs-minor-mode t)
  (setq c++-tab-always-indent t)
  (setq c-basic-offset 4)                  ;; Default is 2
  (setq c-indent-level 4)                  ;; Default is 2
  (setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60))
  (setq tab-width 4)
  (setq indent-tabs-mode t)  ; use spaces only if nil
  )
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
(setq-default tab-width 4)
(setq standard-indent 4)
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
(global-set-key (kbd "C-/") 'comment-or-uncomment-region-or-line)
(global-set-key (kbd "M-;") 'comment-dwim)

(setq c-backspace-function 'backward-delete-char)
;; ;; Byt till h respektive cpp fil
;; (add-hook 'c-mode-common-hook
;;   (lambda() 
;;     (local-set-key  (kbd "M-o") 'ff-find-other-file)))

;; Move temp files to other dir
(setq backup-directory-alist
`((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
`((".*" ,temporary-file-directory t)))
 
;; Remove startup screen
(setq inhibit-startup-screen t)

;; Specify the default font as =Source Code Pro=, which should already
;;    be [[http://blogs.adobe.com/typblography/2012/09/source-code-pro.html][downloaded]] and installed.

(set-frame-font "Source Code Pro")
(set-face-attribute 'default nil :font "Source Code Pro" :height 80)
(set-face-font 'default "Source Code Pro")

(add-to-list 'load-path "~/EmacsConfig/color-theme/")
(add-to-list 'load-path "~/EmacsConfig/color-theme/themes")
(require 'color-theme)
(require 'color-theme-molokai)
(color-theme-molokai)

;; Scroll one line at a time
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; One line at a time
(setq mouse-wheel-progressive-speed nil) ;; Don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1)  ;; keyboard scroll one line at a time

;; Match paranthesis
(show-paren-mode 1)
(setq show-paren-delay 1)

