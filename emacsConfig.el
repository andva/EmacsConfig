;; My config file!

(setq load-path (cons (expand-file-name "~/EmacsConfig/modes") load-path))
;; Line numbers
(global-linum-mode t)
;; Temp, startup
(setq backup-directory-alist
	  `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
	  `((".*" ,temporary-file-directory t)))

;;;; Comments function
(defun comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)
        (next-line)))

;;;; Bindings
;; Compile
(global-set-key (kbd "C-`") 'compile)
;; Toggle comment on region
(global-set-key (kbd "C-/") 'comment-or-uncomment-region-or-line)
;; Add comment at end of line
(global-set-key (kbd "M-;") 'comment-dwim)
;; Add new line and indent to right level
(define-key global-map (kbd "RET") 'newline-and-indent)
;; Remove with backspace
(setq c-backspace-function 'backward-delete-char)

;;;; Company mode
(add-to-list 'load-path "~/EmacsConfig/modes/company-mode")
(autoload 'company-mode "company" nil t)

(defun complete-or-indent ()
  (interactive)
  (if (company-manual-begin)
	  (company-complete-common)
	(indent-according-to-mode)))

(add-hook 'company-mode
          (lambda()
            (global-set-key (kbd "TAB") 'complete-or-indent)
			(global-set-key "\t" 'complete-or-indent)))

(setq company-backends '(company-elisp 
                         company-ropemacs
                         company-gtags
                         company-dabbrev-code
                         company-keywords
                         company-files 
                         company-dabbrev
						 company-clang))
(setq company-idle-delay t)

(company-mode 1)

;;;; Helm mode
;; Responsible for autocomplete in M-X, helm-imenu is nice as wel.
(add-to-list 'load-path "~/EmacsConfig/modes/helm")
(require 'helm-config)
(helm-mode t)

;;;; Cpp and C
;; Add h files to cpp mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
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

;;;; Python
(add-hook 'python-mode
		  (setq compile-command "python "))

;;;; PRMan
(autoload 'rib-mode "rib-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rib\\'" . rib-mode))
(autoload 'rsl-mode "rsl-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.sl\\'" . rsl-mode))

;;;; TeX
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

;;;; GLSL
(autoload 'glsl-mode "glsl-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))

;;;; CMake
(setq load-path (cons (expand-file-name "~/EmacsConfig/modes") load-path))
(require 'cmake-mode)
(setq auto-mode-alist
      (append '(("CMakeLists\\.txt\\'" . cmake-mode)
				("\\.cmake\\'" . cmake-mode))
			  auto-mode-alist))

;;;; elisp
(defun imenu-elisp-sections ()
  (setq imenu-prev-index-position-function nil)
  (add-to-list 'imenu-generic-expression '("Sections" "^;;;; \\(.+\\)$" 1) t))
 
(add-hook 'emacs-lisp-mode-hook 'imenu-elisp-sections)

;;;; Tab default
(setq-default tab-width 4)
(setq standard-indent 4)


;;;; Debugging
;; Use the GDB visual debugging mode
(setq gdb-many-windows t)
;; Debug emacs config
;; (setq c-echo-syntactic-information-p t)

;;;; Scrolling
;; Scroll one line at a time
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; One line at a time
(setq mouse-wheel-progressive-speed nil) ;; Don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1)  ;; keyboard scroll one line at a time

;;;; Font
;; Specify the default font as =Source Code Pro=, which should already
;;    be [[http://blogs.adobe.com/typblography/2012/09/source-code-pro.html][downloaded]] and installed.
(set-frame-font "Source Code Pro")
(set-face-attribute 'default nil :font "Source Code Pro" :height 80)
(set-face-font 'default "Source Code Pro")


;;;; Color theme
(add-to-list 'load-path "~/EmacsConfig/color-theme/")
(add-to-list 'load-path "~/EmacsConfig/color-theme/themes")
(require 'color-theme)
(require 'color-theme-molokai)
(color-theme-molokai)

;; Remove startup screen
(setq inhibit-startup-screen t)

;; Match paranthesis
(show-paren-mode 1)
(setq show-paren-delay 1)

