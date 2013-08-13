;; My config file!
(defconst win32p    (eq system-type 'windows-nt)  
  "Are we running on a Windows system?")
(defconst linuxp    (or (eq system-type 'gnu/linux)  (eq system-type 'linux))  
  "Are we running on Linux?")

;; ErgoEmacs
(setenv "ERGOEMACS_KEYBOARD_LAYOUT" "us") ;

(load-file "~/EmacsConfig/KeyBindings/ergoemacs_1.9.3.1/site-lisp/site-start.el")

;; (setq c-default-style "bsd")
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
;; (defun my-c-mode-hook ()
;; ;;   (indent-tabs-mode)
;;   (tab-width 4))
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

(setq load-path (cons (expand-file-name "~/EmacsConfig/modes") load-path))

(autoload 'cuda-mode "cuda-mode.el")
(add-to-list 'auto-mode-alist '("\\.cu\\'" . cuda-mode))
(add-to-list 'auto-mode-alist '("\\.ggl\\'" . lua-mode))
(require 'cmake-mode)
(setq auto-mode-alist
      (append '(("CMakeLists\\.txt\\'" . cmake-mode)
("\\.cmake\\'" . cmake-mode))
auto-mode-alist))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
(setq-default tab-width 4)
(setq standard-indent 4)

(setq c-backspace-function 'backward-delete-char)

(auto-complete-mode 0)

(add-to-list 'load-path "~/EmacsConfig/complete")
(autoload 'company-mode "company" nil t)
(setq company-backends '(company-elisp 
                         company-ropemacs
                         company-gtags
                         company-dabbrev-code
                         company-keywords
                         company-files 
                         company-dabbrev))
(defun after-init ()
  (auto-complete-mode 0)
  (company-mode))

(add-hook 'after-init-hook 'after-init)

;; Use the GDB visual debugging mode
(setq gdb-many-windows 1)
;;c++-mode



;; Line numbers
(global-linum-mode t)

;; Debug config
;; (setq c-echo-syntactic-information-p t)

(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
      (if (region-active-p)
		  (setq beg (region-beginning) end (region-end))
		(setq beg (line-beginning-position) end (line-end-position)))
      (comment-or-uncomment-region beg end)
      (next-line)))

(defun complete-or-indent ()
    (interactive)
    (if (company-manual-begin)
        (company-complete-common)
      (indent-according-to-mode)))
(setq company-idle-delay t)
;; Byt till h respektive cpp fil
(add-hook 'c-mode-common-hook
	  (lambda() 
	    (local-set-key  (kbd "M-Y") 'ff-find-other-file)))
(define-key global-map (kbd "RET") 'newline-and-indent)
;; (if (and (boundp 'flymake-mode) flymake-mode)
(add-hook 'company-mode
	  (lambda()
	    (global-set-key (kbd "TAB") 'complete-or-indent)))
(global-set-key (kbd "C-B") 'compile)
(global-set-key (kbd "C-4") 'split-window-vertically)
(global-set-key (kbd "M-#") 'delete-other-windows)
(global-set-key (kbd "M-B") 'kill-whole-line)
(global-set-key (kbd "M-J") 'back-to-indentation)
(global-set-key (kbd "M-L") 'end-of-line)
(global-set-key (kbd "C-/") 'comment-or-uncomment-region-or-line)
(global-set-key (kbd "M-?") 'comment-dwim)
(global-set-key (kbd "M->") 'hs-toggle-hiding)
;; Move temp files to other dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Scroll one line at a time
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; One line at a time
(setq mouse-wheel-progressive-speed nil) ;; Don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1)  ;; keyboard scroll one line at a time

;; Match paranthesis
(show-paren-mode 1)
(setq show-paren-delay 1)

;; Remove startup screen
(setq inhibit-startup-screen t)
(setq-default cursor-type 'bar)
(add-to-list 'load-path "~/EmacsConfig/color-theme/themes")
(add-to-list 'load-path "~/EmacsConfig/color-theme/")
(add-to-list 'custom-theme-load-path "~/EmacsConfig/color-theme/themes")

(add-to-list 'load-path "~/EmacsConfig/color-theme/themes")
(require 'color-theme)
(require 'color-theme-molokai)
(color-theme-molokai)

(provide 'Emacs-Config)
