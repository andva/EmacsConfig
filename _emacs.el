;; My config file!

;; C++ mode
(add-hook 'c++-mode-hook
  '(lambda ()
     (c-set-style "stroustrup")
     (setq indent-tabs-mode nil)))

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

;; Turn of auto complete
;; (auto-fill-mode -1)
;; (flyspell-mode 0) 

;; Fix xmsi trigger key
(xmsi-mode 0)

;; Add color scheme

(add-to-list 'load-path "~/EmacsThemes/")
(add-to-list 'load-path "~/color-theme/")

(require 'color-theme)
(require 'ergoemacs-mode)

(require 'color-theme-sublime)

;; (setq color-theme-is-global t)
(color-theme-sublime)

;; Set standard indent to 2 rather that 4
(setq standard-indent 2)








