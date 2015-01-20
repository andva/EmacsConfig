(require 'rcirc)

;; First make sure we ignore any ~/.netrc files...
(setq auth-sources '("~/.authinfo" "~/.authinfo.gpg"))

;; Don't print /away messages.
;; This does not require rcirc to be loaded already,
;; since rcirc doesn't define a 301 handler (yet).
(defun rcirc-handler-301 (process cmd sender args)
  "/away message handler.")

;; Keep input line at bottom.                                                                               
(add-hook 'rcirc-mode-hook
	  (lambda ()
	    (set (make-local-variable 'scroll-conservatively)
             8192)))

;; Adjust the colours of one of the faces.
(set-face-foreground 'rcirc-my-nick "red" nil)

;; Include date in time stamp.
(setq rcirc-time-format "%d|%H:%M ")

;; Join these channels at startup.
(setq rcirc-server-alist
      '(
        ("irc.freenode.org" nickserv "bertilborst" "")
       )
)

(setq rcirc-default-nick "andva")

(setq rcirc-default-full-name "Curious Minds Want To Know")

(setq rcirc-authinfo
      '(("freenode" nickserv "bertilborst" password "")
        )
)
