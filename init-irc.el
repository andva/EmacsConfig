(require 'rcirc)

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
(setq rcirc-time-format "%Y-%m-%d %H:%M ")

;; Join these channels at startup.
(setq rcirc-server-alist
      '(
        ("irc.freenode.org"
         :channels ("#emacs" "##opengl"))
        )
      )

(setq rcirc-default-nick "bertilborst")

(setq rcirc-default-full-name "Curious Minds Want To Know")

(setq rcirc-authinfo
      '(("bitlbee" bitlbee bitlbeeusr bitlbeepsw)))
