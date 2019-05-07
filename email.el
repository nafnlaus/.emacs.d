(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e/")
(require 'mu4e)

(use-package mu4e-maildirs-extension :ensure t
  :pin melpa
  :init (mu4e-maildirs-extension)
   (setq mu4e-maildirs-extension-custom-list
 	'("uw/INBOX" "uw/Drafts" "uw/Sent" "uw/Trash" "nafnlaus/INBOX" "nafnlaus/INBOX.Drafts" "nafnlaus/INBOX.Sent" "nafnlaus/INBOX.Trash")))



(setq mu4e-contexts
 `( ,(make-mu4e-context
     :name "nafnlaus"
     :match-func (lambda (msg) (when msg
       (string-prefix-p "/nafnlaus" (mu4e-message-field msg :maildir))))
     :vars '((mu4e-sent-folder   . "/nafnlaus/INBOX.Sent")
	     (mu4e-drafts-folder . "/nafnlaus/INBOX.Drafts")
	     (mu4e-trash-folder  . "/nafnlaus/INBOX.Trash")
	     (mu4e-refile-folder . "/nafnlaus/INBOX.Archive")))

   ,(make-mu4e-context
     :name "uw"
     :match-func (lambda (msg) (when msg
       (string-prefix-p "/uw" (mu4e-message-field msg :maildir))))
     :vars '((mu4e-sent-folder   . "/uw/Sent")
	     (mu4e-drafts-folder . "/uw/Drafts")
	     (mu4e-trash-folder  . "/uw/Trash")
	     (mu4e-refile-folder . "/uw/Archive")))))
