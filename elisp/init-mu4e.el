;;; init-mu4e.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-mu4e.el
;; Description: configure mu4e
;; Author: Alex de Wit
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This file loads and configures mu4e
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:
(use-package mu4e
  :ensure nil
  :load-path "/usr/share/emacs/site-lisp/mu4e/mu4e.el"
  :commands (mu4e)
  :init
;;  (use-package mu4e-overview :defer t)
;;  :bind ("M-z m" . mu4e)
  :custom
  (mu4e-maildir (expand-file-name "~/Maildir"))
  (mu4e-get-mail-command "mbsync -a")
  (mu4e-view-prefer-html t)
  (mu4e-update-interval 180)
  (mu4e-headers-auto-update t)
  (mu4e-compose-signature-auto-include nil)
  (mu4e-compose-format-flowed t)
  (mu4e-view-show-images t)
  (mu4e-change-filenames-when-moving t) ; work better for mbsync
  (mu4e-attachment-dir "~/Downloads")
  (message-kill-buffer-on-exit t)
  (mu4e-compose-dont-reply-to-self t)
  (mu4e-view-show-addresses t)
  (mu4e-confirm-quit nil)
  (mu4e-use-fancy-chars t)
  (mu4e-html2text-command "html2text --body-width=72")
  :hook
  ((mu4e-view-mode . visual-line-mode)
   (mu4e-compose-mode . (lambda () ;; make this my word proccessing mode
                          (visual-line-mode)
                          (use-hard-newlines -1)
                          (flyspell-mode))))
  :config
  (setq mu4e-contexts
        (list
         (make-mu4e-context
	   :name "nafnlaus"
	   :enter-func (lambda () (mu4e-message "Entering context nafnlaus"))
	   :leave-func (lambda () (mu4e-message "Leaving context nafnlaus"))
	   :match-func  (lambda (msg)
            (when msg
              (mu4e-message-contact-field-matches
               msg '(:from :to :cc :bcc) ".*nafnlaus.*")))
	   :vars '((mu4e-sent-folder   . "/nafnlaus/Sent")
		   (mu4e-drafts-folder . "/nafnlaus/Drafts")
		   (mu4e-trash-folder  . "/nafnlaus/Trash")
		   (user-full-name . "Alex de Wit")
		   (user-mail-address . "alex@nafnlaus.ca")
		   (mu4e-compose-signature . user-full-name)
		   (mu4e-compose-format-flowed . t)
		   (smtpmail-queue-dir . "~/Maildir/nafnlaus/queue/cur")
		   (sendmail-program . "/usr/bin/msmtp")
		   (send-mail-function . smtpmail-send-it)
		   (message-sendmail-f-is-evil . t)
		   (message-sendmail-extra-arguments . ("--read-envelope-from"))
		   (message-send-mail-function . message-send-mail-with-sendmail)
		   (smtpmail-debug-info . t)
		   (smtpmail-debug-verbose . t)
;;		   (mu4e-maildir-shortcuts . ( ("/gmail/INBOX"             . ?i)
;;					       ("/gmail/[email].Sent Mail" . ?s)
;;					       ("/gmail/[email].Trash"     . ?t)
;;					       ("/gmail/[email].All Mail"  . ?a)
;;					       ("/gmail/[email].Starred"   . ?r)
		   ;;					       ("/gmail/[email].Drafts"    . ?d)))
		   ))
	  (make-mu4e-context
	   :name "uw"
	   :match-func  (lambda (msg)
            (when msg
              (mu4e-message-contact-field-matches
               msg '(:from :to :cc :bcc) ".*uwaterloo.*")))
	   :vars '((mu4e-sent-folder   . "/uw/Sent")
		   (mu4e-drafts-folder . "/uw/Drafts")
		   (mu4e-trash-folder  . "/uw/Trash")
		   (user-mail-address . "alex.de.wit@uwaterloo.ca")
		   (user-full-name . "Alex de Wit")
		   (mu4e-compose-signature . (concat "Thanks," "\n" user-full-name))
		   (mu4e-compose-format-flowed . t)
		   (sendmail-program . "/usr/bin/msmtp")
		   (send-mail-function . smtpmail-send-it)
		   (message-sendmail-f-is-evil . t)
		   (message-sendmail-extra-arguments . ("--read-envelope-from"))
		   (message-send-mail-function . message-send-mail-with-sendmail)
		   (smtpmail-debug-info . t)
		   (smtpmail-debug-verbose . t))))))

(provide 'init-mu4e)
;;; init-mu4e.el ends here

