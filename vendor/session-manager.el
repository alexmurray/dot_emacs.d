;;; session-manager.el --- Support for the Gnome Session Manager

;; Copyright (C) 2012 Boris Kaul

;; Author: Boris Kaul <localvoid@gmail.com>
;; Maintainer: Boris Kaul <localvoid@gmail.com>
;; Created: 28 Jul 2012
;; Version: 0.5
;; Keywords: gnome, session

;; This file is not part of GNU Emacs.

;; This file is free software...

;; along with this file.  If not, see <http://www.gnu.org/licenses/>.
(require 'dbus)

(defvar session-manager-client-id nil
  "Session Manager Client Id")

(defun session-manager-call (method &rest args)
  (apply 'dbus-call-method
         :session
         "org.gnome.SessionManager"
         "/org/gnome/SessionManager"
         "org.gnome.SessionManager"
         method args))

(defun session-manager-client-private-call (client-id method &rest args)
  (apply 'dbus-call-method
         :session
         "org.gnome.SessionManager"
         client-id
         "org.gnome.SessionManager.ClientPrivate"
         method args))

(defun session-manager-client-private-register-signal (client-id signal-name handler)
  (dbus-register-signal
   :session
   "org.gnome.SessionManager"
   client-id
   "org.gnome.SessionManager.ClientPrivate"
   signal-name
   handler))

(defun session-manager-register-client (app-id client-startup-id)
  (session-manager-call "RegisterClient" app-id client-startup-id))


(defun session-manager-unregister-client ()
  (session-manager-call "UnregisterClient" session-manager-client-id))

(defun session-manager-end-session-response (is-ok reason)
  (session-manager-client-private-call session-manager-client-id "EndSessionResponse"
                                       is-ok reason))

(defvar session-manager-query-end-session-hook nil)
(defvar session-manager-end-session-hook nil)
(defvar session-manager-cancel-end-session-hook nil)
(defvar session-manager-stop-hook nil)


(defun session-manager-init (name)
  (interactive
   (list
    (read-string "Name: ")))
  (setq session-manager-client-id
        (session-manager-register-client "emacs"
                                         (concat "emacs-" name)))
  (session-manager-client-private-register-signal
   session-manager-client-id "QueryEndSession"
   (lambda (flags)
     (message "SessionManager: QueryEndSession: %i" flags)
     (run-hooks 'session-manager-query-end-session-hook)
     (session-manager-end-session-response t "")))
  (session-manager-client-private-register-signal
   session-manager-client-id "EndSession"
   (lambda (flags)
     (message "SessionManager: EndSession: %i" flags)
     (run-hooks 'session-manager-end-session-hook)
     (run-hooks 'kill-emacs-hook)
     (session-manager-end-session-response t "")
     (setq kill-emacs-hook nil)
     (kill-emacs)))
  (session-manager-client-private-register-signal
   session-manager-client-id "CancelEndSession"
   (lambda ()
     (message "SessionManager: CancelEndSession")
     (run-hooks 'session-manager-cancel-end-session-hook)))
  (session-manager-client-private-register-signal
   session-manager-client-id "Stop"
   (lambda ()
     (message "SessionManager: Stop")
     (run-hooks 'session-manager-stop-hook)
     (kill-emacs))))

(provide 'session-manager)
;;; session-manager.el ends here
