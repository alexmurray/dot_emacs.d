;;; mailscripts.el --- utilities for handling mail on Unixes  -*- lexical-binding: t; -*-

;; Author: Sean Whitton <spwhitton@spwhitton.name>
;; Version: 28
;; Package-Requires: (notmuch)

;; Copyright (C) 2018, 2019, 2020, 2022 Sean Whitton

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; The original purpose of this package was to make it easy to use the small
;; mail-handling utilities shipped in Debian's 'mailscripts' package from
;; within Emacs.  It now also contains some additional, thematically-related
;; utilities which don't invoke any of those scripts.
;;
;; Entry points you might like to look at if you're new to this package:
;; mailscripts-prepare-patch, notmuch-slurp-debbug,
;; notmuch-extract-{thread,message}-patches{,-to-project}.

;;; Code:

(require 'cl-lib)
(require 'thingatpt)
(require 'vc)
(require 'message)

(eval-when-compile (require 'notmuch))

(defgroup mailscripts nil
  "Customisation of functions in the mailscripts package."
  :group 'mail)

(defcustom mailscripts-extract-patches-branch-prefix nil
  "Prefix for git branches created by functions which extract patch series.

E.g. `email/'."
  :type 'string
  :group 'mailscripts)

(defcustom mailscripts-detach-head-from-existing-branch nil
  "Whether to detach HEAD before applying patches to an existing branch.

This is useful if you want to manually review the result of
applying patches before updating any of your existing branches,
or for quick, ad hoc testing of a patch series.

Note that this does not prevent the creation of new branches."
  :type '(choice (const :tag "Always detach" t)
		 (const :tag "Never detach" nil)
		 (const :tag "Ask whether to detach" ask))
  :group 'mailscripts)

(defcustom mailscripts-project-library 'project
  "Which project management library to use to choose from known projects.

Some mailscripts functions allow selecting the repository to
which patches will be applied from the list of projects already
known to Emacs.  There is more than one popular library for
maintaining a list of known projects, however, so this variable
must be set to the one you use."
  :type '(choice (const :tag "project.el" project)
		 (const :tag "Projectile" projectile))
  :group 'mailscripts)

;;;###autoload
(defun notmuch-slurp-debbug (bug &optional no-open)
  "Slurp Debian bug with bug number BUG and open the thread in notmuch.

If NO-OPEN, don't open the thread."
  (interactive "sBug number: ")
  (require 'notmuch)
  (call-process-shell-command (concat "notmuch-slurp-debbug " bug))
  (unless no-open
    (let* ((search (concat "Bug#" bug))
           (thread-id (car (process-lines notmuch-command
                                          "search"
                                          "--output=threads"
                                          "--limit=1"
                                          "--format=text"
                                          "--format-version=4" search))))
      (notmuch-search search t thread-id))))

;;;###autoload
(defun notmuch-slurp-debbug-at-point ()
  "Slurp Debian bug with bug number at point and open the thread in notmuch."
  (interactive)
  (save-excursion
    ;; the bug number might be prefixed with a # or 'Bug#'; try
    ;; skipping over those to see if there's a number afterwards
    (skip-chars-forward "#bBug" (+ 4 (point)))
    (notmuch-slurp-debbug (number-to-string (number-at-point)))))

(declare-function notmuch-show-get-subject "notmuch-show")
(declare-function notmuch-refresh-this-buffer "notmuch-lib")

;;;###autoload
(defun notmuch-slurp-this-debbug ()
  "When viewing a Debian bug in notmuch, download any missing messages."
  (interactive)
  (require 'notmuch)
  (let ((subject (notmuch-show-get-subject)))
    (notmuch-slurp-debbug
     (if (string-match "Bug#\\([0-9]+\\):" subject)
         (match-string 1 subject)
       (read-string "Bug number: ")) t)
    (notmuch-refresh-this-buffer)))

;;;###autoload
(defun notmuch-extract-thread-patches (repo branch &optional reroll-count)
  "Extract patch series in current thread to branch BRANCH in repo REPO.

The target branch may or may not already exist.

With an optional prefix numeric argument REROLL-COUNT, try to
extract the nth revision of a series.  See the --reroll-count
option detailed in mbox-extract-patch(1).

See notmuch-extract-patch(1) manpage for limitations: in
particular, this Emacs Lisp function supports passing only entire
threads to the notmuch-extract-patch(1) command."
  ;; We could obtain a list of message IDs for a subthread, say, and disjoin
  ;; them to produce a more specific query to pass to the script.  This could
  ;; help in large threads where the script fails to extract the right thing.
  (interactive
   "Dgit repo: \nsnew branch name (or leave blank to apply to current HEAD): \nP")
  (let ((search
	 (cond
	  ((derived-mode-p 'gnus-summary-mode 'gnus-article-mode)
	   (mailscripts--gnus-message-id-search t))
	  ((derived-mode-p 'notmuch-show-mode)
           ;; If `notmuch-show' was called with a notmuch query rather
           ;; than a thread ID, as `org-notmuch-follow-link' in
           ;; org-notmuch.el does, then `notmuch-show-thread-id' might
           ;; be an arbitrary notmuch query instead of a thread ID.  We
           ;; need to wrap such a query in thread:{} before passing it
           ;; to notmuch-extract-patch(1), or we might not get a whole
           ;; thread extracted (e.g. if the query is just id:foo)
           (if (string= (substring notmuch-show-thread-id 0 7) "thread:")
	       notmuch-show-thread-id
             (concat "thread:{" notmuch-show-thread-id "}")))
	  (t (user-error "Unsupported major mode"))))
        (default-directory (expand-file-name repo)))
    (mailscripts--check-out-branch branch)
    (shell-command
     (if reroll-count
         (format "notmuch-extract-patch -v%d %s | git am"
                 (prefix-numeric-value reroll-count)
                 (shell-quote-argument search))
       (format "notmuch-extract-patch %s | git am"
               (shell-quote-argument search)))
     "*notmuch-apply-thread-series*")))

;;;###autoload
(define-obsolete-function-alias
  'notmuch-extract-thread-patches-projectile
  'notmuch-extract-thread-patches-to-project
  "mailscripts 0.22")

;;;###autoload
(defun notmuch-extract-thread-patches-to-project ()
  "Like `notmuch-extract-thread-patches', but choose repo from known projects."
  (interactive)
  (mailscripts--project-repo-and-branch
   'notmuch-extract-thread-patches
   (when current-prefix-arg
     (prefix-numeric-value current-prefix-arg))))

(declare-function notmuch-foreach-mime-part "notmuch")
(declare-function notmuch--call-process "notmuch-lib")
(declare-function notmuch-show-get-message-id "notmuch-show")
(declare-function notmuch-show-pipe-message "notmuch-show")
(defvar gnus-article-buffer)
(declare-function article-decode-charset "gnus-art")
(declare-function gnus-article-mime-handles "gnus-art")
(declare-function gnus-summary-show-article "gnus-sum")

;;;###autoload
(defalias 'notmuch-extract-message-patches
  #'mailscripts-extract-message-patches)

;;;###autoload
(defun mailscripts-extract-message-patches (repo branch)
  "Extract patches attached to current message to branch BRANCH in repo REPO.
If there are no attachments that look like patches, offer to try piping the
whole message.

The target branch may or may not already exist.

Patches are applied using git-am(1), so we only consider
attachments with filenames which look like they were generated by
git-format-patch(1)."
  ;; See `debbugs-gnu-apply-patch' in debbugs-gnu.el for other ideas about
  ;; identifying which attachments are the patches to be applied.
  ;; We could make it a defcustom, so that users can supply their own filters.
  (interactive
   "Dgit repo: \nsnew branch name (or leave blank to apply to current HEAD): ")
  (let ((default-directory (expand-file-name repo))
	handles raw)
    (cond ((derived-mode-p 'gnus-summary-mode 'gnus-article-mode)
	   (with-current-buffer gnus-article-buffer
	     (setq handles (mapcar #'cdr (gnus-article-mime-handles))
		   raw (lambda ()
			 (gnus-summary-show-article 'raw)
			 (with-current-buffer gnus-article-buffer
			   (article-decode-charset)
			   (buffer-string))))))
	  ((derived-mode-p 'notmuch-show-mode)
	   (with-current-notmuch-show-message
	    (notmuch-foreach-mime-part (lambda (handle) (push handle handles))
				       (mm-dissect-buffer t)))
	   (setq raw (lambda ()
		       (let (ret)
			 (with-current-notmuch-show-message
			  (setq ret (buffer-string)))
			 ret))))
	  (t (user-error "Unsupported major mode")))
    (cl-callf2 cl-remove-if-not
	(lambda (h)
	  (and-let*
	      ((filename (cdr (assq 'filename (mm-handle-disposition h)))))
	    (string-match "\\`v?[0-9]+-.+\\.\\(?:patch\\|diff\\|txt\\)\\'"
			  filename)))
	handles)
    (if handles
	(cl-loop initially (mailscripts--check-out-branch branch)
		 for handle in handles do (mm-pipe-part handle "git am"))
      ;; We ask for confirmation because our code for identifying attached
      ;; patches, and for finding scissors, is very simple.
      (setq raw (funcall raw))
      (with-temp-buffer
	(insert raw)
	(goto-char (point-min))
	(let ((scissors (re-search-forward "^-- >8 --\\s-*$" nil t)))
	  (cl-case (or (and scissors
			    (yes-or-no-p
			     (substitute-quotes
			      "Pipe whole message to `git am --scissors'?"))
			    'scissors)
		       (yes-or-no-p
			(substitute-quotes
			 (if scissors "Pipe whole message to `git am'?"
"Could not identify attached patches; pipe whole message to `git am'?"))))
	    (scissors
	     (call-process-region nil nil "git" nil nil nil "am" "-c"))
	    ((t) (call-process-region nil nil "git" nil nil nil "am"))))))))

;;;###autoload
(define-obsolete-function-alias
  'notmuch-extract-message-patches-projectile
  'notmuch-extract-message-patches-to-project
  "mailscripts 0.22")

;;;###autoload
(defalias 'notmuch-extract-message-patches-to-project
  #'mailscripts-extract-message-patches-to-project)

;;;###autoload
(defun mailscripts-extract-message-patches-to-project ()
  "Like `mailscripts-extract-message-patches', but choose repo from known projects."
  (interactive)
  (mailscripts--project-repo-and-branch 'notmuch-extract-message-patches))

;;;###autoload
(defun mailscripts-prepare-patch ()
  "Prepare patches for mailing out in a project- and MUA-specific way.
This is a convenience wrapper command for interactive use only.
Its behaviour is subject to change as we add support for more MUAs, ways to
generate patches, etc.."
  (interactive)
  (call-interactively
   (if (eq (vc-deduce-backend) 'Git)
       ;; For Git, default to one message per patch, like git-send-email(1).
       ;; Only use attachments when configured for this project.
       ;;
       ;; We presently assume that if patches-as-attachments has been
       ;; configured for this project, it's unlikely that you'll want to send
       ;; any messages with --scissors patches.  That may not be correct.
       (cond
	((and (local-variable-p 'vc-prepare-patches-separately)
	      (not vc-prepare-patches-separately))
	 #'mailscripts-git-format-patch-attach)
	((and (catch 'found
		(dolist (buffer (buffer-list))
		  (when (and (string-search "unsent " (buffer-name buffer))
			     (with-current-buffer buffer
			       (derived-mode-p 'mail-mode 'message-mode)))
		    (throw 'found t))))
	      (yes-or-no-p "Append -- >8 -- patch to unsent message?"))
	 #'mailscripts-git-format-patch-append)
	(t #'mailscripts-git-format-patch-drafts))
     #'vc-prepare-patch)))

;;;###autoload
(defun mailscripts-git-format-patch-attach (args &optional new)
  "Compose mail with patches generated by git-format-patch(1) attached.
ARGS is a single string of arguments to git-format-patch(1).  If NEW is
non-nil (interactively, with a prefix argument), always start composing a
new message.  Otherwise, attach patches to an existing mail composition
buffer.  This is useful for sending patches in reply to bug reports, etc..

This command is a Git-specific alternative to `vc-prepare-patch' with nil
`vc-prepare-patches-separately'.  It makes it easier to take advantage of
various features of git-format-patch(1), such as reroll counts.
For a command for non-nil `vc-prepare-patches-separately', see
`mailscripts-git-format-patch-drafts'.
See also the interactive wrapper command `mailscripts-prepare-patch'."
  (interactive "sgit format-patch \nP")
  (let ((temp (make-temp-file "patches" t))
	(mml-attach-file-at-the-end t)
	patches subject)
    (condition-case err
	(setq patches (apply #'process-lines "git" "format-patch" "-o" temp
			     (split-string-and-unquote args))
	      subject
	      (if (file-exists-p (car patches))
		  (with-temp-buffer
		    (insert-file-contents (car patches))
		    (message-narrow-to-headers-or-head)
		    (and-let* ((subject (message-fetch-field "subject")))
		      (if (cdr patches)
			  (and (string-match
				"^\\[\\(.*PATCH.*?\\)\\(?:\\s-+[0-9]+/[0-9]+\\)?\\]\\s-"
				subject)
			       (format "[%s] " (match-string 1 subject)))
			subject)))
		(user-error "git-format-patch(1) created no patch files")))
      (error (delete-directory temp t)
	     (signal (car err) (cdr err))))
    (compose-mail (mailscripts--gfp-addressee) subject nil (not new) nil nil
		  `((delete-directory ,temp t)))
    (mapc #'mml-attach-file patches)
    (when (or (not subject) (cdr patches))
      (message-goto-subject))))

;;;###autoload
(defun mailscripts-git-format-patch-drafts (args)
  "Import patches generated by git-format-patch(1) to your drafts folder.
ARGS is a single string of arguments to git-format-patch(1).

This command is a Git-specific alternative to `vc-prepare-patch' with non-nil
`vc-prepare-patches-separately'.  It makes it easier to take advantage of
various features of git-format-patch(1), such as reroll counts.
For a command for nil `vc-prepare-patches-separately', see
`mailscripts-git-format-patch-attach'.
See also the interactive wrapper command `mailscripts-prepare-patch'."
  (interactive "sgit format-patch ")
  (let ((args (cons "--thread" (split-string-and-unquote args))))
    (when-let ((addressee (mailscripts--gfp-addressee)))
      (push (format "--to=%s" addressee) args))
    (cl-case mail-user-agent
      (gnus-user-agent (mailscripts--gfp-drafts-gnus args))
      (notmuch-user-agent (mailscripts--gfp-drafts-notmuch args))
      (t (user-error "Unsupported mail-user-agent `%s'" mail-user-agent)))))

(declare-function gnus-summary-header "gnus-score")
(declare-function gnus-summary-goto-article "gnus-sum")
(declare-function gnus-summary-copy-article "gnus-sum")
(declare-function gnus-summary-exit-no-update "gnus-sum")
(declare-function gnus-uu-mark-buffer "gnus-uu")
(declare-function gnus-group-read-group "gnus-group")
(declare-function gnus-group-read-ephemeral-group "gnus-group")

(defun mailscripts--gfp-drafts-gnus (args)
  (require 'gnus)
  (let* ((temp (make-temp-file "patches"))
	 (group (concat "nndoc+ephemeral:" temp))
	 (method `(nndoc ,temp (nndoc-article-type mbox)))
	 (summary (format "*Summary %s*" group))
	 message-id)
    (unwind-protect
	(progn (with-temp-file temp
		 (unless (zerop (apply #'call-process "git" nil t nil
				       "format-patch" "--stdout" args))
		   (user-error "git-format-patch(1) exited non-zero")))
	       (unless (gnus-alive-p) (gnus-no-server))
	       (gnus-group-read-ephemeral-group group method)
	       (setq message-id (gnus-summary-header "message-id"))
	       (gnus-uu-mark-buffer)
	       (gnus-summary-copy-article nil "nndraft:drafts"))
      (when-let ((buffer (get-buffer summary)))
	(with-current-buffer buffer
	  (gnus-summary-exit-no-update t)))
      (delete-file temp))
    (gnus-group-read-group t t "nndraft:drafts")
    (gnus-summary-goto-article message-id)))

(defun mailscripts--gfp-drafts-notmuch (args)
  (require 'notmuch)
  (let ((temp (make-temp-file "patches" t))
	(insert (cl-list* "insert" (format "--folder=%s" notmuch-draft-folder)
			  "--create-folder" notmuch-draft-tags)))
    (unwind-protect
	(mapc (lambda (patch)
		(unless (zerop (apply #'call-process "notmuch" patch
				      "*notmuch-insert output*" nil insert))
		  (display-buffer "*notmuch-insert output*")
		  (user-error "notmuch-insert(1) exited non-zero")))
	      (apply #'process-lines "git" "format-patch" "-o" temp args))
      (delete-directory temp t)))
  (notmuch-search (format "folder:%s" notmuch-draft-folder)))

(defun mailscripts-git-format-patch-append (args)
  "Append a patch generated by git-format-patch(1) to an unsent message.
ARGS is a single string of arguments to git-format-patch(1).
The patch is formatted such that a recipient can use the --scissors option to
git-am(1) to apply the patch; see \"DISCUSSION\" in git-format-patch(1)."
  (interactive (list (read-string "git format-patch " "-1 ")))
  (let ((dir default-directory))
    (compose-mail nil nil nil t)
    (save-excursion
      (save-restriction
	(message-narrow-to-headers-or-head)
	(let ((unsent-buffer (current-buffer))
	      (default-directory dir)
	      (args (split-string-and-unquote args))
	      (unsent-from (message-fetch-field "from")))
	  (widen)
	  (if (re-search-forward message-signature-separator nil t)
	      (progn (goto-char (pos-bol))
		     (push "--no-signature" args))
	    (goto-char (point-max)))
	  (if (fboundp 'ensure-empty-lines)
	      (ensure-empty-lines 1)
	    ;; This is only some of what (ensure-empty-lines 1) does.
	    (if (bolp)
		(unless (save-excursion (goto-char (pos-bol 0)) (eolp))
		  (newline))
	      (newline 2)))
	  (insert "-- >8 --\n")
	  (with-temp-buffer
	    (apply #'call-process "git" nil t nil "format-patch" "--stdout"
		   args)
	    (when (bobp)
	      (user-error "git-format-patch(1) produced no output"))
	    (goto-char (point-min))
	    (delete-line)		; drop "From $SHA1 $magic_timestamp"
	    (message-narrow-to-headers-or-head)
	    (when-let* ((unsent
			 (and unsent-from
			      (mail-header-parse-address-lax unsent-from)))
			(patch-from (message-fetch-field "from"))
			(patch (mail-header-parse-address-lax patch-from)))
	      (when (equal unsent patch)
		(message-remove-header "^From:\\|^Date:" t)))
	    (widen)
	    (goto-char (point-max))
	    (delete-blank-lines)
	    (append-to-buffer unsent-buffer 1 (point-max))))))))

(defun mailscripts--gfp-addressee ()
  "Try to find a recipient for the --to argument to git-format-patch(1)."
  (or (and (local-variable-p 'vc-default-patch-addressee)
	   vc-default-patch-addressee)
      (car (process-lines-ignore-status
	    "git" "config" "--get" "format.to"))
      (car (process-lines-ignore-status
	    "git" "config" "--get" "sendemail.to"))))

(defun mailscripts--check-out-branch (branch)
  (if (string= branch "")
      (when (and
	     ;; Don't proceed if HEAD is already detached.
	     (zerop (call-process "git" nil nil nil
				  "symbolic-ref" "--quiet" "HEAD"))
	     (or (eq mailscripts-detach-head-from-existing-branch t)
		 (and (eq mailscripts-detach-head-from-existing-branch 'ask)
		      (yes-or-no-p "Detach HEAD before applying patches?"))))
        (call-process-shell-command "git checkout --detach"))
    (call-process-shell-command
     (format "git checkout -b %s"
             (shell-quote-argument
              (if mailscripts-extract-patches-branch-prefix
                  (concat mailscripts-extract-patches-branch-prefix branch)
                branch))))))

(defun mailscripts--gnus-message-id-search (&optional thread)
  (format (if thread "thread:{id:%s}" "id:%s")
	  (string-trim (gnus-summary-header "message-id") "<" ">")))

(defvar projectile-known-projects)
(declare-function project-prompt-project-dir "project")
(declare-function projectile-completing-read "projectile")

(defun mailscripts--project-repo-and-branch (f &rest args)
  (let ((repo (cl-case mailscripts-project-library
		(project
		 (require 'project)
		 (project-prompt-project-dir))
		(projectile
		 (require 'projectile)
		 (projectile-completing-read
		  "Select Projectile project: " projectile-known-projects))
		(t
		 (user-error
		  "Please customize variable `mailscripts-project-library'."))))
        (branch (read-from-minibuffer
                 "Branch name (or leave blank to apply to current HEAD): ")))
    (apply f repo branch args)))

(provide 'mailscripts)

;;; mailscripts.el ends here
