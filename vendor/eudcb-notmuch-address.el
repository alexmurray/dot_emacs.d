;;; eudcb-notmuch-address.el --- notmuch-address EUDC backend -*- lexical-binding: t -*-

;; Copyright (C) 2022 condition-alpha.com

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
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
;;    This library provides an interface to the notmuch-address
;;    package as an EUDC data source.

;;; Usage:
;;    To load the library, first `require' it:
;;
;;      (require 'eudcb-notmuch-address)
;;
;;    In the simplest case then just use:
;;
;;      (eudc-notmuch-address-set-server "localhost")
;;
;;    When using `eudc-server-hotlist', instead use:
;;
;;      (add-to-list 'eudc-server-hotlist '("localhost" . notmuch-address))

;;; Code:

(require 'eudc)
(require 'mail-parse)

;; hook ourselves into the EUDC framework
(eudc-protocol-set 'eudc-query-function
		   'eudcb-notmuch-address-query-internal
		   'notmuch-address)
(eudc-protocol-set 'eudc-list-attributes-function
		   nil
		   'notmuch-address)
(eudc-protocol-set 'eudc-protocol-attributes-translation-alist
		   nil
		   'notmuch-address)
(eudc-protocol-set 'eudc-protocol-has-default-query-attributes
		   nil
		   'notmuch-address)

(defun eudcb-notmuch-address-query-internal (query &optional _return-attrs)
  "Query `notmuch-address-options' with QUERY.
QUERY is a list of cons cells (ATTR . VALUE) where ATTRs should be valid
attribute names.
RETURN-ATTRS is a list of attributes to return, defaulting to
`eudc-default-return-attributes'."
  (let (result
        sres)
    ;; query notmuch-address for all search strings, merging the results
    (dolist (term query)
      (setq sres (append sres (notmuch-address-options (cdr term)))))
    (setq sres (seq-uniq sres))
    ;; parse search results into a result structure for EUDC
    (dolist (str sres)
      (let* ((parsed (mail-header-parse-address str t))
             (email (car parsed))
             (name (cdr parsed)))
        (if parsed
            ;; email could be decomposed, push individual fields
            (push `((email . ,email)
                    ,@(when name (list (cons 'name name))))
                  result)
          ;; else, just forward the value as-is
          (push (list (cons 'email str)) result))))
    result))

(defun eudcb-notmuch-address-set-server (dummy)
  "Set the EUDC server to notmuch-address.
The server in DUMMY is not actually used, since this backend
always and implicitly connetcs to an instance of notmuch running
on the local host."
  (interactive)
  (eudc-set-server dummy 'notmuch-address)
  (message "[eudc] notmuch-address server selected"))

(eudc-register-protocol 'notmuch-address)


(provide 'eudcb-notmuch-address)

;;; eudcb-notmuch-address.el ends here
