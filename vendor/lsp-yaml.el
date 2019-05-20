;;; lsp-yaml.el --- Yaml support for lsp-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Iku Iwasa

;; Author: Iku Iwasa <iku.iwasa@gmail.com>
;; URL: https://github.com/iquiw/lsp-yaml
;; Version: 0.0.0
;; Package-Requires: ((lsp-mode "4.1") (emacs "25"))
;; Keywords: lsp yaml

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

;; Yaml support for lsp-mode.
;;
;; First, install "yaml-lanaguage-server" by "npm".
;;
;; And then, install "lsp-mode" package.
;;
;; To use lsp-yaml in yaml-mode buffer, add `lsp' to `yaml-mode-hook'.
;;
;;     (add-hook 'yaml-mode-hook #'lsp)
;;

;;; Code:

(require 'lsp-mode)

(defgroup lsp-yaml nil
  "Yaml support for lsp-mode."
  :group 'lsp-mode)

(defcustom lsp-yaml-completion t
  "Specify whether to enable autocompletion."
  :type 'boolean)

(defcustom lsp-yaml-format-enable nil
  "Specify whether to enable YAML format feature."
  :type 'boolean)

(defcustom lsp-yaml-format-options nil
  "Specify YAML format options as plist, alist or hash table.
Specified options are converted to JSON object under \"yaml.format\" and
sent to the server as is.

For example,

  (:singleQuote t :bracketSpacing :json-false :proseWrap \"preserve\")

will be sent as

  {
    \"yaml\": {
      \"format\": {
        \"singleQuote\": true,
        \"bracketSpacing\": false,
        \"proseWrap\": \"preserve\"
      }
    }
  }
"
  :type '(choice (plist :tag "Format options plist")
                 (alist :tag "Format options plist")))

(defcustom lsp-yaml-hover t
  "Specify whether to enable hover."
  :type 'boolean)

(defcustom lsp-yaml-server "yaml-language-server"
  "The \"yaml-language-server\" executable to use."
  :type 'file)

(defcustom lsp-yaml-schemas nil
  "Schemas plist or alist that associates schema with glob patterns.
This can be also a hash table."
  :type '(choice (plist :tag "Schemas plist")
                 (alist :tag "Schemas alist")))

(defcustom lsp-yaml-validate t
  "Specify whether to enable YAML validation feature."
  :type 'boolean)

(defun lsp-yaml--set-configuration ()
  "Notify lsp-yaml settings to server."
  (lsp--set-configuration (lsp-yaml--settings)))

(defun lsp-yaml--settings ()
  "Return lsp-yaml settings to be notified to server."
  `(:yaml
    (:completion
     ,(or lsp-yaml-completion :json-false)
     :format
     ,(lsp-yaml--format-options)
     :hover
     ,(or lsp-yaml-hover :json-false)
     :schemas
     ,(or lsp-yaml-schemas
          (make-hash-table))
     :validate ,(or lsp-yaml-validate :json-false))))

(defun lsp-yaml--format-options ()
  "Return format options settings.
The value is composed from `lsp-yaml-format-enable' and `lsp-yaml-format-options'."
  (let ((enable (or lsp-yaml-format-enable :json-false)))
    (pcase lsp-yaml-format-options
      ((pred json-plist-p) (cons :enable
                                 (cons enable lsp-yaml-format-options)))
      ((pred json-alist-p) (cons (cons "enable" enable)
                                 lsp-yaml-format-options))
      ((pred hash-table-p)
       (let ((options (copy-hash-table lsp-yaml-format-options)))
         (puthash "enable" enable options)
         options))
      (_ (user-error "Invalid `lsp-yaml-format-options'. Plist, alist or hash table is expected.")))))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection (list lsp-yaml-server "--stdio"))
                  :major-modes '(yaml-mode)
                  :server-id 'yaml
                  :initialized-fn (lambda (workspace)
                                    (with-lsp-workspace workspace
                                      (lsp-yaml--set-configuration)))))

(provide 'lsp-yaml)
;;; lsp-yaml.el ends here
