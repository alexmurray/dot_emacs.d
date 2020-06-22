;;; flycheck-pod.el --- Perl 5 POD support in Flycheck

;; Copyright (C) 2018 Richard M. Loveland <r@rmloveland.com>

;; Author: Richard M. Loveland <r@rmloveland.com>
;; URL: https://github.com/rmloveland/flycheck-pod
;; Keywords: tools, convenience
;; Version: 0.1

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Perl 5 POD syntax checking support for Flycheck.  Runs "podchecker" on the
;; current file.

;; You may need to customize the location of your compiler if
;; Emacs isn't seeing it on your PATH:

;; (setq flycheck-pod-executable "/usr/local/bin/podchecker")

;; For more information, see <https://perldoc.perl.org/podchecker.html>.

;;; Code:

(require 'flycheck)

(defgroup flycheck-pod nil
  "Perl 5 POD support for Flycheck."
  :prefix "flycheck-pod-"
  :group 'flycheck
  :link '(url-link :tag "Github"
                   "https://github.com/rmloveland/flycheck-pod"))

(flycheck-define-checker pod
  "A Perl 5 POD syntax checker using the 'podchecker' tool.
See URL `https://perldoc.perl.org/podchecker.html'."
  :command ("podchecker" "-warnings" "-warnings" source)
  :error-patterns
  ((error line-start
          (minimal-match "*** ERROR: ") (message)
          " at line " line " in file " (file-name) line-end))
  :modes (pod-mode perl-mode cperl-mode))

(add-to-list 'flycheck-checkers 'pod)

(provide 'flycheck-pod)

;;; flycheck-pod.el ends here
