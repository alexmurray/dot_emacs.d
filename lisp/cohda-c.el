;;; cohda-c.el --- Cohda C Standard

;;; Commentary:
;;

;;; Code:
(require 'cc-mode)

;; define a specific Cohda C indentation style - inherit from ellemtel
;; but use 2 spaces, no tabs and no offset for labels
(defconst cohda-c-style
  '("ellemtel"
    (indent-tabs-mode . nil)
    (c-basic-offset . 2)
    (c-offsets-alist . ((label . 0))))
  "Cohda C Programming Style.")

(c-add-style "cohda" cohda-c-style)

(provide 'cohda-c)

;;; cohda-c.el ends here
