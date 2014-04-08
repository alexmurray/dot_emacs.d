;;; init-ack.el --- Initialise the ack-and-a-half package

;;; Commentary:
;;

;;; Code:
(require 'ack-and-a-half)

;; shorter aliases for ack-and-a-half commands
(defalias 'ack 'ack-and-a-half)
(defalias 'ack-same 'ack-and-a-half-same)
(defalias 'ack-find-file 'ack-and-a-half-find-file)
(defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)

(provide 'init-ack-and-a-half)

;;; init-ack-and-a-half.el ends here
