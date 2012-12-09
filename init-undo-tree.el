;;; init-undo-tree.el --- Initialise undo-tree package
;; undo-tree

;;; Commentary:
;;

;;; Code:

(global-undo-tree-mode 1)

(eval-after-load "diminish"
  '(diminish 'undo-tree-mode " U"))

(provide 'init-undo-tree)

;;; init-undo-tree.el ends here
