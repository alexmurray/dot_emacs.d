;;; init-undo-tree.el --- Initialise undo-tree package
;; undo-tree

;;; Commentary:
;;

;;; Code:

(global-undo-tree-mode 1)

(eval-after-load "diminish"
  '(diminish 'undo-tree-mode))

(provide 'init-undo-tree)

;;; init-undo-tree.el ends here
