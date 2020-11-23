;; Used in emacs 27 to speed up initial package loading
(setq package-quickstart t)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(let ((font "DejaVu Sans Mono-10"))
  (add-to-list 'default-frame-alist `(font . ,font))
  (set-frame-font font))
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
