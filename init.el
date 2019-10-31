;;; init.el --- Starting point for Alex Murray's Emacs Configuration -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(defvar gc-cons-threshold--orig gc-cons-threshold)
(setq gc-cons-threshold (* 100 1024 1024)
      gc-cons-percentage 0.6)

(defun apm-set-gc-threshold ()
  "Reset `gc-cons-threshold' and `gc-cons-percentage' to their default values."
  (setq gc-cons-threshold gc-cons-threshold--orig
        gc-cons-percentage 0.1))

;; prefer newer non-byte compiled sources to older byte compiled ones
(setq load-prefer-newer t)

;;; Package management
(require 'package)

;; use https for both melpa and gelpa
(eval-and-compile
  (setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                           ("melpa" . "https://melpa.org/packages/")
                           ("org" . "https://orgmode.org/elpa/"))))

;; this is done automatically in 27 etc
(if (or (version< emacs-version "27")
        (not package--initialized))
    (package-initialize))

(defvar use-package-enable-imenu-support t)
;; must be set before loading use-package
(setq use-package-enable-imenu-support t)
;; uncomment to debug package loading times
;; (setq use-package-verbose t)


;; Bootstrap `use-package' from melpa
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (require 'use-package))

(use-package diminish
  :ensure t)

(use-package bind-key
  :ensure t)

(use-package system-packages
  :ensure t
  :config
  (setq system-packages-package-manager 'apt)
  (setq system-packages-use-sudo t))

(use-package use-package-ensure-system-package
  :ensure t)

;; customisations
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;; load custom but ignore error if doesn't exist
(load custom-file t)

(use-package alert
  :ensure t
  :config (when (eq system-type 'gnu/linux)
            (setq alert-default-style 'notifications)))

(use-package doom-themes
  :ensure t
  :config
  (doom-themes-org-config)
  (load-theme 'doom-solarized-light t))

;; used in some of my yasnippet snippets
(defun apm-camelize (s &optional delim)
  "Convert under_score string S to CamelCase string with optional DELIM."
  (interactive "s")
  (mapconcat 'identity (mapcar
                        #'(lambda (word) (capitalize (downcase word)))
                        (split-string s (if delim delim "_"))) ""))

(defvar apm-preferred-emacs-version "26.2")
(when (version< emacs-version apm-preferred-emacs-version)
  (alert (format "Emacs version too old - please run %s or newer"
                 apm-preferred-emacs-version)
         :severity 'high))


;;; General settings etc

;; personalisation
(setq user-full-name "Alex Murray")

;; enable narrow-to-region
(put 'narrow-to-region 'disabled nil)

;; tabs are never ok
(setq-default indent-tabs-mode nil)

;; set a reasonable fill and comment column
(setq-default fill-column 75)
(setq-default comment-column 75)

;; just use y or n not yes or no
(defalias 'yes-or-no-p 'y-or-n-p)

;; don't try and use dialog boxes
(setq use-dialog-box nil)
(setq use-file-dialog nil)

;; inhibit startup message and splash screen
(setq inhibit-startup-message t)
;; remove message from initial scratch buffer
(setq initial-scratch-message nil)

;; disable menu, tool and scroll-bars, show time
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode 0))
;; Show line column numbers in mode line
(line-number-mode 1)
(column-number-mode 1)
;; Show buffer size in mode line
(size-indication-mode 1)
;; don't use gtk style tooltips since are intrusive
(when (boundp 'x-gtk-use-system-tooltips)
  (setq x-gtk-use-system-tooltips nil))
(blink-cursor-mode -1)
(setq frame-title-format
      '((buffer-file-name "%f" (dired-directory dired-directory "%b"))))
(setq icon-title-format frame-title-format)
;; Mouse & Smooth Scroll
;; Scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq scroll-step 1
      scroll-margin 0
      scroll-conservatively 100000)

;; make emacs less laggy
(setq inhibit-compacting-font-caches t)

(set-language-environment "UTF-8")

;; default to maximised windows
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(toggle-frame-maximized)

;; set preferred font
(add-to-list 'default-frame-alist '(font . "Hack-10"))

;; prompt when trying to switch out of a dedicated window
(setq switch-to-buffer-in-dedicated-window 'prompt)

;; ensure scrolling forwards / backwards preserves original location such that
;; they undo each other
(setq scroll-preserve-screen-position 'always)

;; from http://endlessparentheses.com/fill-and-unfill-paragraphs-with-a-single-key.html
(defun endless/fill-or-unfill ()
  "Like `fill-paragraph', but unfill if used twice."
  (interactive)
  (let ((fill-column
         (if (eq last-command 'endless/fill-or-unfill)
             (progn (setq this-command nil)
                    (point-max))
           fill-column)))
    (call-interactively #'fill-paragraph)))

(bind-key [remap fill-paragraph] #'endless/fill-or-unfill)

;;; Packages
(use-package abbrev
  :diminish abbrev-mode
  :config
  (setq save-abbrevs t)
  (setq-default abbrev-mode t))

(use-package ace-window
  :ensure t
  :after avy
  :defer t
  :bind (("C-x o" . ace-window))
  :config (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package adaptive-wrap
  :ensure t
  :defer t
  :hook ((text-mode . adaptive-wrap-prefix-mode)))

(use-package aggressive-indent
  :ensure t
  :defer t
  :diminish aggressive-indent-mode
  :config (global-aggressive-indent-mode))

(use-package all-the-icons
  :ensure t
  :config
  (unless (file-exists-p (expand-file-name "~/.local/share/fonts/all-the-icons.ttf"))
    (all-the-icons-install-fonts)))

(use-package all-the-icons-dired
  :ensure t
  :hook ((dired-mode . all-the-icons-dired-mode)))

(use-package all-the-icons-ivy
  :ensure t
  :after (all-the-icons ivy)
  :custom (all-the-icons-ivy-buffer-commands '(ivy-switch-buffer-other-window))
  :config
  (add-to-list 'all-the-icons-ivy-file-commands 'counsel-dired-jump)
  (add-to-list 'all-the-icons-ivy-file-commands 'counsel-find-library)
  (all-the-icons-ivy-setup))

(use-package android-mode
  :ensure t
  :defer t
  :diminish android-mode
  :commands android-mode
  :hook java-mode
  :init
  (setq android-mode-sdk-dir (expand-file-name "~/android-sdk-linux/"))
  ;; change prefix so doesn't conflict with comment-region
  (setq android-mode-key-prefix (kbd "C-c C-m")))

(use-package ansi-color
  ;; show colours correctly in shell
  :config (ansi-color-for-comint-mode-on))

(use-package anzu
  :ensure t
  :diminish anzu-mode global-anzu-mode
  :init (global-anzu-mode)
  :bind (("M-%" . anzu-query-replace-regexp)
         ("C-M-%" . anzu-query-replace)))

(use-package apm-misc
  :load-path "lisp/"
  :bind (("C-c b l" . apm-browse-lp-bug-at-point)))

(use-package apparmor-mode
  :load-path "vendor/")

(use-package appt
  :preface
  (defun apm-appt-notify (time-to-appt time msg)
    "Notify for appointment at TIME-TO-APPT TIME MSG alert."
    (alert msg
           :title (format "Appointment in %s minutes [%s]" time-to-appt time)
           :icon "/usr/share/icons/gnome/32x32/status/appointment-soon.png"))
  :config
  (setq appt-disp-window-function #'apm-appt-notify)
  (appt-activate 1))

(use-package apropos
  :bind ("C-h a" . apropos))


(use-package asn1-mode
  :ensure t
  :defer t
  :mode (("\\.asn1?$" . asn1-mode)))

(use-package autorevert
  :diminish auto-revert-mode
  :config (global-auto-revert-mode 1))

(use-package auctex
  :ensure t
  :preface
  (defun apm-latex-mode-setup ()
    "Tweaks and customisations for LaTeX mode."
    ;; Enable source-correlate for Control-click forward/reverse search.
    (TeX-source-correlate-mode 1)
    ;; enable math mode in latex
    (LaTeX-math-mode 1)
    ;; Enable reftex
    (turn-on-reftex))
  :defer t
  :commands (LaTeX-math-mode TeX-source-correlate-mode)
  :mode ("\\.tex\\'" . LaTeX-mode)
  :hook ((LaTeX-mode . apm-latex-mode-setup))
  :init
  (setq-default TeX-auto-save t)
  (setq-default TeX-parse-self t)
  (setq-default TeX-PDF-mode t)
  (setq-default TeX-master nil)
  (setq-default reftex-plug-into-AUCTeX t)
  (setq-default TeX-source-correlate-start-server t))

(use-package secrets
  ;; patch bug in secrets - https://haukerehfeld.de/notes/2018-06-emacs-secrets-dbus-bug/
  :config
  (defun secrets-search-items (collection &rest attributes)
    "Search items in COLLECTION with ATTRIBUTES.
ATTRIBUTES are key-value pairs.  The keys are keyword symbols,
starting with a colon.  Example:

  (secrets-search-items \"Tramp collection\" :user \"joe\")

The object labels of the found items are returned as list."
    (let ((collection-path (secrets-unlock-collection collection))
          result props)
      (unless (secrets-empty-path collection-path)
        ;; Create attributes list.
        (while (consp (cdr attributes))
          (unless (keywordp (car attributes))
            (error 'wrong-type-argument (car attributes)))
          (unless (stringp (cadr attributes))
            (error 'wrong-type-argument (cadr attributes)))
          (setq props (append
                       props
                       (list :dict-entry
                             ;; HACK fixed so that dict entries are conses
                             (list
                              (substring (symbol-name (car attributes)) 1)
                              (cadr attributes))))
                attributes (cddr attributes)))
        ;; Search.  The result is a list of object paths.
        (setq result
              (dbus-call-method
               :session secrets-service collection-path
               secrets-interface-collection "SearchItems"
               (if props
                   (cons :array props)
                 '(:array :signature "{ss}"))))
        ;; Return the found items.
        (mapcar
         (lambda (item-path) (secrets-get-item-property item-path "Label"))
         result)))))

(use-package auth-source
  ;; prefer gnome-keyring via Login keyring, encrypted auth source to non-encrypted
  :init
  (require 'secrets)
  (setq auth-sources '("~/.authinfo" "secrets:Login" "~/.netrc")))

(use-package avy
  :ensure t
  :bind (("C-c SPC" . avy-goto-word-or-subword-1))
  :config
  ;; dim text when avy is active
  (setq avy-background t)
  ;; insert chars infront of target rather than over target
  (setq avy-style 'pre))

(use-package beginend
  :ensure t
  :diminish beginend-global-mode
  :config
  ;; beginend defines lots of different modes so diminish them all
  (dolist (m beginend-modes)
    (diminish (cdr m)))
  (beginend-global-mode 1))

(use-package bbdb
  :ensure t
  :config (bbdb-initialize 'message 'mu4e))

(use-package browse-kill-ring
  :ensure t)

(use-package bs
  :bind ("C-x C-b" . bs-show))

(use-package bug-reference
  :defer t
  :hook ((prog-mode . bug-reference-prog-mode)
         (erc-mode . bug-reference-mode)
         (mu4e-view-mode . bug-reference-mode)
         (org-mode . bug-reference-mode))
  :preface (defun apm-bug-reference-url-format ()
             (let ((prefix (match-string-no-properties 1))
                   (id (match-string-no-properties 3)))
               (cond ((or (string-prefix-p "lp" prefix t)
                          (string-prefix-p "bug" prefix t))
                      (format "https://bugs.launchpad.net/bugs/%s" id))
                     ((string-prefix-p "CVE" prefix)
                      (format "https://people.canonical.com/~ubuntu-security/cve/CVE-%s.html" id))
                     ((string-prefix-p "USN" prefix)
                      (format "https://usn.ubuntu.com/%s" id))
                     (t (error (concat "Unknown bug prefix '%s'" prefix))))))
  :init
  (eval-when-compile
    (require 'bug-reference))
  (setq bug-reference-url-format #'apm-bug-reference-url-format
        bug-reference-bug-regexp "\\<\\(\\([Ll][Pp]:?\\|bug\\) #?\\|CVE[ -]\\|USN[ -]\\)\\([0-9][0-9-]*\\)\\>"))

(use-package cargo
  :ensure t
  :defer t
  :hook ((rust-mode . cargo-minor-mode)))

(use-package cc-mode
  :defer t
  :preface
  ;; c-mode and other derived modes (c++, java etc) etc
  (defun apm-c-mode-common-setup ()
    "Tweaks and customisations for all modes derived from c-common-mode."
    (auto-fill-mode 1)
    ;; diminish auto-fill in the modeline
    (with-eval-after-load 'diminish
      (diminish 'auto-fill-function))
    ;; turn on auto-newline and hungry-delete
    (c-toggle-auto-hungry-state 1)
    ;; turn on electric indent
    (c-toggle-electric-state 1)
    ;; ensure fill-paragraph takes doxygen @ markers as start of new
    ;; paragraphs properly
    (setq paragraph-start "^[ ]*\\(//+\\|\\**\\)[ ]*\\([ ]*$\\|@param\\)\\|^\f"))
  :hook ((c-mode-common . apm-c-mode-common-setup))
  :config
  ;; from https://www.kernel.org/doc/html/v5.0/process/coding-style.html
  (defvar c-syntactic-element)
  (defun c-lineup-arglist-tabs-only (ignored)
    "Line up argument lists by tabs, not spaces"
    (let* ((anchor (c-langelem-pos c-syntactic-element))
           (column (c-langelem-2nd-pos c-syntactic-element))
           (offset (- (1+ column) anchor))
           (steps (floor offset c-basic-offset)))
      (* (max steps 1) c-basic-offset)))
  ;; Add upstream kernel style which uses actual tabs
  (c-add-style "linux-tabs-only"
               '("linux" (c-offsets-alist
                          (arglist-cont-nonempty
                           c-lineup-gcc-asm-reg
                           c-lineup-arglist-tabs-only)))))

(use-package apm-c
  :load-path "lisp/"
  :preface
  (defun apm-c-mode-setup ()
    "Tweaks and customisations for `c-mode'."
    (let ((filename (buffer-file-name)))
      (if (and filename (string-match-p (expand-file-name "~/apparmor-kernel") filename))
          ;; upstream linux kernel style actually uses tabs... urgh
          (progn
            (setq indent-tabs-mode t)
            (setq c-basic-offset 8)
            (c-set-style "linux-tabs-only")
            ;; silence byte compiler
            (eval-when-compile
              (require 'ethan-wspace))
            (with-eval-after-load 'ethan-wspace
              (setq ethan-wspace-errors (remove 'tabs ethan-wspace-errors))))
        ;; default to normal linux style
        (c-set-style "linux")))
    ;; always show trailing whitespace
    (setq show-trailing-whitespace t)
    ;; ensure fill-paragraph takes doxygen @ markers as start of new
    ;; paragraphs properly
    (setq paragraph-start "^[ ]*\\(//+\\|\\**\\)[ ]*\\([ ]*$\\|@param\\)\\|^\f"))
  :hook ((c-mode c++-mode) . apm-c-mode-setup)
  :config
  ;; treat linux styles as safe for local variable
  (add-to-list 'safe-local-variable-values '(c-indentation-style . linux))
  (add-to-list 'safe-local-variable-values '(c-indentation-style . linux-tabs-only)))

(use-package check-cves-mode
  :load-path "~/ubuntu/git/ubuntu-cve-tracker/scripts/"
  :mode ("check-cves\\..*\\'" . check-cves-mode))

(use-package company
  :ensure t
  :diminish company-mode
  ;; Use Company for completion
  :bind (("C-<tab>" . company-complete-common)
         :map company-mode-map
         ([remap completion-at-point] . company-complete-common)
         ([remap complete-symbol] . company-complete-common))
  :init (global-company-mode 1)
  :config
  (setq tab-always-indent 'complete)
  ;; some better default values
  (setq company-idle-delay 0.2)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 1)
  (setq company-selection-wrap-around t)

  ;; align annotations in tooltip
  (setq company-tooltip-align-annotations t)

  ;; nicer keybindings
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "C-d") 'company-show-doc-buffer)

  ;; put most often used completions at stop of list
  (setq company-transformers '(company-sort-by-occurrence)))

(use-package company-anaconda
  :ensure t
  :defer t
  :after company
  :init (add-to-list 'company-backends #'company-anaconda))

(use-package company-auctex
  :ensure t
  :defer t
  :hook ((LaTeX-mode . company-auctex-init)))

(use-package company-box
  :ensure t
  :diminish company-box-mode
  :hook ((company-mode . company-box-mode))
  :config
  (setq company-box-backends-colors nil)
  (setq company-box-show-single-candidate t)
  (setq company-box-max-candidates 50)
  (setq company-box-icons-alist 'company-box-icons-all-the-icons)
  (defun company-box-icons--elisp (candidate)
    (when (derived-mode-p 'emacs-lisp-mode)
      (let ((sym (intern candidate)))
        (cond ((fboundp sym) 'Function)
              ((featurep sym) 'Module)
              ((facep sym) 'Color)
              ((boundp sym) 'Variable)
              ((symbolp sym) 'Text)
              (t . nil)))))

  (with-eval-after-load 'all-the-icons
    (declare-function all-the-icons-faicon 'all-the-icons)
    (declare-function all-the-icons-material 'all-the-icons)
    (setq company-box-icons-all-the-icons
          `((Unknown . ,(all-the-icons-material "find_in_page" :height 0.9 :v-adjust -0.2))
            (Text . ,(all-the-icons-material "text_fields" :height 0.9 :v-adjust -0.2))
            (Method . ,(all-the-icons-faicon "cube" :height 0.9 :v-adjust -0.06 :face 'all-the-icons-purple))
            (Function . ,(all-the-icons-faicon "cube" :height 0.9 :v-adjust -0.06 :face 'all-the-icons-purple))
            (Constructor . ,(all-the-icons-faicon "cube" :height 0.9 :v-adjust -0.06 :face 'all-the-icons-purple))
            (Field . ,(all-the-icons-faicon "tag" :height 0.9 :v-adjust -0.06 :face 'all-the-icons-blue))
            (Variable . ,(all-the-icons-faicon "tag" :height 0.9 :v-adjust -0.06 :face 'all-the-icons-blue))
            (Class . ,(all-the-icons-material "settings_input_component" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-orange))
            (Interface . ,(all-the-icons-material "share" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-blue))
            (Module . ,(all-the-icons-material "view_module" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-blue))
            (Property . ,(all-the-icons-faicon "wrench" :height 0.9 :v-adjust -0.06))
            (Unit . ,(all-the-icons-material "settings_system_daydream" :height 0.9 :v-adjust -0.2))
            (Value . ,(all-the-icons-material "format_align_right" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-blue))
            (Enum . ,(all-the-icons-material "storage" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-orange))
            (Keyword . ,(all-the-icons-material "filter_center_focus" :height 0.9 :v-adjust -0.2))
            (Snippet . ,(all-the-icons-material "format_align_center" :height 0.9 :v-adjust -0.2))
            (Color . ,(all-the-icons-material "palette" :height 0.9 :v-adjust -0.2))
            (File . ,(all-the-icons-faicon "file-o" :height 0.9 :v-adjust -0.06))
            (Reference . ,(all-the-icons-material "collections_bookmark" :height 0.9 :v-adjust -0.2))
            (Folder . ,(all-the-icons-faicon "folder-open" :height 0.9 :v-adjust -0.06))
            (EnumMember . ,(all-the-icons-material "format_align_right" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-blueb))
            (Constant . ,(all-the-icons-faicon "square-o" :height 0.9 :v-adjust -0.06))
            (Struct . ,(all-the-icons-material "settings_input_component" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-orange))
            (Event . ,(all-the-icons-faicon "bolt" :height 0.9 :v-adjust -0.06 :face 'all-the-icons-orange))
            (Operator . ,(all-the-icons-material "control_point" :height 0.9 :v-adjust -0.2))
            (TypeParameter . ,(all-the-icons-faicon "arrows" :height 0.9 :v-adjust -0.06))
            (Template . ,(all-the-icons-material "format_align_center" :height 0.9 :v-adjust -0.2))))))

(use-package company-dabbrev
  :after company
  ;; keep original case
  :config (setq company-dabbrev-downcase nil))

(use-package company-gnome-shell
  :load-path "vendor/company-gnome-shell/"
  :after (company gnome-shell-mode)
  ;; don't do this - company-gnome-shell breaks company completion in other
  ;; modes...
  ;; :config (with-eval-after-load 'company
  ;;           (add-to-list 'company-backends 'company-gnome-shell))
  )

(use-package company-lsp
  :ensure t
  :after company
  :init (add-to-list 'company-backends 'company-lsp))

(use-package company-math
  :ensure t
  :defer t
  :after company
  ;; Add backend for math characters
  :init
  (add-to-list 'company-backends 'company-math-symbols-unicode)
  (add-to-list 'company-backends 'company-math-symbols-latex))

(use-package company-reftex
  :ensure t
  :after company
  :init
  (add-to-list 'company-backends 'company-reftex-labels)
  (add-to-list 'company-backends 'company-reftex-citations))

(use-package company-shell
  :ensure t
  :defer t
  :after company
  :init (add-to-list 'company-backends 'company-shell))

(use-package company-prescient
  :ensure t
  :after (company prescient)
  :config (company-prescient-mode 1))

(use-package company-web
  :ensure t
  :defer t
  :after company
  :init (add-to-list 'company-backends 'company-web-html))

(use-package compile
  ;; automatically scroll to first error on output
  :config (setq compilation-scroll-output 'first-error))

(use-package counsel
  :ensure t
  :after ivy org
  :diminish counsel-mode
  :init (counsel-mode 1)
  :bind (("C-x b" . counsel-switch-buffer)
         ("C-x 4 b" . counsel-switch-buffer-other-window)
         ("C-x C-r" . counsel-recentf)
         ("M-y" . counsel-yank-pop)
         ("C-c b" . counsel-search)
         ("C-c S" . counsel-grep-or-swiper)
         ("M-s s" . counsel-grep-or-swiper)
         ("C-c R" . counsel-grep-or-swiper-backward)
         ("M-s r" . counsel-grep-or-swiper-backward)
         :map outline-mode-map ("M-i" . counsel-outline)
         :map company-active-map ("C-/" . counsel-company))
  :config
  ;; required so we can use counsel-yank-pop in the minibuffer itself
  (setq enable-recursive-minibuffers t)
  (setq counsel-yank-pop-preselect-last t)
  ;; use google for searching
  (setq counsel-search-engine 'google)
  (with-eval-after-load 'helpful
    (setq counsel-describe-function-function #'helpful-callable)
    (setq counsel-describe-variable-function #'helpful-variable)))

(use-package counsel-projectile
  :ensure t
  :after (counsel projectile)
  :config
  ;; open project in vc after switching
  (counsel-projectile-modify-action
   'counsel-projectile-switch-project-action
   '((default counsel-projectile-switch-project-action-vc)))
  (counsel-projectile-mode))

(use-package counsel-world-clock
  :ensure t
  :after counsel)

(use-package cov
  :ensure t
  :defer t
  :diminish cov-mode
  :preface
  (defun apm-cov-mode-setup ()
    "Setup cov-mode."
    (make-local-variable 'cov-coverage-file-paths))
  :hook ((c-mode-common . cov-mode)
         (cov-mode . apm-cov-mode-setup)))

(use-package ccls
  :ensure t
  :init (unless (executable-find "ccls")
          (alert (format "Please install the ccls snap"))))

(use-package crontab-mode
  :ensure t)

(use-package cstyle
  :disabled t
  :load-path "vendor/")

(use-package cua-base
  ;; use CUA mode for rectangle selections etc but not copy/paste etc
  :init (cua-selection-mode 1))

(use-package cve-mode
  :load-path "~/ubuntu/git/ubuntu-cve-tracker/scripts/"
  :hook ((cve-mode . flycheck-mode)))

;; show suspicious c constructs automatically
(use-package cwarn
  :diminish cwarn-mode
  :init (global-cwarn-mode 1))

(use-package deadgrep
  :ensure t
  :bind ("<f5>" . deadgrep)
  :config (unless (executable-find "rg")
            (alert "Please install ripgrep snap...")))

(use-package debian-el
  :ensure t)

(use-package disk-usage
  :ensure t)

(use-package doom-modeline
  :ensure t
  :config
  (setq doom-modeline-height 26)
  (setq doom-modeline-major-mode-color-icon t)
  (setq doom-modeline-buffer-encoding nil)
  (doom-modeline-mode 1))

(use-package dpkg-dev-el
  :ensure t)

(use-package delsel
  ;; enable delete-selection mode to allow replacing selected region
  ;; with new text automatically
  :init (delete-selection-mode 1))

(use-package diff-hl
  :ensure t
  ;; Integrate with Magit and highlight changed files in the fringe of dired
  :hook ((magit-post-refresh . diff-hl-magit-post-refresh)
         (dired-mode . diff-hl-dired-mode))
  :config (global-diff-hl-mode 1))

(use-package disaster
  :ensure t
  :bind ((:map c-mode-base-map ("C-c d" . disaster))))

(use-package doxyas
  :load-path "vendor/"
  :commands doxyas-document-function
  ;; defer since is bound via general
  :defer t)

(use-package dts-mode
  :ensure t)

(use-package ediff
  :defer t
  :config (setq ediff-window-setup-function 'ediff-setup-windows-plain
                ediff-split-window-function 'split-window-horizontally))

(use-package eldoc
  :diminish eldoc-mode
  :config (global-eldoc-mode 1))

(use-package elf-mode
  :ensure t)

(use-package electric
  :init
  ;; electric indent, layout and pair modes to make more IDE like
  (electric-indent-mode 1)
  (electric-layout-mode 1)
  (electric-pair-mode 1))

(use-package elisp-def
  :ensure t
  :defer t
  :diminish elisp-def-mode
  :hook ((emacs-lisp-mode ielm-mode) . elisp-def-mode))

(use-package emojify
  :ensure t
  :ensure-system-package ("/usr/share/fonts/truetype/ancient-scripts/Symbola_hint.ttf" . fonts-symbola)
  :diminish emojify-mode
  :bind (("C-c e" . emojify-insert-emoji))
  :defer nil
  :config
  ;; display emojis using images since looks nicer
  (setq emojify-display-style 'image)
  ;; only replace unicode and github :slight_smile: style emojis (don't
  ;; mess with ascii ones)
  (setq emojify-emoji-styles '(unicode github))
  ;; echo the actual underlying character to the minibuffer when point is
  ;; over them so we don't mess with the displayed buffer itself
  (setq emojify-point-entered-behaviour 'echo)
  (global-emojify-mode 1))

(use-package erc
  :ensure t
  :preface
  (eval-when-compile
    (require 'erc-log)
    (require 'erc-match))

  ;; face to show in header line when disconnected
  (defface apm-erc-header-line-disconnected
    '((t (:foreground "black" :background "indianred")))
    "Face to use when ERC has been disconnected.")

  (defun apm-erc-update-header-line-show-disconnected ()
    "Use a different face in the header-line when disconnected."
    (erc-with-server-buffer
      (unless (erc-server-process-alive)
        'apm-erc-header-line-disconnected)))

  (defun apm-erc-find-logfile ()
    "Find and open the current `erc-mode` buffers logfile."
    (interactive)
    (when (and (eq major-mode 'erc-mode) erc-log-mode)
      (find-file-other-window (erc-current-logfile))))

  (defun apm-occur-in-erc (&optional regexp)
    "Find matches of REGEXP in all erc buffers.
With a prefix argument, will default to looking for all
`erc-keywords' and mentions of `erc-nick'."
    (interactive
     (list
      (let ((regex  (concat "\\(" (regexp-opt erc-keywords) "\\|"
                            (concat "\\(^\\|[^<]\\)" erc-nick "\\([^>]\\|$\\)")
                            "\\)")))
        (read-string "Regexp: "
                     (substring-no-properties
                      (cond ((region-active-p)
                             (buffer-substring (region-beginning) (region-end)))
                            (current-prefix-arg
                             regex)
                            (t
                             (word-at-point))))))))
    (let ((erc-buffers nil))
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when (and (eq major-mode 'erc-mode)
                     (not (erc-server-buffer-p)))
            (push buffer erc-buffers))))
      (multi-occur erc-buffers regexp)))

  (defun apm-erc-browse-url-from-channel-topic ()
    "Find urls in erc-channel-topic and offer to visit via `browse-url'."
    (interactive)
    (let ((topic erc-channel-topic)
          (urls nil))
      (with-temp-buffer
        (insert topic)
        (goto-char (point-min))
        (while (re-search-forward "https?://" nil t)
          (push (thing-at-point 'url t) urls)))
      (if urls
          (browse-url (completing-read "URL: " urls))
        (user-error "No URLs listed in channel topic"))))

  (defvar apm-erc-greetings '("hi" "hey" "g'day" "howdy" "greetings"))

  (defun erc-cmd-GOODMORNING (&rest _ignore)
    "Say good morning to all pals who are in the current channel."
    (erc-send-message (concat (nth (random (length apm-erc-greetings))
                                   apm-erc-greetings)
                              " "
                              (mapconcat #'identity
                                         (seq-intersection
                                          erc-pals
                                          (erc-get-channel-nickname-list))
                                         " "))))

  :bind (("C-c f e" . apm-erc-find-logfile)
         ("M-s e" . apm-occur-in-erc))
  :config
  (eval-and-compile
    (require 'erc-button)
    (require 'erc-desktop-notifications)
    (require 'erc-join)
    (require 'erc-log)
    (require 'erc-match)
    (require 'erc-networks)
    (require 'erc-services)
    (require 'erc-track))
  ;; canonical irc - we use this via znc-erc below
  (add-to-list 'erc-networks-alist '(Canonical "canonical.com"))
  (setq erc-nick "amurray")
  (setq erc-prompt-for-nickserv-password nil)
  ;; nickserv password for Canonical and freenode
  (dolist (network '((Canonical . "irc.canonical.com")
                     (freenode . "irc.freenode.net")))
    (let* ((pass (auth-source-pick-first-password :host (cdr network) :user erc-nick :login "NickServ")))
      (if (null pass)
          ;; secret-tool store --label='(car network) IRC NickServ' host (cdr network) user amurray login NickServ
          ;; then enter password
          (alert (format "Please store %s NickServ password in secret store for %s"
                         (cdr network) erc-nick))
        (add-to-list 'erc-nickserv-passwords `(,(car network) ((,erc-nick . ,pass)))))))

  (setq erc-autojoin-timing 'ident)

  (setq erc-autojoin-channels-alist nil)
  (setq erc-fill-function #'erc-fill-static)
  (setq erc-fill-static-center 18)
  (setq erc-fill-column 80)

  ;; use sensible buffer names with server as well
  (setq erc-rename-buffers t)

  (add-to-list 'erc-modules 'button)
  (add-to-list 'erc-modules 'log)
  (add-to-list 'erc-modules 'match)
  (add-to-list 'erc-modules 'notifications)
  (add-to-list 'erc-modules 'scrolltobottom)
  (add-to-list 'erc-modules 'services)
  (add-to-list 'erc-modules 'spelling)
  (erc-update-modules)

  ;; redefine this so we can quickly get to buffers from the
  ;; notification -
  ;; http://lists.gnu.org/archive/html/bug-gnu-emacs/2019-04/msg00156.html
  (defun erc-notifications-notify (nick msg)
    "Notify that NICK send some MSG.
This will replace the last notification sent with this function."
    (dbus-ignore-errors
      (setq erc-notifications-last-notification
            (let ((channel (current-buffer)))
              (notifications-notify :bus erc-notifications-bus
                                    :title (format "%s in %s"
                                                   (xml-escape-string nick)
                                                   channel)
                                    ;; don't keep multiple spaces
                                    :body (replace-regexp-in-string " +" " "
                                                                    (replace-regexp-in-string "\n" " "
                                                                                              (string-trim (xml-escape-string msg))))
                                    :replaces-id erc-notifications-last-notification
                                    :app-icon erc-notifications-icon
                                    :actions '("default" "Switch to buffer")
                                    :urgency 'critical
                                    :persistent t
                                    :on-action (lambda (&rest _)
                                                 (pop-to-buffer channel)))))))

  ;; format nicknames to show if user has voice(+), owner (~), admin (&),
  ;; operator (@)
  (setq erc-format-nick-function 'erc-format-@nick)
  (setq erc-pals '("joe" "jdstrand" "mdeslaur" "sbeattie" "jjohansen" "jj" "sarnold" "ChrisCoulson" "leosilva" "msalvatore" "ebarretto" "pfsmorigo" "markmorlino" "emitorino" "deafpool" "Avital" "dan" "sid"))

  (setq erc-keywords '("cve" "vulnerability" "apparmor" "seccomp" "exploit" "security" "esm"))

  ;; when joining don't bring to front
  (setq erc-join-buffer 'bury)

  ;; add to global-mode-string for doom-modeline to show
  (setq erc-track-position-in-mode-line t)
  (setq erc-track-switch-direction 'importance)
  (setq erc-track-exclude-types '("JOIN" "PART" "QUIT" "NICK" "MODE"
                                  ;; channel mode (324), creation
                                  ;; time (329), topic (332), topic
                                  ;; who time (333), names (353), no
                                  ;; chan modes (477)
                                  "324" "329" "332" "333" "353" "477"))
  (setq erc-track-exclude-server-buffer t)
  (setq erc-track-showcount t)

  (add-to-list 'erc-nick-popup-alist
               ;; defined down in eudc use-package
               '("Directory" . (apm-eudc-lookup-nick nick)))

  ;; only hide join / part / quit for those who are idle for more
  ;; than 10 hours (ie are using a bouncer)
  (setq erc-lurker-hide-list '("JOIN" "PART" "QUIT" "NICK"))
  (setq erc-lurker-threshold-time (* 10 60 60))

  ;; hide channel mode (324), creation time (329), topic (332), topic
  ;; who time (333), names (353) - see
  ;; https://www.alien.net.au/irc/irc2numerics.html
  (setq erc-hide-list '("324" "329" "332" "333" "353"))

  (setq erc-log-channels-directory "~/.emacs.d/erc/logs")
  (setq erc-log-insert-log-on-open nil)
  (setq erc-log-file-coding-system 'utf-8)
  (setq erc-log-write-after-send t)
  (setq erc-log-write-after-insert t)
  (setq erc-save-buffer-on-part t)

  (unless (file-exists-p erc-log-channels-directory)
    (mkdir erc-log-channels-directory t))

  (erc-autojoin-mode 1)

  (erc-spelling-mode 1)

  ;; change header line face when disconnected
  (setq erc-header-line-face-method
        #'apm-erc-update-header-line-show-disconnected)

  ;; make sure any privmsg (which is via query buffers) show up as urgent
  ;; in track list
  (defadvice erc-track-find-face (around erc-track-find-face-promote-query activate)
    (if (erc-query-buffer-p)
        (setq ad-return-value (intern "erc-current-nick-face"))
      ad-do-it))

  ;; ensure erc tries to reuse windows as much as possible
  (defun apm-reuse-erc-window (buffer action)
    (with-current-buffer buffer
      (if (eq major-mode 'erc-mode)
          ;; Don't override an explicit action
          (not action))))

  (add-to-list 'display-buffer-alist
               '(apm-reuse-erc-window . (display-buffer-reuse-mode-window
                                         (inhibit-same-window . t)
                                         (inhibit-switch-frame . t)
                                         (mode . erc-mode)))))

(use-package erc-hl-nicks
  :ensure t
  :after erc
  ;; erc-match will highlight pals with erc-pal-face so override that
  ;; for pretty colors
  :config (setq erc-hl-nicks-skip-faces
                (delete "erc-pal-face" erc-hl-nicks-skip-faces)))

(use-package erc-image
  :ensure t
  :after erc
  :config
  (add-to-list 'erc-modules 'image)
  (erc-update-modules))

(use-package erc-status-sidebar
  :ensure t
  :after erc)

(use-package erc-view-log
  :ensure t
  :config
  ;; colorise nicks better
  (with-eval-after-load 'erc-hl-nicks
    (setq erc-view-log-nickname-face-function #'erc-hl-nicks-make-face))
  (add-to-list 'auto-mode-alist
                       `(,(format "%s/.*\\.[log|txt]"
                                  (regexp-quote
                                   (expand-file-name
                                    erc-log-channels-directory))) . erc-view-log-mode)))

(use-package eudc
  ;; Store password using secret-tool as follows:
  ;; secret-tool store --label='Canonical LDAP' host ldaps://ldap.canonical.com
  ;; then enter PASSWORD
  :ensure-system-package (ldapsearch . ldap-utils)
  :config
  (eval-when-compile
    (require 'ldap))
  (setq eudc-server "ldaps://ldap.canonical.com")
  (setq eudc-server-hotlist
        '(("ldaps://ldap.canonical.com" . ldap)))
  (setq eudc-inline-expansion-servers 'hotlist)
  (setq ldap-host-parameters-alist
        `(("ldaps://ldap.canonical.com"
           base "ou=staff,dc=canonical,dc=com"
           binddn "cn=Alex Murray,ou=staff,dc=canonical,dc=com"
           auth-source t)))
  (setq ldap-default-host "ldaps://ldap.canonical.com")
  ;; better display of custom canonical ldap attributes
  (add-to-list 'eudc-user-attribute-names-alist
               '(mozillanickname . "IRC Nick"))
  (add-to-list 'eudc-user-attribute-names-alist
               '(mozillacustom1 . "Team"))
  (add-to-list 'eudc-user-attribute-names-alist
               '(mozillacustom3 . "Timezone Name"))
  (add-to-list 'eudc-user-attribute-names-alist
               '(mozillacustom4 . "UTC Offset"))
  (add-to-list 'eudc-user-attribute-names-alist
               '(launchpadid . "Launchpad ID"))
  ;; keep byte-compiler happy
  (defvar eudc-bob-generic-keymap nil)
  ;; add support for querying up the hierarchy via manager
  (defvar apm-eudc-bob-query-keymap
    (let ((map (make-sparse-keymap)))
      (define-key map [return] 'apm-eudc-query-at-point)
      map))
  (set-keymap-parent apm-eudc-bob-query-keymap eudc-bob-generic-keymap)
  (add-to-list 'eudc-attribute-display-method-alist '("manager" . apm-eudc-display-query))
  (add-to-list 'eudc-attribute-display-method-alist '("utc offset" . apm-eudc-display-utc-offset))
  (add-to-list 'eudc-attribute-display-method-alist '("timezone name" . apm-eudc-display-timezone))
  (add-to-list 'eudc-attribute-display-method-alist '("launchpad id" . apm-eudc-display-launchpadid))
  (defun apm-eudc-query-at-point ()
    (interactive)
    (let ((id (eudc-bob-get-overlay-prop 'query)))
      (unless id (error "No query here"))
      (let ((name))
        (save-match-data
          (if (not (and (string-match "cn=\\([^,]*\\)" id)
                        (setq name (match-string 1 id))))
              (error "Failed to extract cn from id %s" id)
            (eudc-display-records (eudc-query `((cn . ,name)))))))))

  (defun apm-eudc-display-timezone (timezone)
    "Display TIMEZONE with current local time."
      (insert (concat timezone
                      " ["
                      (current-time-string nil timezone)
                      "]")))

  (defun apm-eudc-display-utc-offset (offset)
    "Display OFFSET with current local time."
    (let ((hours (string-to-number (substring offset 0 3)))
          (mins (string-to-number (substring offset 3))))
      (insert (concat offset
                      " ["
                      (current-time-string
                       (time-add (current-time) (* (+ (* hours 60) mins) 60)) t)
                      "]"))))

  (defun apm-eudc-display-launchpadid (id)
    "Display ID as a clickable URL."
      (eudc-display-url (concat "https://launchpad.net/~" id)))

  (defun apm-eudc-display-query (query)
    "Display QUERY as an interactive element."
    (eudc-bob-make-button query apm-eudc-bob-query-keymap nil (list 'query query)))

  (defun apm-eudc-lookup-email (&optional email)
    (interactive (list
                  (let ((initial
                         ;; remove any opening / closing angle brackets if
                         ;; present
                         (replace-regexp-in-string "[<>]" ""
                                                   (substring-no-properties (or (thing-at-point 'email) "")))))
                    (read-string "Email address: " initial))))
    (eudc-display-records (eudc-query  `((email . ,email)))))

  (defun apm-eudc-lookup-nick (&optional nick)
    (interactive
     (list
      ;; some buffers are read-only in which case `word-at-point' returns a
      ;; read-only string so need to remove properties
      (let ((initial (substring-no-properties (or (word-at-point) ""))))
        (if (eq major-mode 'erc-mode)
            (completing-read "Nick: " (erc-get-channel-nickname-list)
                             nil nil initial)
          (read-string "Nick: " initial)))))
    (eudc-display-records (eudc-query  `((mozillaNickName . ,nick))))))

(use-package eshell
  :defer t
  :preface
  (defun apm-eshell-mode-setup ()
    "Initialise 'eshell-mode'."
    (eval-when-compile
      (require 'em-cmpl))
    (eshell-cmpl-initialize)
    (with-eval-after-load 'counsel
      (eval-when-compile (require 'esh-mode))
      (define-key eshell-mode-map [remap eshell-previous-matching-input] #'counsel-esh-history)
      (define-key eshell-mode-map [remap eshell-next-matching-input] #'counsel-esh-history)
      (define-key eshell-mode-map [remap eshell-pcomplete] #'completion-at-point)))
  :commands eshell
  :bind (("C-x m" . eshell))
  :hook ((eshell-mode . apm-eshell-mode-setup)))

(defun makefile-tabs-are-less-evil ()
  "Disable ethan-wspace from caring about tabs in Makefile's."
  ;; silence byte-compilation warnings
  (eval-when-compile
    (require 'ethan-wspace))
  (setq ethan-wspace-errors (remove 'tabs ethan-wspace-errors)))

(use-package ethan-wspace
  :ensure t
  :diminish ethan-wspace-mode
  ;; disable ethan-wspace caring about tabs in Makefile's
  :hook ((makefile-mode . makefile-tabs-are-less-evil))
  ;; ethan-wspace-mode raises lots of warnings if this is enabled...
  ;; hopefully this doesn't cause problems
  :config  (setq mode-require-final-newline nil)
  :init (global-ethan-wspace-mode 1))

(use-package exec-path-from-shell
  :ensure t
  :init (when (memq window-system '(mac ns))
          (exec-path-from-shell-initialize)))

(use-package expand-region
  :ensure t
  :bind (("C-c v" . er/expand-region))
  :config (setq expand-region-contract-fast-key "V"
                expand-region-reset-fast-key "r"))

(use-package eyebrowse
  :ensure t
  :config
  (eyebrowse-mode 1)
  ;; binds to M-1 M-2 etc to easily switch
  (eyebrowse-setup-opinionated-keys))

(use-package files
  :bind ("C-c r" . revert-buffer))

(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :commands flycheck-add-next-checker
  :ensure-system-package (cppcheck shellcheck)
  :init (setq-default flycheck-emacs-lisp-load-path 'inherit)
  :hook ((prog-mode . flycheck-mode))
  :config
  ;; use lsp-ui checker via cquery instead
  (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-gcc))
  (setq-default flycheck-display-errors-delay 0.2))

(use-package flycheck-checkbashisms
  :ensure t
  :after flycheck
  :ensure-system-package (checkbashisms . devscripts)
  :config (flycheck-checkbashisms-setup))

(use-package flycheck-clang-analyzer
  :ensure t
  :after flycheck
  :ensure-system-package clang
  :config
  (setq flycheck-clang-analyzer-executable "clang")
  (flycheck-clang-analyzer-setup)
  ;; automatically sets itself up as next checker after lsp-ui so undo
  ;; that so is instead after cppcheck
  (delete '(warning . clang-analyzer)
          (flycheck-checker-get 'lsp-ui 'next-checkers))
  (flycheck-add-next-checker 'c/c++-cppcheck '(t . clang-analyzer)))

(use-package flycheck-coverity
  :ensure t
  :after flycheck-clang-analyzer
  :init (unless (executable-find "cov-run-desktop")
          (alert "cov-run-desktop not found - is it installed?"))
  :config
  (flycheck-coverity-setup)
  (flycheck-add-next-checker 'clang-analyzer '(t . coverity)))

(use-package flycheck-cstyle
  :ensure t
  :disabled t
  :after lsp-ui
  :init (unless (executable-find "cstyle")
          (alert "cstyle not found - is it installed?"))
  :config
  (flycheck-cstyle-setup)
  (flycheck-add-next-checker 'lsp-ui '(warning . cstyle))
  (flycheck-add-next-checker 'cstyle '(t . c/c++-cppcheck)))

(use-package flycheck-package
  :ensure t
  :defer t
  :after flycheck
  :config (flycheck-package-setup))

(use-package flycheck-plantuml
  :ensure t
  :after flycheck
  :config (flycheck-plantuml-setup))

(use-package flycheck-posframe
  :ensure t
  :after (flycheck posframe)
  :hook ((flycheck-mode . flycheck-posframe-mode))
  :config (flycheck-posframe-configure-pretty-defaults))

(use-package flycheck-rust
  :ensure t
  :config (flycheck-rust-setup))

(use-package flyspell
  :defer t
  :diminish flyspell-mode
  :preface
  (defun apm-flyspell-ignore-http-and-https ()
    "Ignore flyspell checking anything that looks like a URL."
    (save-excursion
      (not (thing-at-point 'url))))
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode))
  :init
  (put 'text-mode 'flyspell-mode-predicate 'apm-flyspell-ignore-http-and-https)
  (put 'cve-mode 'flyspell-mode-predicate 'apm-flyspell-ignore-http-and-https))

(use-package flyspell-correct-ivy
  :ensure t
  ;; use instead of ispell-word
  :bind (([remap ispell-word] . flyspell-correct-word-generic)
         :map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))

(use-package forge
  :ensure t
  :after magit)

(use-package fuzzy
  :ensure t)

(use-package gdb-mi
  :defer t
  :init
  ;; use gdb-many-windows by default
  (setq-default gdb-many-windows t)
  ;; Non-nil means display source file containing the main routine at startup
  (setq-default gdb-show-main t))

(use-package gif-screencast
  :ensure t
  :ensure-system-package (scrot gifsicle))

(use-package gitconfig-mode
  :ensure t
  :defer t)

(use-package gitignore-mode
  :ensure t
  :defer t)

(use-package gnome-shell-mode
  :load-path "vendor/gnome-shell-mode/")

(use-package gnu-elpa-keyring-update
  :ensure t)

(use-package gnuplot
  :ensure t)

(use-package go-mode
  :ensure t)

(use-package google-translate-smooth-translate
  :ensure google-translate
  :defer t
  :init
  (eval-when-compile
    (require 'google-translate))
  (setq google-translate-listen-program (executable-find "totem")))

(use-package goto-addr
  :defer t
  :hook ((prog-mode . goto-address-prog-mode)
         (erc-mode . goto-address-mode)
         (text-mode . goto-address-mode)))

(use-package goto-line-preview
  :ensure t
  :config (global-set-key [remap goto-line] 'goto-line-preview))

(use-package grip-mode
  :ensure t
  :hook ((markdown-mode . grip-mode)))

(use-package gud
  :defer t
  :hook ((gud-mode . gud-tooltip-mode)))

(use-package helm-make
  :ensure t
  :config (setq helm-make-completion-method 'ivy))

(use-package helpful
  :ensure t
  :bind (("C-h a" . helpful-symbol)
         ("C-h f" . helpful-callable)
         ("C-h k" . helpful-key)
         ("C-h v" . helpful-variable)))

(use-package hideshow
  ;; use hs-minor-mode in programming and mail composing - TODO - get it
  ;; working during mail viewing as well to be able to hide quoted bits
  ;; - something like:
  ;; :config (add-to-list 'hs-special-modes-alist '(mu4e-view-mode "^>" "^[^^]"))
  :hook ((prog-mode message-mode) . hs-minor-mode))

(use-package hippie-exp
  :bind (("M-/" . hippie-expand)))

(use-package hl-todo
  :ensure t
  :defer t
  :functions global-hl-todo-mode
  :config
  (add-to-list 'hl-todo-keyword-faces '("@todo" . "#cc9393"))
  (global-hl-todo-mode))

(use-package hungry-delete
  :ensure t
  :diminish hungry-delete-mode
  :config (global-hungry-delete-mode 1))

(use-package imenu
  :bind (("M-i" . imenu)))

(use-package ispell
  :defer t
  :init
  ;; use aspell if can be found
  (if (null (executable-find "aspell"))
      (system-packages-install "aspell")
    ;; use gb dictionary via aspell if available
    (setq ispell-program-name "aspell"
          ispell-dictionary "british"
          ispell-extra-args '("--sug-mode=ultra"))))

(use-package ivy
  :ensure t
  :defer t
  :diminish ivy-mode
  :bind (("C-c C-r" . ivy-resume))
  :init (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t)
  ;; allow to select the typed in value with C-p
  (setq ivy-use-selectable-prompt t)
  (define-key isearch-mode-map (kbd "M-o") 'ivy-occur))

(use-package ivy-prescient
  :ensure t
  :config (ivy-prescient-mode 1))

(use-package ivy-rich
  :ensure t
  :after ivy
  :config (ivy-rich-mode 1))

(use-package ivy-xref
  :ensure t
  :init (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

(use-package js2-mode
  :ensure t
  :defer t
  :init (setq-default js2-basic-offset 2))

(use-package json-mode
  :ensure t)

(use-package lp
  :load-path "/snap/gitlptools/current"
  :after magit
  :init (unless (executable-find "git-lp-open")
          (alert "Please install the gitlptools snap")))

(use-package lsp-mode
  :ensure t
  ;; wants yasnippet by default and complains if not already available
  :after yasnippet
  :hook ((prog-mode . lsp))
  :config (setq lsp-prefer-flymake nil))

(use-package lsp-imenu
  :ensure lsp-mode
  :hook ((lsp-after-open . lsp-enable-imenu)))

(use-package lsp-java
  :ensure t
  :after lsp-mode
  :hook ((java-mode . lsp-java-enable)))

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :hook ((lsp-mode . lsp-ui-mode))
  :bind (:map lsp-ui-mode-map (([remap xref-find-definitions] . lsp-find-definition)
                               ([remap xref-find-references] . lsp-find-references)))
  :config (setq lsp-ui-sideline-enable nil
                lsp-ui-doc-enable t
                lsp-ui-doc-use-childframe t
                lsp-ui-doc-position 'top
                lsp-ui-doc-include-signature t))

(use-package lsp-yaml
  :load-path "vendor/"
  :after lsp
  :init
  (unless (executable-find "yaml-language-server")
    (alert "Please install the yaml-language-server snap"))
  :config
  (add-hook 'yaml-mode-hook #'lsp))

(use-package magit
  :ensure t
  :preface
  :defer t
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch))
  :config (setq magit-completing-read-function 'ivy-completing-read))

(use-package magit-popup
  ;; whilst magit doesn't need this anymore, other packages do and magit
  ;; doesn't provide it anymore so explicitly add it
  :ensure t)

(use-package mallard-mode
  :ensure t
  :defer t)

(use-package markdown-mode
  :ensure t
  :defer t
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :ensure-system-package markdown)

(use-package meson-mode
  :ensure t)

(use-package mml-sec
  :preface
  (defun apm-mml-secure-find-usable-key-prompt-for-missing-key (context name usage &optional justone)
    (when (y-or-n-p (format "No %s key for %s; do you want to manually choose one? "
                            usage name))
      (let ((keys (epa-select-keys context
                                   (format "Choose a key for %s " name))))
        (if (and justone (> (length keys) 0))
            (mml-secure-select-keys context name keys usage)
          keys))))
  :config
  (advice-add 'mml-secure-find-usable-keys :after-until #'apm-mml-secure-find-usable-key-prompt-for-missing-key)
  (setq mml-secure-key-preferences '((OpenPGP
                                      (sign)
                                      (encrypt
                                       ;; encrypt to multiple recipients -
                                       ;; from
                                       ;; https://wiki.ubuntu.com/SecurityTeam/Contacts
                                       ("security@ubuntu.com"
                                        "88E9530BCBDDC200517B5EB0F498D2D9DE7DAD9C" ;amurray
                                        "776FCD6573E7712AC111E3B0FBD3F310B1C9C4C1" ;joe
                                        "4C20C06B5D8BDE688854D28A51DBDC58CC559573" ;jdstrand
                                        "A6063BB5602309A43C8EBBD42F099E8D005E81F4" ;sbeattie
                                        "50C4A0DDCF31E452CEB19B516569D855A744BE93" ;mdeslaur
                                        "EDC4830FBD39AB6AC51047FB052F367018D5C3D8" ;jj
                                        "CE1D6FE48F85F246C77C3516AD3F818B34FA35B7" ;tyhicks
                                        "415010F1BA23C8C720DFB1F5F32172599D8D2E97" ;sarnold
                                        "44DFFFE4C1A008E83229E205611FBDECD5946E0F" ;ChrisCoulson
                                        "7FE79B445728C8EA0042839E45BCE75B840B1F69" ;leosilva
                                        "C196DEF7A9097968763884D1772835433D285D7D" ;msalvatore
                                        "0ADCB2CFA6B3532E80641CD2906788EB31A737FF" ;ebarretto
                                        "410774CCB1C9851EB8BFECADA179EB5276C81B4C" ;jmbl
                                        "3E2CC11CC9ED7154183B9615C2A82596AE602046" ;vineetha1
                                        "9027444394FA0EEB52BDE0B26D4A7990BDE2CC66" ;pfsmorigo
                                        "08AA09489F9DC266D9046E05A897FD7E8600E018" ;markmorlino
                                        "C8726381716BC3D27B868575CB8F16FBD3A64F82" ;emitorino
                                        "9673BF778F45A143CAF5FC32B46763D063A1DBEC")  ;Avital
                                       ("tyhicks@canonical.com" "CE1D6FE48F85F246C77C3516AD3F818B34FA35B7")))
                                     (CMS
                                      (sign)
                                      (encrypt)))))

(use-package modern-cpp-font-lock
  :ensure t
  :defer t
  :hook ((c++-mode . modern-c++-font-lock-mode)))

(use-package moinmoin-mode
  :load-path "vendor/")

(use-package move-text
  :ensure t
  :bind (([(meta shift up)] . move-text-up)
         ([(meta shift down)] . move-text-down)))

(use-package mu4e
  :load-path "/snap/maildir-utils/current/share/emacs/site-lisp/mu4e/"
  :bind (("C-c m" . mu4e))
  :preface

  ;; some hacky bits so we can do an async forward as attachment
  (defvar apm-mu4e-compose-mode-hook-orig)
  (defvar apm-mu4e-compose-forward-as-attachment-orig)

  (defun apm-mu4e-compose-forward-as-attachment-2 ()
    (setq mu4e-compose-forward-as-attachment
          apm-mu4e-compose-forward-as-attachment-orig)
    (setq mu4e-compose-mode-hook
          apm-mu4e-compose-mode-hook-orig))

  (defun apm-mu4e-compose-forward-as-attachment ()
    "Forward the message as an attachment."
    (interactive)
    (setq apm-mu4e-compose-mode-hook-orig mu4e-compose-mode-hook)
    (setq apm-mu4e-compose-forward-as-attachment-orig mu4e-compose-forward-as-attachment)
    (setq mu4e-compose-forward-as-attachment t)
    (setq mu4e-compose-mode-hook '(apm-mu4e-compose-forward-as-attachment-2))
    (mu4e-compose-forward))

  (defvar apm-mu4e-spammers '("duke.abbaddon@gmail.com"
                              "redmine@mantykora.net"
                              "jshaymac@gmail.com"
                              "jira@clockworkers.atlassian.net"
                              "advisories@auraredeye.zendesk.com"
                              "gohr@mail.continue.de"))

  ;; TODO: consider using imapfilter
  (defun apm-mu4e-refile-message (msg)
    (let ((mailing-list (mu4e-message-field msg :mailing-list))
          (subject (mu4e-message-field msg :subject)))
      (cond
       ((and (not (null mailing-list))
             (or (string-match-p "unsubscribe notification$" subject)
                 (string-match-p "^.* post from .* requires approval" subject)
                 (string-match-p "uncaught bounce notification" subject)
                 (string-match-p "Bounce action notification" subject)
                 (string-match-p "moderator request(s) waiting" subject)))
        mu4e-trash-folder)
       ((cl-some #'(lambda (e) (mu4e-message-contact-field-matches msg :from e)) apm-mu4e-spammers)
        "/Spam")
       ((string-match-p "m1-en0on.jp" (cdar (mu4e-message-field msg :from)))
        "/Spam")
       ((or (mu4e-message-contact-field-matches msg :from "do-not-reply@trello.com")
            (mu4e-message-contact-field-matches msg :from "bounce@websense.com")
            (mu4e-message-contact-field-matches msg :from "comments-noreply@docs.google.com")
            (mu4e-message-contact-field-matches msg :from "info@ubuntuforums.org")
            (mu4e-message-contact-field-matches msg :from "root@snakefruit.canonical.com")
            (mu4e-message-contact-field-matches msg :from "esm-notifications@canonical.com"))
        mu4e-trash-folder)
       ((or (string-match-p "^\\[.* Wiki\\] \\(Update of\\|New attachment added to page\\)" subject)
            (string-match-p "We just published a new Production deploy for ubuntusecuritypodcast" subject)
            (string-match-p "Archive is broken! (kernel ABI check failure)" subject))
        mu4e-trash-folder)
       ((string-match-p "^\\[Build #[0-9]+]" subject)
        mu4e-trash-folder)
       ((not (null mailing-list))
        (concat "/Lists/" (mu4e-get-mailing-list-shortname mailing-list)))
       ((or (mu4e-message-contact-field-matches msg :from "Snap Store")
            (string-match-p "^\\(R[eE]: \\)?Manual review \\(for .* version .* requested\\|requested for version\\)" subject)
            (string-match-p "^Package declaration update: " subject)
            (string-match-p "Store upload \\(scan \\)?failed for.*$" subject)
            (string-match-p "Store authorization failed for.*$" subject))
        "/snap-store")
       ((or (string-match-p "^\\(R[eE]: \\)?\\[Bug " subject)
            (mu4e-message-contact-field-matches msg :from "bugs.launchpad.net"))
        "/launchpad-bugs")
       ((mu4e-message-contact-field-matches msg :from "bugs.debian.org")
        "/debian-bugs")
       ((string-match-p "^\\(R[eE]: \\)?\\[Merge\\]" subject)
        "/merge-requests")
       ((or (mu4e-message-contact-field-matches msg :to "distros@vs.openwall.org")
            (mu4e-message-contact-field-matches msg :cc "distros@vs.openwall.org")
            (mu4e-message-contact-field-matches msg :to "linux-distros@vs.openwall.org")
            (mu4e-message-contact-field-matches msg :cc "linux-distros@vs.openwall.org"))
        "/Lists/linux-distros")
       ((mu4e-message-contact-field-matches msg :to "opensuse-security-announce@opensuse.org")
        "/Lists/opensuse-security-announce")
       ((mu4e-message-contact-field-matches msg :to "newsbox@idg.com")
        "/Lists/newsbox-idg")
       ((string-match-p "^Cron .* ~/bin/scripts-diff.sh$" subject)
        mu4e-trash-folder)
       ((mu4e-message-contact-field-matches msg :from "root@lillypilly.canonical.com")
        "/lillypilly")
       ((mu4e-message-contact-field-matches msg :from "root@keule.canonical.com")
        "/keule")
       ((mu4e-message-contact-field-matches msg :from "atpi.com")
        "/Travel")
       ((or (mu4e-message-contact-field-matches msg :from "rt@admin.canonical.com")
            (mu4e-message-contact-field-matches msg :to "rt@admin.canonical.com"))
        "/canonical-is")
       ((and (mu4e-message-contact-field-matches msg :from "security@ubuntu.com")
             (or (string-match-p "^Missing kernel CVE merge commits$" subject)
                 (string-match-p "^Kernel USN needed$" subject)))
        mu4e-trash-folder)
       ((or (mu4e-message-contact-field-matches msg :from "lillypilly.canonical.com")
            (and (mu4e-message-contact-field-matches msg :from "noreply@canonical.com")
                 (string-match-p "\\(contains outdated Ubuntu packages\\|built from outdated Ubuntu kernel\\)" subject))
            (and (mu4e-message-contact-field-matches msg :from "noreply@launchpad.net")
                 (string-match-p "^\\[.*\\] .* build of .* in ubuntu .*$" subject)))
        mu4e-trash-folder)
       ;; messages sent by me go to the sent folder
       ((cl-find-if (lambda (addr)
                      (mu4e-message-contact-field-matches msg :from addr))
                    mu4e-user-mail-address-list)
        mu4e-sent-folder)
       (t "/Archive"))))

  :config
  (setq mail-user-agent 'mu4e-user-agent)
  (setq mu4e-maildir (expand-file-name "~/Maildir"))
  (setq mu4e-sent-folder   "/Sent"
        mu4e-drafts-folder "/Drafts"
        mu4e-trash-folder  "/Trash")
  (setq mu4e-maildir-shortcuts
        '(("/Archive"              . ?a)
          ("/Drafts"               . ?d)
          ("/INBOX"                . ?i)
          ("/Sent"                 . ?s)
          ("/Spam"                 . ?j)
          ("/Trash"                . ?t)))

  (setq mu4e-user-mailing-lists '(;; ubuntu
                                  ("launchpad-announce.lists.ubuntu.com" . "launchpad-announce")
                                  ("ubuntu-devel.lists.ubuntu.com" . "ubuntu-devel")
                                  ("ubuntu-devel-announce.lists.ubuntu.com" . "ubuntu-devel-announce")
                                  ("ubuntu-hardened.lists.ubuntu.com" . "ubuntu-hardened")
                                  ("ubuntu-release.lists.ubuntu.com" . "ubuntu-release")
                                  ("ubuntu-security-announce.lists.ubuntu.com" . "ubuntu-security-announce")

                                  ;; canonical
                                  ("canonical-allhands.lists.canonical.com" . "canonical-allhands")
                                  ("canonical-announce.lists.canonical.com" . "canonical-announce")
                                  ("canonistack-announce.lists.canonical.com" . "canonistack-announce")
                                  ("canonical-snapcraft.lists.canonical.com" . "canonical-snapcraft")
                                  ("roadmap-sprint.lists.canonical.com" . "roadmap-sprint")
                                  ("ue-leads.lists.canonical.com" . "ue-leads")

                                  ;; security
                                  ("security-announce.lists.apple.com" . "apple-security-announce")
                                  ("bugtraq.list-id.securityfocus.com" . "bugtraq")
                                  ("debian-security-announce.lists.debian.org" . "debian-security-announce")
                                  ("fulldisclosure.lists.seclists.org" . "fulldisclosure")
                                  ("kernel-hardening.lists.openwall.com" . "kernel-hardening")
                                  ("officesecurity.lists.freedesktop.org" . "Officesecurity")
                                  ("opensuse-security-announce.opensuse.org" . "opensuse-security-announce")
                                  ("oss-security.lists.openwall.com" . "oss-security")
                                  ("rhsa-announce.redhat.com" . "rhsa-announce")
                                  ("security-ceph.com" . "security-ceph")
                                  ("xen-security-issues.lists.xenproject.org" . "xen-security-issues")
                                  ("xorg-security.lists.x.org" . "xorg-security")))

  (setq mu4e-refile-folder #'apm-mu4e-refile-message)
  (setq mu4e-get-mail-command "mbsync -a")
  ;; needed for mbsync
  (setq mu4e-change-filenames-when-moving t)

  ;; ensure this is off
  (setq mu4e-compose-format-flowed nil)
  (setq mu4e-headers-sort-direction 'ascending)
  (setq mu4e-use-fancy-chars t)
  ;; show all since often get duplicates via multiple mailing lists so want
  ;; to be able to see them all in general
  (setq mu4e-headers-skip-duplicates nil)

  (setq mu4e-update-interval 300)

  ;; cite with better formatting
  (setq message-citation-line-format "On %a, %Y-%m-%d at %T %z, %N wrote:\n")
  (setq message-citation-line-function #'message-insert-formatted-citation-line)

  ;; kill message buffer after sending rather than burying
  (setq message-kill-buffer-on-exit t)

  (setq mu4e-compose-reply-to-address "alex.murray@canonical.com"
        mu4e-user-mail-address-list '("alex.murray@canonical.com")
        user-mail-address "alex.murray@canonical.com"
        user-full-name  "Alex Murray")
  ;; encrypt to self
  (setq epg-user-id "alex.murray@canonical.com")
  (setq mml-secure-openpgp-encrypt-to-self t)
  (setq mml-secure-openpgp-sign-with-sender t)
  (setq mu4e-compose-complete-ignore-address-regexp
        "\\(no-?reply\\|bugs.launchpad.net\\|lillypilly.canonical.com\\)")
  (setq mu4e-compose-signature nil)

  ;; add action to view in brower
  (add-to-list 'mu4e-view-actions
               '("browser view" . mu4e-action-view-in-browser) t)

  (setq mu4e-html2text-command 'mu4e-shr2text)
  ;; html colors in shr usually look bad especially with a dark theme
  (with-eval-after-load 'shr
    (setq shr-use-colors nil))

  (setq mu4e-split-view 'horizontal)
  (setq mu4e-headers-visible-lines 15)
  (setq mu4e-completing-read-function 'completing-read)
  (setq mu4e-headers-fields '((:human-date . 12)
                              (:flags . 6)
                              (:mailing-list . 20)
                              (:from . 22)
                              (:size . 8)
                              (:subject)))

  ;; save attachment to Downloads
  (setq mu4e-attachment-dir (expand-file-name "~/Downloads"))

  ;; attempt to show images when viewing messages
  (setq mu4e-view-show-images t)

  ;; use mu's own viewer since then we only decrypt once ;)
  (setq mu4e-view-use-gnus nil)

  ;; show full addresses in message view
  (setq mu4e-view-show-addresses t)

  ;; always start mu4e in the background
  (mu4e t))

(use-package mu4e-alert
  :ensure t
  :hook ((after-init . mu4e-alert-enable-notifications)
         (after-init . mu4e-alert-enable-mode-line-display))
  :config (mu4e-alert-set-default-style 'notifications))

(use-package mu4e-jump-to-list
  :ensure t)

(use-package mu4e-patch
  :load-path "vendor/"
  :after mu4e
  :hook (mu4e-view-mode . mu4e-patch-highlight)
  :config
  (set-face-attribute 'mu4e-patch-commit-message nil :foreground "black")
  (set-face-attribute 'mu4e-patch-diff-stat-file nil :foreground "orange"))

(use-package no-littering
  :ensure t
  :config
  (eval-when-compile
    (require 'recentf))
  (with-eval-after-load 'recentf
    (add-to-list 'recentf-exclude no-littering-var-directory)
    (add-to-list 'recentf-exclude no-littering-etc-directory)))

(use-package nxml-mode
  ;; enable 'folding' with nxml-mode
  :init
  (require 'hideshow)
  (require 'sgml-mode)

  (add-to-list 'hs-special-modes-alist
               '(nxml-mode
                 "<!--\\|<[^/>]*[^/]>"
                 "-->\\|</[^/>]*[^/]>"

                 "<!--"
                 sgml-skip-tag-forward
                 nil))

  (add-hook 'nxml-mode-hook 'hs-minor-mode)
  :config (setq nxml-slash-auto-complete-flag t))

(use-package org
  :ensure org-plus-contrib
  :pin org
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link))
  :config
  (setq org-directory (expand-file-name "~/org-files/")
        org-agenda-files (mapcar #'(lambda (f)
                                     (expand-file-name f org-directory))
                                 '("personal.org" "canonical.org"
                                   "general.org" "blog.org" "notes.org"))
        ;; don't indent org document sections etc
        org-adapt-indentation nil
        org-imenu-depth 4
        org-todo-keywords '((sequence "TODO(t)" "IN-PROGRESS(p!)" "BLOCKED(b@)" "DEFERRED(D@)" "|" "DONE(d!)")
                            (sequence "|" "CANCELLED(c@)" "DELEGATED(G@)")))
  (add-to-list 'org-file-apps '("\\.webm\\'" . "xdg-open %s"))
  (add-to-list 'org-file-apps '("\\.aup\\'" . "audacity %s"))
  ;; set up org-babel integration for plantuml
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((plantuml .t))))

(use-package org-agenda
  :ensure org-plus-contrib
  :pin org
  :preface
  (defun apm-org-agenda-file-notify (_event)
    "Rebuild appointments when _EVENT specifies any org agenda files change."
    (org-agenda-to-appt t))
  :config
  ;; when modifying agenda files make sure to update appt
  (require 'filenotify)
  (dolist (file org-agenda-files)
    (file-notify-add-watch file '(change) #'apm-org-agenda-file-notify))
  ;; rebuild appointments now
  (org-agenda-to-appt t))

(use-package org-capture
  :after org
  :config (setq org-capture-templates `(("t" "todo" entry (file+headline ,(expand-file-name "canonical.org" org-directory) "Tasks")
                                         "* TODO %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n")
                                        ("m" "meeting" entry (file+headline ,(expand-file-name "canonical.org" org-directory) "Meetings")
                                         "* %?\n%(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n")
                                        ("i" "Topic" entry (file ,(expand-file-name "canonical.org" org-directory))
                                         "* %?\n%a\n")
                                        ("p" "Protocol" entry (file+headline ,(expand-file-name "notes.org" org-directory) "Inbox")
                                         "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
                                        ("L" "Protocol Link" entry (file+headline ,(expand-file-name "notes.org" org-directory) "Inbox")
                                         "* %? [[%:link][%:description]] \nCaptured On: %U"))))

(use-package org-clock
  :after org
  :bind (("C-c g" . org-clock-goto)
         ("C-c i" . org-clock-in)
         ("C-c o" . org-clock-out))
  :preface
  ;; assume idle after 5 minutes
  :ensure-system-package xprintidle
  :config
  (setq org-clock-idle-time 5
        ;; include the current clocked in task in clock reports
        org-clock-report-include-clocking-task t
        ;; save running clock and all history when exiting emacs
        org-clock-persist t
        ;; resume clocking task on clock-in if the clock is open
        org-clock-in-resume t
        ;; persist clock data into org-directory
        org-clock-persist-file (expand-file-name "org-clock-save.el" org-directory)
        ;; insert a note when TODOs are marked DONE
        org-log-done 'note)
  (setq org-clock-x11idle-program-name "xprintidle")
  ;; reload any saved org clock information on startup
  (org-clock-persistence-insinuate))

(use-package org-clock-convenience
  :ensure t
  :bind (:map org-agenda-mode-map
              ("S-<up>" . org-clock-convenience-timestamp-up)
              ("S-<down>" . org-clock-convenience-timestamp-down)))

(use-package org-duration
  :ensure org-plus-contrib
  ;; don't show days, only total hours as maximum value
  :config (setq org-duration-format (quote h:mm)))

(use-package orgit
  :ensure t
  ;; work-around org-link-store-props not being defined
  :config (defalias 'org-link-store-props #'org-store-link-props))

(use-package org-mru-clock
  :ensure t
  :bind (("C-c s" . org-mru-clock-in))
  :config
  (setq org-mru-clock-completing-read #'ivy-completing-read)
  (setq org-mru-clock-keep-formatting t)
  (setq org-mru-clock-how-many 50))

(use-package org-mu4e
  :after (mu4e org)
  ;; store link to message if in header view, not to header query
  :config (setq org-mu4e-link-query-in-headers-mode nil))

(use-package org-notify
  :ensure org-plus-contrib
  :after org
  :config
  (setq org-notify-audible nil)
  (org-notify-start)
  (org-notify-add 'default '(:time "24h" :actions -notify/window :duration 600))
  (org-notify-add 'default '(:time "60m" :actions -notify/window :period "2m" :duration 600))
  (org-notify-add 'default '(:time "15m" :actions -notify/window :period "2m" :duration 120)))

(use-package org-protocol
  :ensure org-plus-contrib
  :init (let ((handler (shell-command-to-string "xdg-mime query default x-scheme-handler/org-protocol")))
          (unless (string-match "^emacsclient.*.desktop" handler)
            ;; ensure an emacsclient26.desktop file exists in
            ;; ~/.local/share/applications and that it has a MimeType
            ;; entry of x-scheme-handler/org-protocol;

            ;; then run the following to set it as the default handler:
            ;; xdg-mime default emacsclient26.desktop \
            ;; x-scheme-handler/org-protocol

            ;; also see the following
            ;; https://github.com/sprig/org-capture-extension#under-linux
            (alert "Please configure emacsclient as handler for org-protocol"))))

(use-package org-table-sticky-header
  :ensure t
  :after org
  :defer t
  :diminish org-table-sticky-header-mode
  :hook ((org-mode . org-table-sticky-header-mode)))

(use-package org-timeline
  :ensure t
  :after org
  :hook ((org-agenda-finalize . org-timeline-insert-timeline)))

(use-package ob-plantuml
  :after plantuml-mode
  :config (with-eval-after-load 'plantuml-mode
            (setq org-plantuml-jar-path plantuml-jar-path)))

(use-package ox-hugo
  :ensure t)

(use-package paradox
  :ensure t
  :preface
  (defun apm-paradox-set-github-token (_no-fetch)
    "Load `paradox-github-token' from authinfo."
    (require 'epa-file)
    (require 'auth-source)
    (eval-when-compile
      (require 'paradox-github))
    (if (file-exists-p "~/.authinfo")
        (let ((authinfo-result (car (auth-source-search
                                     :max 1
                                     :host "github.com"
                                     :port "paradox"
                                     :user "paradox"
                                     :require '(:secret)))))
          (let ((paradox-token (plist-get authinfo-result :secret)))
            (setq paradox-github-token (if (functionp paradox-token)
                                           (funcall paradox-token)
                                         paradox-token))))
      (alert "No github token found in ~/.authinfo")))
  :commands (paradox-list-packages)
  :init (setq paradox-execute-asynchronously nil)
  :config
  (paradox-enable)
  (advice-add 'paradox-list-packages :before 'apm-paradox-set-github-token))

(use-package paredit
  :ensure t
  ;; don't steal occur prefix
  :bind (:map paredit-mode-map ("M-s" . nil))
  :hook ((emacs-lisp-mode . enable-paredit-mode)))

(use-package paren
  :defer t
  :commands show-paren-mode
  :config
  (show-paren-mode 1))

(use-package paren-face
  :ensure t
  :config (global-paren-face-mode 1))

(use-package pcap-mode
  :ensure t
  :mode ("\\.pcapng\\'" . pcap-mode))

(use-package pdf-tools
  :ensure t
  ;; only try and install when needed
  :mode ("\\.pdf\\'" . pdf-tools-install))

(use-package pdf-view-restore
  :ensure t
  :after pdf-tools
  :config
  (add-hook 'pdf-view-mode-hook 'pdf-view-restore-mode))

(use-package plantuml-mode
  :ensure t
  :mode ("\\.p\\(lant\\)?uml\\'" . plantuml-mode)
  :config
  (setq plantuml-jar-path (expand-file-name "~/plantuml.jar"))
  (unless (file-exists-p plantuml-jar-path)
    (message "plantuml not found at %s" plantuml-jar-path)))

(use-package posframe
  :ensure t
  :config (setq posframe-mouse-banish nil))

(use-package prescient
  :ensure t
  :config (prescient-persist-mode 1))

(use-package prog-mode
  :config
  (when (boundp 'prettify-symbols-unprettify-at-point)
    ;; show original text when point is over a prettified symbol
    (setq prettify-symbols-unprettify-at-point 'right-edge))
  ;; prettify symbols (turn lambda -> λ)
  (global-prettify-symbols-mode 1))

(use-package projectile
  :ensure t
  :defer t
  :bind (:map projectile-mode-map ("C-c p" . projectile-command-map))
  :diminish projectile-mode
  :defines (projectile-enable-caching)
  :init
  (setq projectile-enable-caching t)
  (projectile-mode 1)
  :config
  (add-to-list 'projectile-project-root-files "compile_commands.json")
  (add-to-list 'projectile-project-root-files "configure.ac")
  (add-to-list 'projectile-project-root-files ".cquery")
  (add-to-list 'projectile-project-root-files ".cquery.in")
  (add-to-list 'projectile-project-root-files "AndroidManifest.xml")
  (with-eval-after-load 'magit
    (setq projectile-switch-project-action #'magit-status))
  (with-eval-after-load 'ivy
    (setq projectile-completion-system 'ivy)))

(use-package python
  :defer t
  :init (setq-default python-indent-offset 4))

(use-package quilt
  :ensure t)

(use-package racer
  :ensure t
  :preface
  (defun apm-racer-mode-setup ()
    "Setup racer-mode."
    (eval-when-compile
      (require 'racer))
    (unless (executable-find "cargo")
      (alert "Please install rust: https://www.rust-lang.org/en-US/install.html"))
    (unless (file-exists-p racer-cmd)
      (alert "rustup toolchain add nightly && cargo +nightly install racer"))
    (unless (file-exists-p racer-rust-src-path)
      (alert "rustup component add rust-src"))
    (racer-mode 1))
  :defer t
  :hook ((rust-mode . apm-racer-mode-setup)))

(use-package rainbow-mode
  :ensure t
  :defer t
  :hook ((css-mode html-mode) . rainbow-mode))

(use-package region-state
  :ensure t
  :config (region-state-mode 1))

(use-package rnc-mode
  :ensure t)

(use-package rust-mode
  :ensure t)

(use-package sauron
  :ensure t
  :config
  ;; just use sauron for erc for now
  (setq sauron-modules '(sauron-erc))
  ;; notify on pals coming/going
  (with-eval-after-load 'erc-match
    (setq sauron-watch-nicks erc-pals))
  (setq sauron-max-line-length 120)
  (setq sauron-hide-mode-line t)
  (sauron-start))

;; save minibuffer history
(use-package savehist
  :init (savehist-mode 1)
  ;; I wonder if this is causing large CPU usage like
  ;; https://github.com/syl20bnr/spacemacs/issues/9409
  :disabled t)

(use-package saveplace
  :config
  (setq-default save-place t)
  (setq save-place-file (expand-file-name ".places" user-emacs-directory)))

(use-package scratch
  :ensure t
  :defer t)

(use-package server
  :config
  ;; start emacs server only it has not already been started
  (unless (server-running-p)
    (server-start))
  (add-hook 'after-make-frame-functions
            #'(lambda (frame) (select-frame-set-input-focus frame)) t))

(use-package session-manager
  :load-path "vendor/"
  :config (session-manager-init "apm"))

(use-package sh-script
  :init (setq-default sh-basic-offset 2
                      sh-indentation 2))

(use-package simple
  :defer t
  :diminish visual-line-mode
  :init
  ;; save whatever is in the system clipboard to the kill ring before
  ;; killing something else into the kill ring
  (setq save-interprogram-paste-before-kill t)
  (setq visual-line-fringe-indicators
        '(left-curly-arrow right-curly-arrow)))

(use-package smtpmail
  ;; store password using secret-tool as follows:
  ;; secret-tool store --label='Canonical SMTP' host smtp.canonical.com port 587 user canonical
  ;; then enter PASSWORD
  :config
  (setq smtpmail-smtp-user "canonical")
  (setq smtpmail-smtp-server "smtp.canonical.com")
  (setq smtpmail-smtp-service 587))

(use-package solarized-theme
  :ensure t
  :disabled t
  :config
  ;; settings for moody package
  (let ((line (face-attribute 'mode-line :underline)))
    (set-face-attribute 'mode-line          nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :underline  line)
    (set-face-attribute 'mode-line          nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :background "#f9f2d9"))
  (setq x-underline-at-descent-line t)
  (setq solarized-distinct-fringe-background t)
  (setq solarized-scale-org-headlines nil)
  (setq solarized-use-variable-pitch nil)
  (load-theme 'solarized-light t))

(use-package sudo-edit
  :ensure t
  :commands (sudo-edit))

(use-package suggest
  :ensure t)

(use-package swiper
  :ensure t
  :bind (("C-s" . swiper-isearch)
         ("C-r" . swiper-isearch-backward)
         ("M-s o" . swiper-isearch-thing-at-point)
         ("M-s ." . swiper-isearch-thing-at-point)
         :map isearch-mode-map ("M-i" . swiper-from-isearch)))

(use-package systemd
  :ensure t)

(use-package telega
  :ensure t
  :bind (("C-c t" . telega))
  :config
  (telega-notifications-mode 1)
  (if (not (file-exists-p "/usr/local/lib/libtdcore.a"))
      (alert "Pleasie compile and install libtd for telegram support")
    ;; ensure library can be found
    (setenv "LD_LIBRARY_PATH" (concat (getenv "LD_LIBRARY_PATH") ":/usr/local/lib"))
    ;; automatically connect on startup
    (telega 1)))

(use-package text-mode
  ;; enable auto-fill-mode in all text-mode and derived buffers
  :config (toggle-text-mode-auto-fill))

(use-package time
  :after erc-hl-nicks
  :bind ((("C-c z" . display-time-world)))
  :config (let ((team '(("Australia/Adelaide" . ("amurray"))
                        ("Europe/London" . ("ChrisCoulson"))
                        ("Brazil/East" . ("ebarretto" "emitorino" "leosilva" "pfsmorigo"))
                        ("US/Eastern" . ("dan" "markmorlino" "msalvatore" "sid" "vineetha1"))
                        ("Canada/Eastern" . ("Avital" "mdeslaur"))
                        ("US/Central" . ("jdstrand" "jmbl" "tyhicks"))
                        ("US/Mountain" . ("joe"))
                        ("US/Pacific" . ("jj" "sarnold" "sbeattie")))))
            ;; validate team
            (dolist (member team)
              (unless (file-exists-p (expand-file-name (car member) "/usr/share/zoneinfo"))
                (user-error "TZ %s does not exist!" (car member))))
            (setq zoneinfo-style-world-list
                  ;; make nicks stand out
                  (mapcar #'(lambda (member)
                              (list (car member)
                                    (mapconcat #'(lambda (nick)
                                                   (propertize nick
                                                               'face
                                                               (erc-hl-nicks-make-face nick)))
                                               (cdr member)
                                               ", ")))
                          team)))
  (setq display-time-default-load-average nil)
  (setq display-time-use-mail-icon t)
  (setq display-time-day-and-date t)
  (setq display-time-24hr-format t)
  (display-time))

(use-package tramp
  :ensure t
  :config (setq-default tramp-default-method "ssh"))

(use-package uct
  :load-path "~/ubuntu/git/ubuntu-cve-tracker/scripts/"
  :defer nil
  :bind (("C-c u" . uct)
         ("C-c f c" . uct-find-cve)))

(use-package undo-propose
  :ensure t
  :bind (("C-x u" . undo-propose)))

(use-package uniquify
  :config (setq uniquify-buffer-name-style 'post-forward
                uniquify-separator ":"
                uniquify-after-kill-buffer-p t
                uniquify-ignore-buffers-re "^\\*"))

(use-package usn
  :load-path "~/ubuntu/git/usn-tool/"
  :commands (usn-compose-from-draft)
  :bind (("C-c f u" . usn-show-at-point)))

(use-package web-mode
  :ensure t
  :defer t
  :mode ("\\.php\\'" . web-mode))

(use-package vdiff
  :ensure t
  :config (define-key vdiff-mode-map (kbd "C-c") vdiff-mode-prefix-map))

(use-package vdiff-magit
  :ensure t
  :bind (:map magit-mode-map
              ("e" . vdiff-magit-dwim)
              ("E" . vdiff-magit))
  :config
  (transient-suffix-put 'magit-dispatch "e" :description "vdiff (dwim)")
  (transient-suffix-put 'magit-dispatch "e" :command 'vdiff-magit-dwim)
  (transient-suffix-put 'magit-dispatch "E" :description "vdiff")
  (transient-suffix-put 'magit-dispatch "E" :command 'vdiff-magit))

(use-package vimrc-mode
  :ensure t
  :mode (("\\.vim\\(rc\\)?\\'" . vimrc-mode)))

(use-package webpaste
  :ensure t
  :bind (("C-c C-p C-b" . webpaste-paste-buffer)
         ("C-c C-p C-r" . webpaste-paste-region))
  :config
  (add-to-list 'webpaste-providers-alist
               `("pastebin.ubuntu.com"
                 :uri "https://pastebin.ubuntu.com/"
                 ;; poster cannot be empty string and expiry in 1 day
                 :post-data (("poster" . ,user-full-name)
                             ("expiration" . "day"))
                 :post-field "content"
                 :post-lang-field-name "syntax"
                 ;; should this be common-lisp perhaps?
                 :lang-overrides ((emacs-lisp-mode . "clojure"))
                 :success-lambda webpaste--providers-success-response-url))
  (setq webpaste-provider-priority '("pastebin.ubuntu.com")))

(use-package which-func
  :config (which-function-mode 1))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config (which-key-mode))

(use-package whitespace
  :diminish whitespace-mode
  :hook ((prog-mode . whitespace-mode))
  :init
  ;; show trailing whitespace
  (setq-default whitespace-style '(face tabs trailing)))

(use-package windmove
  :ensure t
  :config
  (windmove-default-keybindings 'shift)
  ;; wrap around at edges
  (setq windmove-wrap-around t))

(use-package world-time-mode
  :ensure t)

(use-package wttrin
  :load-path "~/emacs-wttrin/"
  :bind (("C-c w" . wttrin))
  :config
  (setq wttrin-api-version 2)
  (setq wttrin-default-cities '("Adelaide")))

(use-package yaml-mode
  :ensure t)

(use-package yasnippet
  :ensure t
  :diminish (yas-global-mode yas-minor-mode)
  :config (yas-global-mode 1))

(use-package x86-lookup
  :ensure t
  :init (unless (file-exists-p "~/Documents/325383-sdm-vol-2abcd.pdf")
          (alert "Downloading Intel x86 instruction set to ~/Documents")
          (with-demoted-errors "Error downloading x86-lookup document: %s"
            (url-copy-file
             "https://software.intel.com/sites/default/files/managed/a4/60/325383-sdm-vol-2abcd.pdf"
             (expand-file-name "~/Documents/325383-sdm-vol-2abcd.pdf"))))
  :config (setq x86-lookup-pdf "~/Documents/325383-sdm-vol-2abcd.pdf")
  :bind ("C-h x" . x86-lookup))

(use-package znc
  :ensure t
  :preface
  (defvar apm-znc-slugs '(oftc freenode canonical))
  (defvar apm-znc-server "znc.secret.server")
  (defvar apm-znc-username "amurray")
  (defvar apm-znc-port 7076)
  (defvar apm-znc-password (auth-source-pick-first-password
                            :user apm-znc-username
                            :port (format "%d" apm-znc-port)))

  (defun apm-znc-generate-server (slug)
    "Generate a ZNC server entry for SLUG."
    (list slug
          (format "%s/%s" apm-znc-username slug)
          apm-znc-password))

  (defvar apm-znc-servers (list
                           (list apm-znc-server apm-znc-port t
                                 (mapcar #'apm-znc-generate-server
                                         apm-znc-slugs))))

  (defun apm-znc-all (&optional disconnect)
    "Smarter `znc-all'."
    (interactive)
    ;; make sure we don't get called a second time automatically
    (dolist (hook '(after-make-frame-functions after-init-hook))
      (remove-hook hook #'apm-znc-all))
    (when (y-or-n-p "Connect to IRC? ")
      (znc-all disconnect)))

  :hook ((after-init . apm-znc-all))
  :config
  (unless (url-gateway-nslookup-host apm-znc-server)
    ;; need to ensure /etc/hosts points znc.secret.server to
    ;; the correct hostname
    (alert "Please correctly define znc.secret.server in /etc/hosts"))
  (unless apm-znc-password
    ;; secret-tool store --label=ZNC user amurray port 7076
    (alert "Please store ZNC password in keyring via secret-tool"))
  (setq znc-servers apm-znc-servers))

;; set gc-cons-threshold back to original value
(add-hook 'emacs-startup-hook #'apm-set-gc-threshold)

(provide 'init)

;;; init.el ends here
