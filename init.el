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

;; fix recursive load *.el.gz issue with emacs-snapshot -
;; https://github.com/purcell/emacs.d/issues/340#issuecomment-237177032
(setq load-file-rep-suffixes '(""))

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

;; must be set before loading use-package
(setq use-package-enable-imenu-support t)
;; uncomment to debug package loading times
;; (setq use-package-verbose t)


;; Bootstrap `use-package' from melpa
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package bind-key
  :ensure t)

(use-package system-packages
  :ensure t
  :config (setq system-packages-use-sudo t))

(use-package use-package-ensure-system-package
  :ensure t)

;; customisations
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;; load custom but ignore error if doesn't exist
(load custom-file t)

;; load early so we can ensure evil-want-integration is nil
(use-package evil
  :ensure t
  :preface
  (defun apm-make-underscore-word-character ()
    "Make _ a word character."
    (modify-syntax-entry ?_ "w"))
  ;; use evil-collection instead
  :init (setq evil-want-integration nil)
  ;; make underscore a word character so movements across words
  ;; include it - this is the same as vim - need to do it on each
  ;; major mode change
  :hook ((after-change-major-mode . apm-make-underscore-word-character))
  :config (progn
            ;; make cursor easier to see
            (setq evil-normal-state-cursor '("#b294bb" box))
            (setq evil-insert-state-cursor '("#de935f" bar))
            (setq evil-emacs-state-cursor '("#cc6666" box))

            ;; add vim-like bindings for some nice stuff
            (with-eval-after-load 'flyspell
              (define-key evil-normal-state-map "]s" 'flyspell-goto-next-error)
              ;; taken from spacemacs
              (define-key evil-normal-state-map "[b" 'evil-prev-buffer)
              (define-key evil-normal-state-map "]b" 'evil-next-buffer)
              (define-key evil-normal-state-map "[q" 'previous-error)
              (define-key evil-normal-state-map "]q" 'next-error))

            (define-key evil-ex-map "bd " 'kill-buffer)

            ;; these should be bound automatically but apparently not so rebind
            ;; them
            (bind-keys :map evil-insert-state-map
                       ("C-x C-n" . evil-complete-next-line)
                       ("C-x C-l" . evil-complete-next-line)
                       ("C-x C-p" . evil-complete-previous-line))

            ;; fixup company-complete-number to be handled better with evil
            (evil-declare-change-repeat 'company-complete-number)
            (evil-mode 1)))

(use-package alert
  :ensure t
  :config (when (eq system-type 'gnu/linux)
            (setq alert-default-style 'notifications)))

;; used in some of my yasnippet snippets
(defun apm-camelize (s &optional delim)
  "Convert under_score string S to CamelCase string with optional DELIM."
  (interactive "s")
  (mapconcat 'identity (mapcar
                        #'(lambda (word) (capitalize (downcase word)))
                        (split-string s (if delim delim "_"))) ""))

(defvar apm-preferred-emacs-version "26.1")
(when (version< emacs-version apm-preferred-emacs-version)
  (alert (format "Emacs version too old - please run %s or newer"
                 apm-preferred-emacs-version)
         :severity 'high))


;; minibuffer settings
;; automatically garbage collect when switch away from emacs
(add-hook 'focus-out-hook 'garbage-collect)

;; set high gc limit for minibuffer so doesn't slowdown on helm etc
(defun apm-minibuffer-setup ()
  "Setup minibuffer."
  (setq gc-cons-threshold most-positive-fixnum))

(defun apm-minibuffer-exit ()
  "Undo minibuffer setup."
  (setq gc-cons-threshold (* 64 1024 1024)))

(add-hook 'minibuffer-setup-hook #'apm-minibuffer-setup)
(add-hook 'minibuffer-exit-hook #'apm-minibuffer-exit)

;;; General settings etc

;; personalisation
(setq user-full-name "Alex Murray")

;; enable narrow-to-region
(put 'narrow-to-region 'disabled nil)

;; tabs are never ok
(setq-default indent-tabs-mode nil)

;; set a reasonable fill and comment column
(setq-default fill-column 79)
(setq-default comment-column 78)

;; just use y or n not yes or no
(defalias 'yes-or-no-p 'y-or-n-p)

;; inhibit startup message and splash screen
(setq inhibit-startup-message t)
;; remove message from initial scratch buffer
(setq initial-scratch-message nil)

;; Make Tab complete if the line is indented
(setq tab-always-indent 'complete)

;; disable menu, tool and scroll-bars, show time
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode 0))
(display-time-mode 1)
;; Show line column numbers in mode line
(line-number-mode 1)
(column-number-mode 1)
;; don't use gtk style tooltips since are intrusive
(setq-default x-gtk-use-system-tooltips nil)
(blink-cursor-mode -1)
(setq frame-title-format
      '((buffer-file-name "%f" (dired-directory dired-directory "%b"))))
(setq icon-title-format frame-title-format)

;; default to maximised windows
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; prompt when trying to switch out of a dedicated window
(setq switch-to-buffer-in-dedicated-window 'prompt)

;; ensure scrolling forwards / backwards preserves original location such that
;; they undo each other
(setq scroll-preserve-screen-position 'always)

(defvar apm-preferred-font-name "Dejavu Sans Mono"
  "Preferred font to use.")

(defvar apm-preferred-font-package "fonts-dejavu-core"
  "Package to install to get `apm-preferred-font-name'.")

(defvar apm-preferred-font-height 10
  "Preferred font height to use.")

(defun apm-graphic-frame-init (&optional frame)
  "Initialise properties specific to graphical display for FRAME."
  (interactive)
  (with-selected-frame frame
    (remove-hook 'after-make-frame-functions #'apm-graphic-frame-init)
    (if (font-info apm-preferred-font-name)
        (set-frame-font (format "%s-%d"
                                apm-preferred-font-name
                                apm-preferred-font-height)
                        nil (list frame))
      (system-packages-install apm-preferred-font-package))))

(if (daemonp)
    ;; make sure graphical properties get set on client frames
    (add-hook 'after-make-frame-functions #'apm-graphic-frame-init)
  (apm-graphic-frame-init (selected-frame)))

;; Use regex searches and replace by default.
(bind-key "C-s" 'isearch-forward-regexp)
(bind-key "C-r" 'isearch-backward-regexp)
(bind-key "M-%" 'query-replace-regexp)
(bind-key "C-M-s" 'isearch-forward)
(bind-key "C-M-r" 'isearch-backward)
(bind-key "C-M-%" 'query-replace)

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
  :config (progn
            (setq save-abbrevs t)
            (setq-default abbrev-mode t)))

(use-package ace-window
  :ensure t
  :defer t
  :bind (("C-x o" . ace-window)))

(use-package adaptive-wrap
  :ensure t
  :defer t
  :hook ((text-mode . adaptive-wrap-prefix-mode)))

(use-package aggressive-indent
  :ensure t
  :defer t
  :config (global-aggressive-indent-mode))

(use-package ag
  :ensure t
  :defer t
  :ensure-system-package (ag . silversearcher-ag))

(use-package all-the-icons
  :ensure t
  :config (unless (file-exists-p (expand-file-name "~/.local/share/fonts/all-the-icons.ttf"))
            (all-the-icons-install-fonts)))

(use-package android-mode
  :ensure t
  :defer t
  :commands android-mode
  :hook java-mode
  :init (progn
          (setq android-mode-sdk-dir (expand-file-name "~/android-sdk-linux/"))
          ;; change prefix so doesn't conflict with comment-region
          (setq android-mode-key-prefix (kbd "C-c C-m"))))

(use-package ansi-color
  ;; show colours correctly in shell
  :config (ansi-color-for-comint-mode-on))

(use-package anzu
  :ensure t
  :init (global-anzu-mode)
  :bind (("M-%" . anzu-query-replace-regexp)
         ("C-M-%" . anzu-query-replace)))

(use-package apm-c
  :load-path "lisp/")

(use-package apm-misc
  :load-path "lisp/")

(use-package appt
  :preface
  (defun apm-appt-notify (time-to-appt time msg)
    "Notify for appointment at TIME-TO-APPT TIME MSG alert."
    (alert msg
           :title (format "Appointment in %s minutes [%s]" time-to-appt time)
           :icon "/usr/share/icons/gnome/32x32/status/appointment-soon.png"))
  :config (progn
            (setq appt-disp-window-function #'apm-appt-notify)
            (appt-activate 1)))

(use-package apropos
  :bind ("C-h a" . apropos))


(use-package asn1-mode
  :ensure t
  :defer t
  :mode (("\\.asn1?$" . asn1-mode)))

(use-package autorevert
  :init (global-auto-revert-mode 1))

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
  :init (progn
          (setq-default TeX-auto-save t)
          (setq-default TeX-parse-self t)
          (setq-default TeX-PDF-mode t)
          (setq-default TeX-master nil)
          (setq-default reftex-plug-into-AUCTeX t)
          (setq-default TeX-source-correlate-start-server t)))

(use-package auth-source
  ;; prefer synced, encrypted auth source to non-encrypted
  :init (setq auth-sources '("~/Dropbox/.authinfo.gpg" "~/.authinfo.gpg" "~/.authinfo" "~/.netrc")))

(use-package avy
  :ensure t
  :defer t
  :config (progn
            ;; dim text when avy is active
            (setq avy-background t)
            ;; insert chars infront of target rather than over target
            (setq avy-style 'pre)))

(use-package beginend
  :ensure t
  :config (beginend-global-mode 1))

(use-package browse-kill-ring
  :ensure t)

(use-package bs
  :bind ("C-x C-b" . bs-show))

(use-package bug-reference
  :defer t
  :hook ((prog-mode . bug-reference-prog-mode))
  :init (progn
          (eval-when-compile
            (require 'bug-reference))
          (setq bug-reference-url-format "https://bugs.launchpad.net/bugs/%s"
                bug-reference-bug-regexp "\\(LP ?#?:?\\)\\([0-9]+\\(?:#[0-9]+\\)?\\)")))

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
    ;; turn on auto-newline and hungry-delete
    (c-toggle-auto-hungry-state 1)
    ;; turn on electric indent
    (c-toggle-electric-state 1)
    ;; ensure fill-paragraph takes doxygen @ markers as start of new
    ;; paragraphs properly
    (setq paragraph-start "^[ ]*\\(//+\\|\\**\\)[ ]*\\([ ]*$\\|@param\\)\\|^\f")
    ;; add key-bindings for smartparens hybrid sexps
    (with-eval-after-load 'smartparens
      (local-set-key (kbd "C-)") 'sp-slurp-hybrid-sexp)
      (local-set-key (kbd "C-<right>") 'sp-slurp-hybrid-sexp)
      (local-set-key (kbd "C-<left>") 'sp-dedent-adjust-sexp)))
  :hook ((c-mode-common . apm-c-mode-common-setup)))

(use-package apm-c
  :load-path "lisp/"
  :preface
  (defun apm-c-mode-setup ()
    "Tweaks and customisations for `c-mode'."
    (c-set-style "linux")
    ;; and treat linux style as safe for local variable
    (add-to-list 'safe-local-variable-values '(c-indentation-style . linux))
    ;; ensure fill-paragraph takes doxygen @ markers as start of new
    ;; paragraphs properly
    (setq paragraph-start "^[ ]*\\(//+\\|\\**\\)[ ]*\\([ ]*$\\|@param\\)\\|^\f"))
  :hook ((c-mode c++-mode) . apm-c-mode-setup))

(use-package company
  :ensure t
  ;; Use Company for completion
  :bind (("C-;" . company-complete-common)
         :map company-mode-map
         ([remap completion-at-point] . company-complete-common)
         ([remap complete-symbol] . company-complete-common))
  :init (progn
          ;; set default lighter as nothing so in general it is not displayed
          ;; but will still be shown when completion popup is active to show the
          ;; backend which is in use
          (setq company-lighter-base "")
          (global-company-mode 1))
  :config (progn
            ;; ensure flyspell doesn't steal our binding of C-;
            (eval-when-compile
              (require 'flyspell))
            (with-eval-after-load 'flyspell
              (bind-key "C-;" nil flyspell-mode-map))
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
            (setq company-transformers '(company-sort-by-occurrence))
            ;; try tab style behaviour
            (company-tng-configure-default)))

(use-package company-anaconda
  :ensure t
  :defer t
  :after company
  :init (add-to-list 'company-backends #'company-anaconda))

(use-package company-auctex
  :ensure t
  :defer t
  :hook ((LaTeX-mode . company-auctex-init)))

(use-package company-dabbrev
  :after company
  ;; keep original case
  :config (setq company-dabbrev-downcase nil))

(use-package company-lsp
  :ensure t
  :after company
  :init (add-to-list 'company-backends 'company-lsp))

(use-package company-math
  :ensure t
  :defer t
  :after company
  ;; Add backend for math characters
  :init (progn
          (add-to-list 'company-backends 'company-math-symbols-unicode)
          (add-to-list 'company-backends 'company-math-symbols-latex)))

(use-package company-quickhelp
  :ensure t
  :defer t
  :after company
  :hook ((company-mode . company-quickhelp-mode))
  :config (setq company-quickhelp-delay 0.1))

(use-package company-reftex             ; Backends for RefTeX
  :ensure t
  :after company
  :init (progn
          (add-to-list 'company-backends 'company-reftex-labels)
          (add-to-list 'company-backends 'company-reftex-citations)))

(use-package company-shell
  :ensure t
  :defer t
  :after company
  :init (add-to-list 'company-backends 'company-shell))

(use-package company-prescient
  :ensure t
  :after (company prescient)
  :config (company-prescient-mode 1))

(use-package company-tracwiki
  :load-path "vendor/"
  :disabled t
  :after company
  :commands company-tracwiki
  :init (add-to-list 'company-backends 'company-tracwiki))

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
  :after ivy
  :defer t
  :bind (("M-x" . counsel-M-x)
         ("M-y" . counsel-yank-pop)
         ("C-x C-f" . counsel-find-file)
         ("C-x C-r" . counsel-recentf)
         :map company-active-map ("C-/" . counsel-company))
  :config (with-eval-after-load 'evil
            (eval-when-compile (require 'evil))
            (define-key evil-ex-map "e " 'counsel-find-file)
            (evil-ex-define-cmd "ap[ropos]" 'counsel-apropos)
            (define-key evil-ex-map "ap " 'counsel-apropos)))

(use-package counsel-notmuch
  :ensure t
  :disabled t
  :after (counsel notmuch)
  :commands counsel-notmuch
  :config (evil-define-key 'normal notmuch-common-keymap (kbd "s") #'counsel-notmuch))

(use-package counsel-projectile
  :ensure t
  :after (counsel projectile)
  :config (progn
            (setq projectile-switch-project-action 'counsel-projectile)
            (counsel-projectile-mode)))

(use-package cov
  :ensure t
  :preface
  (defun apm-cov-mode-setup ()
    "Setup cov-mode."
    (make-local-variable 'cov-coverage-file-paths))
  :defer t
  :hook ((c-mode-common . cov-mode)
         (cov-mode . apm-cov-mode-setup)))


(use-package cquery
  :ensure t
  :preface
  ;; autogenerate a .cquery if there is an associated .cquery.in
  (defun apm-autogenerate-cquery ()
    "Autogenerate a .cquery if needed when opening a project."
    (when (and (fboundp 'projectile-project-root)
               ;; handle if not in project by returning nil
               (not (null (condition-case nil
                              (projectile-project-root)
                            (error nil))))
               (cl-every #'identity (mapcar #'(lambda (f)
                                                (file-exists-p (concat
                                                                (file-name-as-directory
                                                                 (projectile-project-root))
                                                                f)))
                                            '(".cquery.in" "Makefile"))))
      (projectile-with-default-dir (projectile-project-root)
        (shell-command "make .cquery"))))

  (defvar apm-cquery-executable
    (expand-file-name "~/cquery/build/release/bin/cquery"))
  :defer t
  :commands lsp-cquery-enable
  :hook ((c-mode c++-mode) .  lsp-cquery-enable)
  :init (progn
          (advice-add 'lsp-cquery-enable :before #'apm-autogenerate-cquery)
          (unless (file-exists-p apm-cquery-executable)
            (alert (format "cquery not found at %s - see https://github.com/jacobdufault/cquery/wiki/Getting-started"
                           apm-cquery-executable))))
  :config (progn
            ;; do both Doxygen comment (1) and normal comments (2) and use
            ;; msgpack instead of json for more compact cache
            (setq cquery-extra-init-params '(:index (:comments 2) :cacheFormat "msgpack"))
            (setq cquery-executable apm-cquery-executable)))

(use-package crux
  :ensure t
  :bind (([remap move-beginning-of-line] . crux-move-beginning-of-line)
         ("C-c o" . crux-open-with)))

(use-package cstyle
  :disabled t
  :load-path "vendor/")

(use-package cua-base
  ;; use CUA mode for rectangle selections etc but not copy/paste etc
  :init (cua-selection-mode 1))

;; show suspicious c constructs automatically
(use-package cwarn
  :init (global-cwarn-mode 1))

(use-package debian-changelog-mode
  :ensure t)

(use-package delsel
  ;; enable delete-selection mode to allow replacing selected region
  ;; with new text automatically
  :init (delete-selection-mode 1))

(use-package diff
  ;; default to unified diff
  :config (setq diff-switches "-u"))

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
  ;; defer since is bound via evil-leader
  :defer t)

(use-package dts-mode
  :ensure t)

;; taken from http://kaushalmodi.github.io/2015/03/09/do-ediff-as-i-mean/
(defun apm-ediff-dwim ()
  "Do ediff as I mean.

If a region is active when command is called, call `ediff-regions-wordwise'.

Else if the current frame has 2 windows,
- Do `ediff-files' if the buffers are associated to files and the buffers
  have not been modified.
- Do `ediff-buffers' otherwise.

Otherwise call `ediff-buffers' interactively."
  (interactive)
  (if (region-active-p)
      (call-interactively 'ediff-regions-wordwise)
    (if (= 2 (safe-length (window-list)))
        (let (bufa bufb filea fileb)
          (setq bufa  (get-buffer (buffer-name)))
          (setq filea (buffer-file-name bufa))
          (save-excursion
            (other-window 1)
            (setq bufb (get-buffer (buffer-name))))
          (setq fileb (buffer-file-name bufb))
          (if (or
               ;; if either of the buffers is not associated to a file
               (null filea) (null fileb)
               ;; if either of the buffers is modified
               (buffer-modified-p bufa) (buffer-modified-p bufb))
              (progn
                (message "Running (ediff-buffers \"%s\" \"%s\") .." bufa bufb)
                (ediff-buffers bufa bufb))
            (progn
              (message "Running (ediff-files \"%s\" \"%s\") .." filea fileb)
              (ediff-files filea fileb))))
      (call-interactively 'ediff-buffers))))

(use-package ediff
  :defer t
  :config (setq ediff-window-setup-function 'ediff-setup-windows-plain
                ediff-split-window-function 'split-window-horizontally))

(use-package eldoc
  :config (global-eldoc-mode 1))

(use-package elf-mode
  :ensure t)

(use-package electric
  :init (progn
          ;; electric indent and layout modes to make more IDE like
          (electric-indent-mode 1)
          (electric-layout-mode 1)))

(use-package elisp-slime-nav
  :ensure t
  :defer t
  :after evil
  :hook ((emacs-lisp-mode ielm-mode) . elisp-slime-nav-mode)
  :config (progn
            (evil-define-key 'normal elisp-slime-nav-mode-map (kbd "C-]")
              #'elisp-slime-nav-find-elisp-thing-at-point)
            (evil-define-key 'visual elisp-slime-nav-mode-map (kbd "C-]")
              #'elisp-slime-nav-find-elisp-thing-at-point)
            (evil-define-key 'normal elisp-slime-nav-mode-map (kbd "C-t")
              #'pop-tag-mark)))

(use-package emojify
  :ensure t
  :config (progn
            ;; only show unicode emojis
            (setq emojify-display-style 'unicode)
            (setq emojify-emoji-styles '(unicode))
            ;; uncover emoji when point is over them
            (setq emojify-point-entered-behaviour 'uncover)
            (global-emojify-mode 1)))

(use-package erc
  :ensure t
  :preface (defun apm-launch-erc ()
             (erc :server "irc.freenode.net")
             (erc-tls :server "irc.canonical.com" :port 6697)
             (erc-tls :server "irc.oftc.net" :port 6697))
  ;; autoconnect at startup
  :hook (after-init . apm-launch-erc)
  :config (progn
            (eval-and-compile
              (require 'erc-join)
              (require 'erc-log)
              (require 'erc-networks)
              (require 'erc-services))
            ;; canonical irc
            (add-to-list 'erc-networks-alist '(Canonical "canonical.com"))
            (add-to-list 'erc-server-alist '("Canonical IRC" 'Canonical "irc.canonical.com" 6697))
            (setq erc-nick "amurray")
            (setq erc-autojoin-channels-alist '(("freenode.net"
                                                 "#emacs"
                                                 "#oss-security"
                                                 "#snappy"
                                                 "#ubuntu-app-devel"
                                                 "#ubuntu-ci-eng"
                                                 "#ubuntu-devel"
                                                 "#ubuntu-hardened"
                                                 "#ubuntu-kernel"
                                                 "#ubuntu-meeting"
                                                 "#ubuntu-release"
                                                 "#ubuntu-server")
                                                ("oftc.net"
                                                 "#apparmor"
                                                 "#debian-security")
                                                ("canonical.com"
                                                 "#canonical"
                                                 "#distro"
                                                 "#security"
                                                 "#security-private"
                                                 "#snappy-internal"
                                                 "#server"
                                                 "#FIPS"
                                                 "#is")))
            (setq erc-hide-list '("JOIN" "PART" "QUIT"))
            (setq erc-fill-function #'erc-fill-static)
            (setq erc-fill-static-center 22)

            ;; use sensible buffer names with server as well
            (setq erc-rename-buffers t)

            (add-to-list 'erc-modules 'spelling)
            (add-to-list 'erc-modules 'log)
            (erc-update-modules)

            (setq erc-pals '("ratliff" "jdstrand" "mdeslaur" "jjohansen" "ChrisCoulson" "sarnold" "sbeattie" "tyhicks" "leosilva"))
            (setq erc-keywords '("[Cc][Vv][Ee]"))

            (setq erc-log-channels-directory "~/.emacs.d/erc/logs")
            (setq erc-log-insert-log-on-open t)
            (setq erc-log-file-coding-system 'utf-8)
            (setq erc-log-write-after-send t)
            (setq erc-log-write-after-insert t)
            (setq erc-save-buffer-on-part t)

            (unless (file-exists-p erc-log-channels-directory)
              (mkdir erc-log-channels-directory t))

            (erc-autojoin-mode 1)))

(use-package erc-hl-nicks
  :ensure t
  :after erc)

(use-package erc-image
  :ensure t
  :after erc
  :config (progn
            (add-to-list 'erc-modules 'image)
            (erc-update-modules)))

(use-package ercn
  :ensure t
  :preface
  (defun apm-ercn-notify (nickname message)
    "Displays a notification message for ERC for NICKNAME with MESSAGE."
    (let* ((channel (buffer-name))
           (nick (erc-hl-nicks-trim-irc-nick nickname))
           (title (if (string-match-p (concat "^" nickname) channel)
                      nick
                    (concat nick " (" channel ")")))
           (msg (s-trim (s-collapse-whitespace message))))
      (alert (concat nick ": " msg) :title title)))
  ;; notify via alert when mentioned
  :hook ((ercn-notify . apm-ercn-notify))
  ;; be notified when mentioned or pals talk in given channels or
  ;; finally if in private chat
  :config (setq ercn-notify-rules
                '((current-nick . all)
                  (pal . ("#security" "#security-private"))
                  (query-buffer . all))))

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
  ;; disable ethan-wspace caring about tabs in Makefile's
  :hook ((makefile-mode . makefile-tabs-are-less-evil))
  ;; ethan-wspace-mode raises lots of warnings if this is enabled...
  ;; hopefully this doesn't cause problems
  :config  (setq mode-require-final-newline nil)
  :init (global-ethan-wspace-mode 1))

(use-package evil-collection
  :ensure t
  :after evil
  :config (evil-collection-init))

(use-package evil-anzu
  :ensure t)

(use-package evil-args
  :ensure t
  :defer t
  :init (progn
          ;; bind evil-args text objects
          (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
          (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)

          ;; bind evil-forward/backward-args
          (define-key evil-normal-state-map "L" 'evil-forward-arg)
          (define-key evil-normal-state-map "H" 'evil-backward-arg)
          (define-key evil-motion-state-map "L" 'evil-forward-arg)
          (define-key evil-motion-state-map "H" 'evil-backward-arg)))

(use-package evil-commentary
  :ensure t
  :config (evil-commentary-mode 1))

(use-package evil-expat
  :ensure t)

(use-package evil-goggles
  :ensure t
  :config (progn
            (setq evil-goggles-duration 0.1)
            (evil-goggles-mode 1)
            (evil-goggles-use-diff-faces)))

(use-package evil-leader
  :ensure t
  :config (progn
            (setq evil-leader/leader "<SPC>"
                  evil-leader/in-all-states t)
            (evil-leader/set-key
              "SPC" 'avy-goto-word-or-subword-1
              "l" 'avy-goto-line
              "c" 'avy-goto-char-timer
              "a" 'counsel-ag
              "b" 'ivy-switch-buffer
              "df" 'doxyas-document-function
              "e" 'eshell
              "fc" 'flycheck-buffer
              "ff" 'counsel-find-file
              "ge" 'google-error
              "gg" 'counsel-git-grep
              "go" 'google-this
              "i" 'counsel-imenu
              "k" 'kill-buffer
              "mg" 'magit-status
              "mm" 'magit-dispatch-popup
              "oa" 'org-agenda
              "ob" 'org-ido-switchb
              "oca" 'org-capture
              "occ" 'org-clock-cancel
              "ocd" 'org-clock-display
              "ocg" 'org-clock-goto
              "oci" 'org-clock-in
              "oco" 'org-clock-out
              "ocs" 'org-mru-clock-in
              "ocu" 'org-clock-update-time-maybe
              "oo" 'counsel-org-goto
              "ot" 'org-todo-list
              "P" 'counsel-projectile-switch-project
              "pa" 'counsel-projectile-ag
              "pb" 'counsel-projectile-switch-to-buffer
              "pc" 'counsel-projectile
              "pe" 'projectile-run-eshell
              "pd" 'counsel-projectile-find-dir
              "pD" 'projectile-find-dir-other-window
              "pf" 'counsel-projectile-find-file
              "pF" 'projectile-find-file-other-window
              "ph" 'counsel-projectile
              "pk" 'projectile-kill-buffers
              "pm" 'helm-make-projectile
              "po" 'projectile-find-other-file
              "pp" 'counsel-projectile
              "pr" 'projectile-recentf
              "r" 'counsel-recentf
              "s" 'counsel-grep-or-swiper
              "u" 'counsel-unicode-char
              "v" 'er/expand-region
              "x" 'counsel-M-x))
  :init (global-evil-leader-mode 1))

(use-package evil-magit
  :ensure t)

(use-package evil-matchit
  :ensure t
  :config (global-evil-matchit-mode 1))

(use-package evil-numbers
  :ensure t
  :bind (("C-c +" . evil-numbers/inc-at-pt)
         ("C-c -" . evil-numbers/dec-at-pt)))

(use-package evil-smartparens
  :ensure t
  :defer t
  ;; only use with strict smartparens otherwise is too annoying for normal cases
  :hook ((smartparens-strict-mode . evil-smartparens-mode)))

(use-package evil-surround
  :ensure t
  :init (global-evil-surround-mode 1))

(use-package evil-textobj-anyblock
  :ensure t
  :bind ((:map evil-inner-text-objects-map ("b" . 'evil-textobj-anyblock-inner-block)
               :map evil-outer-text-objects-map ("b" . 'evil-textobj-anyblock-a-block))))

(use-package evil-visualstar
  :ensure t
  :config (global-evil-visualstar-mode 1))

(use-package exec-path-from-shell
  :ensure t
  :init (when (memq window-system '(mac ns))
          (exec-path-from-shell-initialize)))

(use-package expand-region
  :ensure t
  :config (setq expand-region-contract-fast-key "V"
                expand-region-reset-fast-key "r"))

(use-package fancy-battery
  :ensure t
  :config (fancy-battery-mode 1))

(use-package files
  :bind ("C-c r" . revert-buffer))

(use-package flycheck
  :ensure t
  :preface
  (defun apm-flycheck-setup ()
    "Setup flycheck."
    (define-key evil-normal-state-map "[e" 'flycheck-previous-error)
    (define-key evil-normal-state-map "]e" 'flycheck-next-error))
  :commands flycheck-add-next-checker
  :hook ((flycheck-mode . apm-flycheck-setup))
  :ensure-system-package (cppcheck shellcheck)
  :init (setq-default flycheck-emacs-lisp-load-path 'inherit)
  :config (progn
            ;; use lsp-ui checker via cquery instead
            (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-gcc))
            (setq-default flycheck-display-errors-delay 0.2)
            (global-flycheck-mode 1)))

(use-package flycheck-checkbashisms
  :ensure t
  :after flycheck
  :ensure-system-package (checkbashisms . devscripts)
  :config (flycheck-checkbashisms-setup))

(use-package flycheck-clang-analyzer
  :ensure t
  :after flycheck-cstyle
  :ensure-system-package clang
  :config (progn
            (setq flycheck-clang-analyzer-executable "clang")
            (flycheck-clang-analyzer-setup)
            ;; automatically sets itself up as next checker after lsp-ui so undo
            ;; that so is instead after cppcheck
            (delete '(warning . clang-analyzer)
                    (flycheck-checker-get 'lsp-ui 'next-checkers))
            (flycheck-add-next-checker 'c/c++-cppcheck '(t . clang-analyzer))))

(use-package flycheck-coverity
  :ensure t
  :after flycheck-clang-analyzer
  :init (unless (executable-find "cov-run-desktop")
          (alert "cov-run-desktop not found - is it installed?"))
  :config (progn
            (flycheck-coverity-setup)
            (flycheck-add-next-checker 'clang-analyzer '(t . coverity))))

(use-package flycheck-color-mode-line
  :ensure t
  :after flycheck
  :defer t
  :hook ((flycheck-mode . flycheck-color-mode-line-mode)))

(use-package flycheck-cstyle
  :ensure t
  :after lsp-ui
  :init (unless (executable-find "cstyle")
          (alert "cstyle not found - is it installed?"))
  :config (progn
            (flycheck-cstyle-setup)
            (flycheck-add-next-checker 'lsp-ui '(warning . cstyle))
            (flycheck-add-next-checker 'cstyle '(t . c/c++-cppcheck))))

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
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode)))

(use-package flyspell-correct-ivy
  :ensure t
  ;; use instead of ispell-word which evil binds to z=
  :bind (([remap ispell-word] . flyspell-correct-word-generic)
         :map flyspell-mode-map ("C-;" . flyspell-correct-previous-word-generic)))

(use-package fuzzy
  :ensure t)

(use-package gdb-mi
  :defer t
  :init (progn
          ;; use gdb-many-windows by default
          (setq-default gdb-many-windows t)
          ;; Non-nil means display source file containing the main routine at startup
          (setq-default gdb-show-main t)))

(use-package gif-screencast
  :ensure t
  :ensure-system-package (scrot gifsicle))

(use-package gitconfig-mode
  :ensure t
  :defer t)

(use-package gitignore-mode
  :ensure t
  :defer t)

(use-package gnuplot
  :ensure t)

(use-package google-this
  :ensure t
  :defer t)

(use-package goto-addr
  :defer t
  :hook ((prog-mode . goto-address-prog-mode)))

(use-package gud
  :defer t
  :hook ((gud-mode . gud-tooltip-mode)))

;; we use ivy instead
(use-package helm-make
  :ensure t
  :bind (("C-x C-m" . helm-make-projectile))
  :config (setq helm-make-completion-method 'ivy))


(use-package helpful
  :ensure t
  :bind (("C-h a" . helpful-symbol)
         ("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)))

(use-package hl-todo
  :ensure t
  :defer t
  :functions hl-todo-mode
  :hook ((prog-mode . hl-todo-mode))
  :config (add-to-list 'hl-todo-keyword-faces '("@todo" . "#cc9393")))

(use-package hungry-delete
  :ensure t
  :config (global-hungry-delete-mode 1))

(use-package ispell
  :defer t
  :hook ((text-mode . ispell-minor-mode))
  :init (progn
          ;; windows specific config
          (when (eq system-type 'windows-nt)
            (add-to-list 'exec-path "C:/Program Files (x86)/Aspell/bin/"))
          ;; use aspell if can be found
          (when (executable-find "aspell")
            ;; use gb dictionary via aspell if available
            (setq ispell-program-name "aspell"
                  ispell-dictionary "british"
                  ispell-extra-args '("--sug-mode=ultra")))))

(use-package ivy
  :ensure t
  :defer t
  :bind (("C-x b" . ivy-switch-buffer)
         ("C-c C-r" . ivy-resume))
  :config (progn
            (setq ivy-use-virtual-buffers t)
            ;; allow to select the typed in value with C-p
            (setq ivy-use-selectable-prompt t)
            (define-key isearch-mode-map (kbd "M-o") 'ivy-occur)
            (ivy-mode 1)
            (setq ivy-re-builders-alist '((t . ivy--regex-plus)))
            ;; integrate with evil
            (with-eval-after-load 'evil
              (define-key evil-ex-map "b " 'ivy-switch-buffer))))

(use-package ivy-prescient
  :ensure t
  :after (ivy prescient)
  :config (ivy-prescient-mode 1))

(use-package ivy-xref
  :ensure t
  :init (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

(use-package jenkins
  :ensure t
  :commands (jenkins)
  ;; don't set jenkins-api-token here - do it in custom.el so it is not checked
  ;; into git
  :config (setq jenkins-hostname "http://cw-jenkins/jenkins/"
                jenkins-username "amurray"))

(use-package js2-mode
  :ensure t
  :defer t
  :init (setq-default js2-basic-offset 2))


(use-package log-edit
  :defer t
  :preface
  (defun apm-log-edit-insert-yasnippet-template ()
    "Insert the default template with Summary and Author."
    (interactive)
    (when (or (called-interactively-p 'interactive)
              (log-edit-empty-buffer-p))
      (require 'yasnippet)
      (yas-expand-snippet "${1:Summary of this change}

${2:Longer description of this change}

${3:Ticket: #${4:XXXX}}")))
  :init (progn
          (with-eval-after-load 'evil
            (evil-set-initial-state 'log-edit-mode 'insert))
          (add-hook 'log-edit-hook 'apm-log-edit-insert-yasnippet-template)
          (remove-hook 'log-edit-hook 'log-edit-insert-message-template)))

(use-package lsp-mode
  ;; don't use lsp-flycheck since there is lsp-ui now
  :ensure t)

(use-package lsp-imenu
  :ensure lsp-mode
  :hook ((lsp-after-open . lsp-enable-imenu)))

(use-package lsp-java
  :ensure t
  :after lsp-mode
  :hook ((java-mode . lsp-java-enable)))

(use-package lsp-python
  :ensure t
  :after lsp-mode
  :hook ((python-mode . lsp-python-enable))
  :init (unless (executable-find "pyls")
          (alert "pyls not found - pip install python-language-server")))

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :hook ((lsp-mode . lsp-ui-mode))
  :config (progn
            ;; use the original icon since was removed in
            ;; https://github.com/emacs-lsp/lsp-ui/commit/0dff02a1d02f16ab017f2ad7cd4a9913733f48ca
            ;; due to others not having unicode
            (setq lsp-ui-sideline-code-actions-prefix "ℹ ")
            (setq lsp-ui-sideline-show-symbol nil)))

(use-package magit
  :ensure t
  :preface
  (defun apm-magit-mode-setup ()
    "Setup `magit-mode'."
    (setq mode-name ""))
  :defer t
  :bind ("C-x g" . magit-status)
  :hook ((magit-mode . apm-magit-mode-setup))
  :config (setq magit-completing-read-function 'ivy-completing-read))

(use-package magithub
  :ensure t
  :after magit
  :config (magithub-feature-autoinject t))

(use-package make-mode
  ;; don't show major mode name
  :init (add-hook 'makefile-mode-hook #'(lambda () (setq mode-name nil))))

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

(use-package minions
  :ensure t
  :init (minions-mode))

(use-package modern-cpp-font-lock
  :ensure t
  :defer t
  :hook ((c++-mode . modern-c++-font-lock-mode)))

(use-package moody
  :ensure t
  :config (progn
            (setq x-underline-at-descent-line t)
            (setq moody-mode-line-height 24)
            (moody-replace-mode-line-buffer-identification)
            (moody-replace-vc-mode)
            (size-indication-mode)))

(use-package move-text
  :ensure t
  :bind (([(meta shift up)] . move-text-up)
         ([(meta shift down)] . move-text-down)))

(use-package mu4e
  :ensure-system-package ((mu . mu4e))
  :preface
  ;; TODO: consider using imapfilter
  (defun apm-mu4e-refile-message (msg)
    (let ((mailing-list (mu4e-message-field msg :mailing-list))
          (subject (mu4e-message-field msg :subject)))
      (cond
       ((not (null mailing-list))
        (concat "/Lists." (mu4e-get-mailing-list-shortname mailing-list)))
       ((or (string-prefix-p "[Bug " subject)
            (mu4e-message-contact-field-matches msg :from "bugs.launchpad.net"))
        "/launchpad-bugs")
       ((string-prefix-p "[USN-" subject) "/usn")
       ((string-prefix-p "[Merge]" subject) "/merge-requests")
       ((mu4e-message-contact-field-matches msg :to "newsbox@idg.com")
        "/Lists.newsbox-idg")
       ((mu4e-message-contact-field-matches msg :from "root@lillypilly.canonical.com")
        "/lillypilly")
       ((mu4e-message-contact-field-matches msg :from "root@keule.canonical.com")
        "/keule")
       ((mu4e-message-contact-field-matches msg :from "atpi.com")
        "/Travel")
       ((or (mu4e-message-contact-field-matches msg :from "rt@admin.canonical.com")
            (mu4e-message-contact-field-matches msg :to "rt@admin.canonical.com"))
        "/canonical-is")
       (t "/Archive"))) )
  :config (progn
            (setq mail-user-agent 'mu4e-user-agent)
            (setq mu4e-maildir "~/Maildir")
            (setq mu4e-sent-folder   "/Sent"
                  mu4e-drafts-folder "/Drafts"
                  mu4e-trash-folder  "/Trash")
            (setq mu4e-maildir-shortcuts
                  '(("/Archive"              . ?a)
                    ("/INBOX"                . ?i)
                    ("/Sent"                 . ?s)
                    ("/Travel"               . ?t)
                    ("/ubuntu-security-team" . ?u)))

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
                                            ("roadmap-sprint.lists.canonical.com" . "roadmap-sprint")

                                            ;; security
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
            ;; only run once and ignore any autorefresh in config
            (setq mu4e-get-mail-command "offlineimap -o -u basic")
            (setq mu4e-update-interval 60)
            (setq mu4e-compose-reply-to-address "alex.murray@canonical.com"
                  mu4e-user-mail-address-list '("alex.murray@canonical.com")
                  user-mail-address "alex.murray@canonical.com"
                  user-full-name  "Alex Murray")
            (setq mu4e-compose-signature
                  "Alex Murray\nhttps://launchpad.net/~alexmurray\n")

            (setq mu4e-completing-read-function 'completing-read)
            ;; show full conversations
            (setq mu4e-headers-include-related t)
            ;; and include the maildir in the headers
            (setq mu4e-headers-fields '((:human-date . 12)
                                        (:flags . 6)
                                        (:mailing-list . 10)
                                        (:maildir . 20)
                                        (:from . 22)
                                        (:subject)))

            ;; save attachment to Downloads
            (setq mu4e-attachment-dir "~/Downloads")

            ;; attempt to show images when viewing messages
            (setq mu4e-view-show-images t)

            ;; show full addresses in message view
            (setq mu4e-view-show-addresses t)

            ;; always start mu4e in the background
            (mu4e t)))

(use-package mu4e-alert
  :ensure t
  :config (progn
            (mu4e-alert-set-default-style 'notifications)
            (add-hook 'after-init-hook #'mu4e-alert-enable-notifications)
            (add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)))

(use-package no-littering
  :ensure t
  :config (progn
            (eval-when-compile
              (require 'recentf))
            (with-eval-after-load 'recentf
              (add-to-list 'recentf-exclude no-littering-var-directory)
              (add-to-list 'recentf-exclude no-littering-etc-directory))))

(use-package notmuch
  :ensure t
  :disabled t
  :ensure-system-package (notmuch offlineimap)
  :config (progn
            ;; email sending via message mode
            (setq mail-user-agent 'message-user-agent)
            (setq message-auto-save-directory "~/Mail/Canonical/Drafts/")
            (setq notmuch-fcc-dirs '(("alex.murray@canonical.com" . "\"Canonical/Sent\" +sent -inbox -unread")))))

(use-package nxml-mode
  ;; enable 'folding' with nxml-mode
  :init (progn
          (require 'hideshow)
          (require 'sgml-mode)

          (add-to-list 'hs-special-modes-alist
                       '(nxml-mode
                         "<!--\\|<[^/>]*[^/]>"
                         "-->\\|</[^/>]*[^/]>"

                         "<!--"
                         sgml-skip-tag-forward
                         nil))

          (add-hook 'nxml-mode-hook 'hs-minor-mode))
  :config (setq nxml-slash-auto-complete-flag t))

(use-package org
  :ensure org-plus-contrib
  :pin org
  :config (progn
            (setq org-agenda-files (mapcar #'expand-file-name
                                           '("~/Dropbox/Orgzly/personal.org"
                                             "~/Dropbox/Orgzly/canonical.org"
                                             "~/Dropbox/Orgzly/general.org"
                                             "~/Dropbox/Orgzly/blog.org"
                                             "~/Dropbox/Orgzly/notes.org"))
                  ;; don't indent org document sections etc
                  org-adapt-indentation nil
                  org-imenu-depth 4
                  org-todo-keywords '((sequence "TODO(t)" "IN-PROGRESS(p!)" "BLOCKED(b@)" "DEFERRED(D@)" "|" "DONE(d!)")
                                      (sequence "|" "CANCELLED(c@)" "DELEGATED(G@)"))
                  org-goto-interface 'outline-path-completion)
            ;; set up org-babel integration for plantuml
            (org-babel-do-load-languages
             'org-babel-load-languages
             '((plantuml .t)))))

(use-package org-agenda
  :ensure org-plus-contrib
  :pin org
  :preface
  (defun apm-org-agenda-file-notify (_event)
    "Rebuild appointments when _EVENT specifies any org agenda files change."
    (org-agenda-to-appt t))
  :config (progn
            ;; when modifying agenda files make sure to update appt
            (require 'filenotify)
            (dolist (file org-agenda-files)
              (file-notify-add-watch file '(change) #'apm-org-agenda-file-notify))
            ;; rebuild appointments now
            (org-agenda-to-appt t)))

(use-package org-capture
  :after org
  :config (setq org-capture-templates '(("t" "Todo" entry (file+headline "~/Dropbox/Orgzly/canonical.org" "Tasks")
                                         "* TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n"))))

(use-package org-clock
  :after org
  :preface
  (defun apm-org-clock-heading ()
    "Create `org-clock-heading' by truncating if needed."
    (s-truncate 8 (nth 4 (org-heading-components))))

  (defvar apm-org-clock-notification nil)

  (defun apm-org-clock-clear-notification ()
    "Clear any existing org clock notification."
    (when (not (or (eq apm-org-clock-notification 'ignore)
                   (null apm-org-clock-notification)))
      (notifications-close-notification apm-org-clock-notification))
    (setq apm-org-clock-notification nil))

  (defun apm-org-clock-warn-notification-action (_id action)
    "For notification ID handle ACTION."
    (pcase action
      ("ignore" (setq apm-org-clock-notification 'ignore))
      ("default" (progn
                   (raise-frame)
                   (make-frame-visible)
                   (select-frame-set-input-focus (selected-frame))
                   (org-mru-clock-in)
                   (apm-org-clock-clear-notification)))))

  (defun apm-org-clock-warn-if-not-clocked-in ()
    "Warn if not currently clocked in."
    (eval-when-compile
      (require 'org-clock))
    (when (and (null org-clock-current-task)
               (not (eq apm-org-clock-notification 'ignore)))
      ;; show a notification but keep it persistent - don't show more than one
      (if (require 'notifications nil t)
          (progn
            (when apm-org-clock-notification
              (notifications-close-notification apm-org-clock-notification))
            (setq apm-org-clock-notification
                  (notifications-notify :title "You're not clocked in!"
                                        :body "Click to select a task or choose ignore..."
                                        :actions '("ignore" "Ignore"
                                                   "default" "Select one")
                                        :on-action #'apm-org-clock-warn-notification-action
                                        :on-close #'(lambda (id _reason)
                                                      (when (= id apm-org-clock-notification)
                                                        (setq apm-org-clock-notification nil)))))))))
  ;; ensure when clocking in we close any existing notification
  :hook ((org-clock-in . apm-org-clock-clear-notification))
  ;; assume idle after 5 minutes
  :ensure-system-package xprintidle
  :config (progn
            (setq org-clock-idle-time 5
                  ;; truncate clock heading in modeline
                  org-clock-heading-function #'apm-org-clock-heading
                  ;; save running clock and all history when exiting emacs
                  org-clock-persist t
                  ;; resume clocking task on clock-in if the clock is open
                  org-clock-in-resume t
                  ;; persist clock data into Dropbox
                  org-clock-persist-file (expand-file-name "~/Dropbox/Orgzly/org-clock-save.el")
                  ;; insert a CLOSED timestamp when TODOs are marked DONE
                  org-log-done 'time)
            (setq org-clock-x11idle-program-name "xprintidle")
            ;; reload any saved org clock information on startup
            (org-clock-persistence-insinuate)
            ;; notify if not clocked in - disable for now
            ;; (run-with-timer 60 60 #'apm-org-clock-warn-if-not-clocked-in)
            ))

(use-package org-clock-convenience
  :ensure t
  :bind (:map org-agenda-mode-map
              ("S-<up>" . org-clock-convenience-timestamp-up)
              ("S-<down>" . org-clock-convenience-timestamp-down)))

(use-package org-duration
  :ensure org-plus-contrib
  ;; don't show days, only total hours as maximum value
  :config (setq org-duration-format (quote h:mm)))

(use-package org-mru-clock
  :ensure t
  :config (setq org-mru-clock-completing-read #'ivy-completing-read))

(use-package org-mu4e
  :after (mu4e org)
  ;; store link to message if in header view, not to header query
  :config (setq org-mu4e-link-query-in-headers-mode nil))

(use-package org-notify
  :ensure org-plus-contrib
  :after org
  :config (progn
            (org-notify-start)
            (org-notify-add 'default '(:time "24h" :actions -notify/window :duration 600))
            (org-notify-add 'default '(:time "60m" :actions -notify/window :period "2m" :duration 600))
            (org-notify-add 'default '(:time "15m" :actions -notify/window :period "2m" :duration 120))))

(use-package org-notmuch
  :disabled t
  :ensure org-plus-contrib
  :after org)

(use-package org-table-sticky-header
  :ensure t
  :after org
  :defer t
  :hook ((org-mode . org-table-sticky-header-mode)))

(use-package ob-plantuml
  :after plantuml-mode
  :config (with-eval-after-load 'plantuml-mode
            (setq org-plantuml-jar-path plantuml-jar-path)))

(use-package paradox
  :ensure t
  :preface
  (defun apm-paradox-set-github-token (_no-fetch)
    "Load `paradox-github-token' from authinfo."
    (require 'epa-file)
    (require 'auth-source)
    (eval-when-compile
      (require 'paradox-github))
    (if (file-exists-p "~/.authinfo.gpg")
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
      (alert "No github token found in ~/.authinfo.gpg")))
  :commands (paradox-list-packages)
  :init (setq paradox-execute-asynchronously nil)
  :config (progn
            (paradox-enable)
            (advice-add 'paradox-list-packages :before 'apm-paradox-set-github-token)))

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

(use-package plantuml-mode
  :ensure t
  :mode ("\\.p\\(lant\\)?uml\\'" . plantuml-mode)
  :config (progn
            (setq plantuml-jar-path (expand-file-name "~/plantuml.jar"))
            (unless (file-exists-p plantuml-jar-path)
              (alert (format "plantuml not found at %s" plantuml-jar-path)))))

(use-package posframe
  :ensure t)

(use-package prescient
  :ensure t
  :config (prescient-persist-mode 1))

(use-package prog-mode
  :config (progn
            (when (boundp 'prettify-symbols-unprettify-at-point)
              ;; show original text when point is over a prettified symbol
              (setq prettify-symbols-unprettify-at-point 'right-edge))
            ;; prettify symbols (turn lambda -> λ)
            (global-prettify-symbols-mode 1)))

(use-package projectile
  :ensure t
  :defer t
  :defines (projectile-enable-caching)
  :init (progn
          (setq projectile-enable-caching t)
          (projectile-mode 1))
  :config (progn
            (add-to-list 'projectile-project-root-files "compile_commands.json")
            (add-to-list 'projectile-project-root-files "configure.ac")
            (add-to-list 'projectile-project-root-files ".cquery")
            (add-to-list 'projectile-project-root-files ".cquery.in")
            (add-to-list 'projectile-project-root-files "AndroidManifest.xml")
            (with-eval-after-load 'ivy
              (setq projectile-completion-system 'ivy))))

(use-package python
  :defer t
  :init (setq-default python-indent-offset 4))

(use-package racer
  :ensure t
  :preface
  (defun apm-racer-mode-setup ()
    "Setup racer-mode."
    (eval-when-compile
      (require 'racer))
    (unless (file-exists-p racer-cmd)
      (alert "cargo install racer?"))
    (unless (file-exists-p racer-rust-src-path)
      (alert (format "git clone https://github.com/rust-lang/rust.git %s"
                     (file-name-directory (directory-file-name racer-rust-src-path)))))
    (racer-mode 1))
  :defer t
  :hook ((rust-mode . apm-racer-mode-setup))
  :init (setq racer-rust-src-path (expand-file-name "~/rust/src")))

(use-package rainbow-mode
  :ensure t
  :defer t
  :hook (css-mode html-mode))

(use-package region-state
  :ensure t
  :config (region-state-mode 1))

(use-package rnc-mode
  :ensure t)

(use-package rust-mode
  :ensure t)

;; save minibuffer history
(use-package savehist
  :init (savehist-mode 1))

(use-package saveplace
  :config (progn
            (setq-default save-place t)
            (setq save-place-file (expand-file-name ".places" user-emacs-directory))))

(use-package scratch
  :ensure t
  :defer t)

(use-package server
  :config (progn
            ;; start emacs server only it has not already been started
            (unless (server-running-p)
              (server-start))
            (add-hook 'after-make-frame-functions
                    #'(lambda (frame) (select-frame-set-input-focus frame)) t)))

(use-package session-manager
  :load-path "vendor/"
  :config (session-manager-init "apm"))

(use-package sh-script
  :init (setq-default sh-basic-offset 2
                      sh-indentation 2))

(use-package simple
  :defer t
  :hook ((text-mode . visual-line-mode))
  :init (progn
          ;; save whatever is in the system clipboard to the kill ring before
          ;; killing something else into the kill ring
          (setq save-interprogram-paste-before-kill t)
          (setq visual-line-fringe-indicators
                '(left-curly-arrow right-curly-arrow))))

(use-package smartparens
  :ensure t
  :preface
  ;; taken from https://github.com/Fuco1/smartparens/issues/80#issuecomment-18910312
  (defun apm-c-mode-common-open-block (&rest ignored)
    "Open a new brace or bracket expression, with relevant newlines and indent (IGNORED is ignored)."
    (newline)
    (indent-according-to-mode)
    (forward-line -1)
    (indent-according-to-mode))
  ;; use smartparens in strict mode for programming and ielm
  :hook ((prog-mode ielm-mode) . smartparens-strict-mode)
  :init (smartparens-global-mode 1)
  :config (progn
            (require 'smartparens-config)
            (require 'smartparens-latex)
            (setq sp-base-key-bindings 'paredit)
            ;; always jump out of string when hitting end "
            (setq sp-autoskip-closing-pair 'always)
            (setq sp-hybrid-kill-entire-symbol nil)
            (sp-use-paredit-bindings)

            ;; highlights matching pairs
            (show-smartparens-global-mode 1)

            ;; disable pairing of ' in minibuffer
            (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)

            ;; use smartparens to automatically indent correctly when opening
            ;; a new block
            (dolist (mode '(c-mode c++-mode java-mode))
              (sp-local-pair mode "{" nil :post-handlers '((apm-c-mode-common-open-block "RET"))))
            ;; don't try and complete tag end - breaks nxml completion etc
            (sp-local-pair 'nxml-mode "<" ">" :actions '(:rem insert))))

(use-package smtpmail
  ;; ensure an entry in ~/.authinfo.gpg or similar like:
  ;; machine smtp.canonical.com login USERNAME port 587 password PASSWORD
  :config (progn
            (setq smtpmail-smtp-server "smtp.canonical.com")
            (setq smtpmail-smtp-service 587)))

(use-package solarized-theme
  :ensure t
  :config (progn
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
            (load-theme 'solarized-dark t)))

(use-package sudo-edit
  :ensure t
  :commands (sudo-edit))

(use-package suggest
  :ensure t)

(use-package systemd
  :ensure t)

(use-package tramp
  :config (setq-default tramp-default-method "ssh"))

(use-package undo-tree
  :ensure t
  :init (global-undo-tree-mode 1))

(use-package uniquify
  :config (setq uniquify-buffer-name-style 'post-forward
                uniquify-separator ":"
                uniquify-after-kill-buffer-p t
                uniquify-ignore-buffers-re "^\\*"))

(use-package web-mode
  :ensure t
  :defer t
  :init (progn
          ;; use smartparens instead
          (setq web-mode-enable-auto-pairing nil))
  :mode ("\\.php\\'" . web-mode))

(use-package which-func
  :config (which-function-mode 1))

(use-package which-key
  :ensure t
  :config (which-key-mode))

(use-package whitespace
  :hook ((prog-mode . whitespace-mode))
  ;; higlight long lines
  :init (progn
          (setq-default whitespace-line-column 80)
          ;; show tabs, trailing whitespace and long lines
          (setq-default whitespace-style
                        '(face trailing tabs tab-mark lines-tail))
          (setq-default whitespace-display-mappings
                        '((tab-mark ?\t [?\u279b ?\t] [?\\ ?\t])))))

(use-package yaml-mode
  :ensure t)

(use-package yasnippet
  :ensure t
  :bind (:map yas-keymap
              ;; Use C-tab to both expand a snippet and move through its fields
              ("<C-tab>" . yas-next-field-or-maybe-expand)
              ("C-f" . yas-next-field-or-maybe-expand)
              ("C-b" . yas-prev-field)
              ("TAB" . nil)
              ("<tab>" . nil)
              ("S-<tab>" . nil)
              ("<backtab>"  .  nil)
              :map yas-minor-mode-map
              ("<C-tab>" . yas-expand)
              ("TAB" . nil)
              ("<tab>" . nil))
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

(use-package xref
  :config (with-eval-after-load 'evil
            (define-key evil-visual-state-map (kbd "C-]") #'xref-find-definitions)
            (define-key evil-normal-state-map (kbd "C-]") #'xref-find-definitions)
            (define-key evil-normal-state-map (kbd "C-t") #'xref-pop-marker-stack)))

;; set gc-cons-threshold back to original value
(add-hook 'emacs-startup-hook #'apm-set-gc-threshold)

(provide 'init)

;;; init.el ends here
