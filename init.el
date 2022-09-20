;;; init.el --- Starting point for Alex Murray's Emacs Configuration -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; prefer newer non-byte compiled sources to older byte compiled ones
(setq load-prefer-newer t)

;;; Package management
(require 'package)

;; add melpa archive
(eval-and-compile
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/")))

(defvar use-package-enable-imenu-support t)
;; must be set before loading use-package
(setq use-package-enable-imenu-support t)

;; Bootstrap `use-package' from melpa
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (require 'use-package))

;; uncomment to debug package loading times
;; (setq use-package-verbose t)
;; compute stats to see what is taking so long on init
;; (setq use-package-compute-statistics t)

;; load no-littering as soon as possible during init so it can hook as many
;; paths as possible
(use-package no-littering
  :ensure t
  :config
  (eval-when-compile
    (require 'recentf))
  (with-eval-after-load 'recentf
    (add-to-list 'recentf-exclude no-littering-var-directory)
    (add-to-list 'recentf-exclude no-littering-etc-directory)))

(use-package use-package-ensure-system-package
  :ensure t)

(use-package diminish
  :ensure t)

(use-package bind-key
  :ensure t)

(use-package system-packages
  :ensure t
  :config
  (setq system-packages-package-manager 'apt)
  (setq system-packages-noconfirm t)
  (setq system-packages-use-sudo t))

;; customisations
(setq custom-file (locate-user-emacs-file "custom.el"))
;; load custom but ignore error if doesn't exist
(load custom-file 'noerror 'nomessage)

(defvar apm-preferred-dark-theme nil)
(defvar apm-preferred-light-theme nil)
(defvar apm-preferred-theme nil)

(defvar apm-load-preferred-theme-hook nil)

(defun apm-load-preferred-theme (dark)
  "Set the preferred DARK or light theme."
  (let ((theme (if (eq dark 1)
                   apm-preferred-dark-theme
                 apm-preferred-light-theme)))
    (when theme
      (when apm-preferred-theme
        (disable-theme apm-preferred-theme))
      (setq apm-preferred-theme theme)
      (load-theme apm-preferred-theme t)
      (run-hooks 'apm-load-preferred-theme-hook))))

(defun apm-desktop-portal-settings-changed (path var value)
  "Update preferred theme based on VALUE of VAR at PATH."
  (when (and (string-equal path "org.freedesktop.appearance")
             (string-equal var "color-scheme"))
    (apm-load-preferred-theme (car value))))

(defun apm-set-preferred-theme ()
  "Set preferred theme based on desktop color-scheme."
  (dbus-call-method-asynchronously
   :session
   "org.freedesktop.portal.Desktop"
   "/org/freedesktop/portal/desktop"
   "org.freedesktop.portal.Settings"
   "Read"
   (lambda (value)
     (apm-load-preferred-theme (caar value)))
   "org.freedesktop.appearance"
   "color-scheme"))

(when (require 'dbus nil t)
  ;; monitor for changes to the desktop portal settings
  (dbus-register-signal
   :session
   "org.freedesktop.portal.Desktop"
   "/org/freedesktop/portal/desktop"
   "org.freedesktop.portal.Settings"
   "SettingChanged"
   #'apm-desktop-portal-settings-changed))

(use-package doom-themes
  :ensure t
  :preface
  (defun apm-setup-doom-themes ()
    (if (eq apm-preferred-theme apm-preferred-dark-theme)
        (custom-set-faces `(erc-keyword-face ((t (:weight bold :foreground ,(doom-color 'yellow)))))
                          ;; make some notmuch elements have more contrast
                          `(notmuch-message-summary-face ((t (:foreground ,(doom-color 'constants)))))
                          `(notmuch-wash-cited-text ((t (:foreground ,(doom-color 'base6))))))
      (custom-set-faces `(erc-keyword-face ((t (:weight bold :foreground ,(doom-color 'yellow)))))
                        ;; revert some elements for light theme
                        `(notmuch-message-summary-face ((t (:foreground ,(doom-color 'grey)))))
                        `(notmuch-wash-cited-text ((t (:foreground ,(doom-color 'base4))))))))
  :custom
  (doom-one-padded-modeline t)
  (doom-one-light-padded-modeline t)
  :config
  (doom-themes-visual-bell-config)
  (doom-themes-org-config)
  (setq apm-preferred-dark-theme 'doom-one)
  (setq apm-preferred-light-theme 'doom-one-light)
  ;; set customisations after loading the theme
  (add-hook 'apm-load-preferred-theme-hook #'apm-setup-doom-themes)
  (apm-set-preferred-theme))

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


;;; General settings etc from C source so no package to associate settings
;;; with...

;; use pipes for subprocess communication
(setq-default process-connection-type nil)
;; performance increases as per https://emacs-lsp.github.io/lsp-mode/page/performance/
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; personalisation
(setq user-full-name "Alex Murray")

;; enable narrow-to-region
(put 'narrow-to-region 'disabled nil)

;; tabs are never ok
(setq-default indent-tabs-mode nil)

;; set a reasonable fill and comment column
(setq-default fill-column 80)
(setq-default comment-column 80)

;; don't try and use dialog boxes
(setq-default use-dialog-box nil)
(setq-default use-file-dialog nil)

;; inhibit startup message and splash screen
(setq inhibit-startup-message t)
;; remove message from initial scratch buffer
(setq initial-scratch-message nil)

;; don't restore window layout on minibuffer exit
(setq read-minibuffer-restore-windows nil)

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

;; use better font
(let ((preferred-font "Fira Code-10"))
  (if (font-info preferred-font)
      ;; apply this to all existing and future frames
      (set-frame-font preferred-font nil t)
    (system-packages-install "fonts-firacode")))



;; make emacs less laggy
(setq inhibit-compacting-font-caches t)

(set-language-environment "UTF-8")

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

(use-package all-the-icons-completion
  :ensure t
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :hook (after-init . all-the-icons-completion-mode))

(use-package all-the-icons-dired
  :ensure t
  :hook ((dired-mode . all-the-icons-dired-mode)))

(use-package ansi-color
  :hook ((compilation-filter . ansi-color-compilation-filter))
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
  :ensure t)

(use-package appt
  :preface
  (defun apm-appt-notify (min-to-appt _new-time msg)
    "Notify for appointment at MIN-TO-APPT for _NEW-TIME with MSG."
    ;; the arguments may be lists or not so always use them as lists
    (unless (listp min-to-appt)
      (setq min-to-appt (list min-to-appt)))
    (unless (listp msg)
      (setq msg (list msg)))
    (alert (string-join msg "\n")
           :title (format "Appointment(s) in %s minutes" (string-join min-to-appt ", "))
           :icon "/usr/share/icons/HighContrast/32x32/status/appointment-soon.png"))
  :config
  (setq appt-disp-window-function #'apm-appt-notify)
  (appt-activate 1))

(use-package apropos
  :bind ("C-h a" . apropos))

(use-package arc-mode
  :config  (add-to-list 'auto-mode-alist '("\\.snap\\'" . archive-mode)))

(use-package asn1-mode
  :ensure t
  :defer t
  :mode (("\\.asn1?$" . asn1-mode)))

(use-package autorevert
  :diminish auto-revert-mode
  :config
  (setq global-auto-revert-non-file-buffers t)
  (global-auto-revert-mode 1))

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

(use-package auth-source
  ;; prefer gnome-keyring via Login keyring, encrypted auth source to non-encrypted
  :init
  (require 'secrets)
  (setq auth-sources '("~/.authinfo" "secrets:Login" "~/.netrc")))

(use-package avy
  :ensure t
  :bind (("C-\\" . avy-goto-char-timer))
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
  :config (bbdb-initialize 'message))

(use-package blacken
  :ensure t
  :init (unless (executable-find "black")
          (alert "Please install the black snap")))

(use-package blamer
  :ensure t)

(use-package browse-kill-ring
  :ensure t
  :config (browse-kill-ring-default-keybindings))

(use-package browse-url
  ;; since using the firefox snap this needs to be somewhere that a snap
  ;; can access and global /tmp is not that... plus this also means we
  ;; don't drop our random files in the global /tmp which is good too
  :custom ((browse-url-temp-dir (expand-file-name "~/tmp"))))

(use-package bs
  :bind ("C-x C-b" . bs-show))

(use-package bug-reference
  :defer t
  :hook ((prog-mode . bug-reference-prog-mode)
         (erc-mode . bug-reference-mode)
         (notmuch-show-mode . bug-reference-mode)
         (org-mode . bug-reference-mode))
  :preface (defun apm-bug-reference-url-format ()
             (let ((prefix (match-string-no-properties 2))
                   (id (match-string-no-properties 4)))
               (cond ((or (string-prefix-p "lp" prefix t)
                          (string-prefix-p "bug" prefix t))
                      (format "https://launchpad.net/bugs/%s" id))
                     ((string-prefix-p "CVE" prefix)
                      (format "https://ubuntu.com/security/CVE-%s" id))
                     ((string-match "^\\([UL]SN\\)" prefix)
                      (format "https://ubuntu.com/security/notices/%s-%s"
                              (match-string-no-properties 1 prefix) id))
                     ((string-prefix-p "USN" prefix)
                      (format "https://ubuntu.com/security/notices/USN-%s" id))
                     (t (error (concat "Unknown bug prefix '%s'" prefix))))))
  :init
  (eval-when-compile
    (require 'bug-reference))
  (setq bug-reference-url-format #'apm-bug-reference-url-format
        bug-reference-bug-regexp "\\<\\(\\(\\([Ll][Pp]:?\\|bug\\) #?\\|CVE[ -]\\|[UL]SN[ -]\\)\\([0-9][0-9-]*\\)\\)\\>"))

(use-package calendar
  :custom (calendar-week-start-day 1))

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
      (if (and filename (string-match-p (expand-file-name "~/git/apparmor-kernel") filename))
          ;; upstream linux kernel style actually uses tabs... urgh
          (progn
            (setq indent-tabs-mode t)
            (setq c-basic-offset 8)
            (c-set-style "linux-tabs-only"))
        ;; default to normal linux style
        (c-set-style "linux")))
    ;; always show trailing whitespace
    (setq show-trailing-whitespace t)
    ;; ensure fill-paragraph takes doxygen @ markers as start of new
    ;; paragraphs properly
    (setq paragraph-start "^[ ]*\\(//+\\|\\**\\)[ ]*\\([ ]*$\\|@param\\)\\|^\f"))
  :hook (((c-mode c++-mode) . apm-c-mode-setup))
  :config
  ;; treat linux styles as safe for local variable
  (add-to-list 'safe-local-variable-values '(c-indentation-style . linux))
  (add-to-list 'safe-local-variable-values '(c-indentation-style . linux-tabs-only)))

(use-package check-cves-mode
  :load-path "~/ubuntu/git/ubuntu-cve-tracker/scripts/"
  :mode ("check-cves\\..*\\'" . check-cves-mode))

(use-package comint
  :hook ((comint-output-filter-functions . comint-osc-process-output)))

(use-package comint-mime
  :ensure t
  :hook ((shell-mode . comint-mime-setup)))

(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  :init
  (global-corfu-mode 1)
  (setq tab-always-indent 'complete)
  ;; cycle with at least 3 candidates
  (setq completion-cycle-threshold 3))

(use-package corfu-doc
  :ensure t
  :hook (corfu-mode . corfu-doc-mode)
  :bind (:map corfu-map
              ("M-p" . corfu-doc-scroll-down)
              ("M-n" . corfu-doc-scroll-up)
              ("M-d" . corfu-doc-toggle)))

(use-package cape
  :ensure t
  :config
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-ispell))

(use-package compile
  :hook ((shell-mode . compilation-shell-minor-mode))
  ;; automatically scroll to first error on output
  :config (setq compilation-scroll-output 'first-error)
  ;; use compilation-mode for .build files
  (add-to-list 'auto-mode-alist '("\\.build\\'" . compilation-mode)))

(use-package consult
  :ensure t
  :bind (
         ("C-c h" . consult-history)
         ("C-c s" . consult-clock-in)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("M-s i" . consult-imenu)
         ("M-s l" . consult-line)
         ("M-s o" . consult-outline)
         ("M-y". consult-yank-pop)
         ("<help> a" . consult-apropos)
         ("M-g e" . consult-compile-error)
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-project-imenu)
         ;; M-s bindings (search-map)
         ("M-s f" . consult-find)
         ("M-s L" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch)
         ;; https://emacsredux.com/blog/2021/11/25/redo-complex-command-with-consult/
         ([remap repeat-complex-command] . consult-complex-command)
         :map isearch-mode-map
         ("M-e" . consult-isearch)                 ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch)               ;; orig. isearch-edit-string
         ("M-s l" . consult-line))
  :init
  ;; Replace functions (consult-multi-occur is a drop-in replacement)
  (fset 'multi-occur #'consult-multi-occur)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  ;; from https://github.com/minad/consult/wiki#org-clock
  (defun consult-clock-in (&optional match scope resolve)
    "Clock into an Org heading."
    (interactive (list nil nil current-prefix-arg))
    (require 'org-clock)
    (org-clock-load)
    (save-window-excursion
      (consult-org-heading
       match
       (or scope
           (thread-last org-clock-history
                        (mapcar 'marker-buffer)
                        (mapcar 'buffer-file-name)
                        (delete-dups)
                        (delq nil))
           (user-error "No recent clocked tasks")))
      (org-clock-in nil (when resolve
                          (org-resolve-clocks)
                          (org-read-date t t)))))
  (consult-customize consult-clock-in
                     :prompt "Clock in: "
                     :preview-key (kbd "M-.")
                     :group
                     (lambda (cand transform)
                       (let* ((marker (get-text-property 0 'consult--candidate cand))
                              (name (if (member marker org-clock-history)
                                        "*Recent*"
                                      (buffer-name (marker-buffer marker)))))
                         (if transform (substring cand (1+ (length name))) name)))))

(use-package consult-notmuch
  :ensure t)

(use-package copilot
  :load-path "vendor/"
  ;; no copilot access anymore
  :disabled t
  :bind (:map copilot-mode-map
              ("C-<tab>" . copilot-accept-completion))
  :hook ((prog-mode . copilot-mode))
  ;; this needs to be set after copilot-mode is loaded otherwise it will
  ;; still be the default value which is relative to copilot.el in our
  ;; local vendor directory
  :config (setq copilot--base-dir "/snap/copilot-client/current")
  :custom
  ;; ensure we use the node executable shipped in the copilot-client snap
  (copilot-node-executable "/snap/bin/copilot-client.node"))

(use-package crontab-mode
  :ensure t)

(use-package cua-base
  ;; use CUA mode for rectangle selections etc but not copy/paste etc
  :init (cua-selection-mode 1))

(use-package cve-mode
  :load-path "~/ubuntu/git/ubuntu-cve-tracker/scripts"
  :mode (("CVE-[[:digit:]]\\{4\\}-[[:digit:]]\\{4,\\}\\'" . cve-mode)
         ("00boilerplate.*\\'" . cve-mode))
  :hook ((cve-mode . flymake-mode)))

;; show suspicious c constructs automatically
(use-package cwarn
  :diminish cwarn-mode
  :init (global-cwarn-mode 1))

(use-package cycle-at-point
  :ensure t
  :bind (("M-p" . cycle-at-point)))

(use-package deadgrep
  :ensure t
  :ensure-system-package (rg . ripgrep))

(use-package debian-el
  :ensure t)

(use-package cvelist
  :load debian-cvelist
  :load-path "~/ubuntu/git/security-tracker/conf/"
  :mode ("list" . debian-cvelist-mode))

(use-package debbugs
  :ensure t)

(use-package define-word
  :ensure t
  :bind (("C-c d" . define-word-at-point)
         ("C-c D" . define-word)))

(use-package diff-mode
  :mode (("\\.debdiff\\'" . diff-mode)))

(use-package disk-usage
  :ensure t)

(use-package display-fill-column-indicator
  :hook ((prog-mode . display-fill-column-indicator-mode)
         (text-mode . display-fill-column-indicator-mode)))

(use-package dpkg-dev-el
  :ensure t
  :after flyspell
  :hook (debian-changelog-mode . flyspell-mode))

(use-package debian-changelog-mode
  :ensure dpkg-dev-el
  :config (let ((releases (append (split-string
                                   (shell-command-to-string
                                    "distro-info --supported-esm"))
                                   (split-string
                                   (shell-command-to-string
                                    "distro-info --devel")))))
            (dolist (release releases)
              (add-to-list 'debian-changelog-allowed-distributions release))))

(use-package delsel
  ;; enable delete-selection mode to allow replacing selected region
  ;; with new text automatically
  :init (delete-selection-mode 1))

(use-package diff-hl
  :ensure t
  ;; Integrate with Magit and highlight changed files in the fringe of dired
  :hook ((magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh)
         (dired-mode . diff-hl-dired-mode))
  :config (global-diff-hl-mode 1))

(use-package disaster
  :ensure t
  :bind ((:map c-mode-base-map ("C-c d" . disaster))))

(use-package dts-mode
  :ensure t)

(use-package edebug-inline-result
  :ensure t
  :defer t
  :custom (edebug-inline-result-backend 'posframe)
  :hook (edebug-mode . edebug-inline-result-mode))

(use-package ediff
  :defer t
  :config (setq ediff-window-setup-function 'ediff-setup-windows-plain
                ediff-split-window-function 'split-window-horizontally))

(use-package eglot
  :ensure t
  :pin gnu
  :hook (prog-mode . eglot-ensure)
  :custom (eglot-extend-to-xref t)
  :config (add-to-list 'eglot-server-programs '(markdown-mode "vscode-markdown-languageserver" "--stdio")))

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

(use-package epg
  :config
  (setq epg-user-id "alex.murray@canonical.com"))

(use-package erc
  :ensure t
  :pin gnu
  :preface
  (eval-when-compile
    (require 'erc-log)
    (require 'erc-match))

  (defun apm-prompt-to-connect-to-irc ()
    "Prompt to connect to irc."
    (interactive)
    (when (y-or-n-p "Connect to IRC? ")
      ;; connect to matterircd on localhost and oftc and freenode via znc
      ;;(erc :server "localhost" :port "6667" :nick "alexmurray")
      (erc-tls :server "znc.secret.server" :port "7076"
               :nick "amurray" :password (concat "amurray/OFTC:"
                                                 (auth-source-pick-first-password
                                                  :user "amurray"
                                                  :host "znc.secret.server"
                                                  :port "7076")))
      (erc-tls :server "znc.secret.server" :port "7076"
               ;; secret-tool store --label="ZNC" host znc.secret.server \
               ;; user amurray port 7076
               :nick "amurray" :password (concat "amurray/libera:"
                                                 (auth-source-pick-first-password
                                                  :user "amurray"
                                                  :host "znc.secret.server"
                                                  :port "7076")))))
  (defgroup apm-erc nil
    "apm's erc customisations."
    :group 'erc)

  ;; face to show in header line when disconnected
  (defface apm-erc-header-line-disconnected
    '((t (:foreground "black" :background "indianred")))
    "Face to use when ERC has been disconnected."
    :group 'apm-erc)

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

  (defun apm-erc-nicks ()
    "Returns the list of possible nicks from `erc-nick'."
    (if (listp erc-nick)
        erc-nick
      (list erc-nick)))

  (defun apm-occur-in-erc (&optional regexp)
    "Find matches of REGEXP in all erc buffers.
With a prefix argument, will default to looking for all
`erc-keywords' and mentions of `erc-nick'."
    (interactive
     (list
      (let ((regex  (concat "\\(" (regexp-opt erc-keywords) "\\|"
                            (concat "\\(^\\|[^<]\\)" (regexp-opt (apm-erc-nicks)) "\\([^>]\\|$\\)")
                            "\\)")))
        (read-string "Regexp: "
                     (substring-no-properties
                      (or (cond ((region-active-p)
                                 (buffer-substring (region-beginning) (region-end)))
                                (current-prefix-arg
                                 regex)
                                (t
                                 (word-at-point)))
                          ""))))))
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

  (defun apm-erc-lookup-nick (nick)
    ;; if this is a matterircd buffer then query via launchpadid since they
    ;; are used as nicks there
    (if (eq 'matterircd (erc-network))
        (apm-eudc-lookup-launchpadid nick)
      (apm-eudc-lookup-nick nick)))

  :hook ((after-init . apm-prompt-to-connect-to-irc))
  :bind (:map erc-mode-map
              ("C-c f e" . apm-erc-find-logfile)
              ("M-s e" . apm-occur-in-erc))
  :config
  (eval-and-compile
    (require 'erc-button)
    (require 'erc-desktop-notifications)
    (require 'erc-fill)
    (require 'erc-join)
    (require 'erc-log)
    (require 'erc-match)
    (require 'erc-networks)
    (require 'erc-notify)
    (require 'erc-services)
    (require 'erc-track))

  (setq erc-user-full-name user-full-name)
  (setq erc-nick (list user-login-name "alexmurray"))
  (setq erc-prompt-for-nickserv-password nil)
  ;; need to ensure we set the password as:
  ;; secret-tool store --label="Libera IRC NickServ" host irc.libera.chat user amurray
  ;; secret-tool store --label="OFTC IRC NickServ" host irc.oftc.net user amurray

  (setq erc-use-auth-source-for-nickserv-password t)

  (setq erc-autojoin-timing 'ident)

  ;; since we connect to oftc directly, we need to autojoin channels there
  ;; - not needed for libera (since we use ZNC) or canonical matterircd
  (setq erc-autojoin-channels-alist nil)
  (setq erc-fill-function #'erc-fill-static)
  ;; account for really long names
  (setq erc-fill-static-center 22)
  ;; this fits on a dual horizontal split on my laptop
  (setq erc-fill-column 110)

  ;; use sensible buffer names with server as well
  (setq erc-rename-buffers t)

  ;; try harder to reconnect but wait longer each time since it may take a
  ;; while to get a DHCP lease etc
  (setq erc-server-auto-reconnect t)
  (setq erc-server-reconnect-attempts 5)
  (setq erc-server-reconnect-timeout 30)

  (add-to-list 'erc-modules 'button)
  (add-to-list 'erc-modules 'log)
  (add-to-list 'erc-modules 'match)
  (add-to-list 'erc-modules 'notifications)
  (add-to-list 'erc-modules 'services)
  (add-to-list 'erc-modules 'spelling)
  (erc-update-modules)

  ;; format nicknames to show if user has voice(+), owner (~), admin (&),
  ;; operator (@)
  (setq erc-format-nick-function 'erc-format-@nick)
  (setq erc-pals '("joemcmanus" "jdstrand" "mdeslaur" "sbeattie" "jjohansen" "jj" "sarnold" "ChrisCoulson" "leosilva" "msalvatore" "ebarretto" "pfsmorigo" "emitorino" "deafpool" "Avital" "dan" "sid"))
  (setq erc-notify-list erc-pals)

  (setq erc-keywords '("alexmurray" "cve" "vulnerability" "apparmor" "seccomp" "exploit" "security" "esm" "@here" "@all" "@channel" "@security"))

  ;; when joining don't bring to front
  (setq erc-join-buffer 'bury)

  (setq erc-track-switch-direction 'importance)
  (setq erc-track-exclude-types '("JOIN" "PART" "QUIT" "NICK" "MODE"
                                  ;; channel mode (324), creation
                                  ;; time (329), topic (332), topic
                                  ;; who time (333), names (353), no
                                  ;; chan modes (477)
                                  "324" "329" "332" "333" "353" "477"))

  (setq erc-track-exclude-server-buffer t)
  (setq erc-track-showcount t)
  ;; emacs channels are noisy
  (setq erc-track-exclude '("#emacs" "#emacsconf" "#ubuntu"))
  (setq erc-track-shorten-function nil)
  ;; ensure our nick highlighted with erc-hl-nicks gets picked up by
  ;; erc-track
  (with-eval-after-load 'erc-hl-nicks
    (dolist (nick (apm-erc-nicks))
      (add-to-list 'erc-track-faces-priority-list
                   `(,(erc-hl-nicks-make-face nick) erc-current-nick-face))))

  (add-to-list 'erc-nick-popup-alist
               '("Directory" . (apm-erc-lookup-nick nick)))
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

  ;; log mentions when away
  (add-to-list 'erc-log-matches-types-alist
               '(current-nick . "ERC Mentions"))

  (unless (file-exists-p erc-log-channels-directory)
    (mkdir erc-log-channels-directory t))

  (erc-autojoin-mode 1)

  (erc-spelling-mode 1)

  ;; change header line face when disconnected
  (setq erc-header-line-face-method
        #'apm-erc-update-header-line-show-disconnected)

  ;; make sure any privmsg (which is via query buffers) show up as urgent
  ;; in track list
  (defadvice erc-track-select-mode-line-face (around erc-track-find-face-promote-query activate)
    (if (erc-query-buffer-p)
        (setq ad-return-value 'erc-current-nick-face)
      ad-do-it)))

(use-package erc-goodies
  :ensure erc
  ;; ensure this is set and we don't inadvertently unset it
  :bind (:map erc-mode-map ("C-c C-c" . nil))
  :config (setq erc-interpret-controls-p t))

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

(use-package erc-matterircd
  :load-path "vendor/"
  :after erc
  :config
  ;; don't clutter view with context ids and make sure emojify doesn't try
  ;; and emojify over the top of us otherwise the keymap added by
  ;; erc-matterircd's use of erc-button get lost
  (setq erc-matterircd-replace-context-id
        (propertize "↩" 'emojify-inhibit t))
  (setq erc-matterircd-updatelastviewed-on-buffer-switch t)
  (setq erc-matterircd-server "chat.canonical.com")
  (setq erc-matterircd-team "canonical")
  (let ((token (auth-source-pick-first-password :host "matterircd")))
    (if (null token)
        (alert (format "Please store matterircd token in secret store with :host matterircd"))
      (setq erc-matterircd-password (concat "token=" token))))
  (add-to-list 'erc-modules 'matterircd)
  (erc-update-modules))

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
  :after ldap
  :config
  (eval-when-compile
    (require 'ldap))
  (eudc-set-server ldap-default-host 'ldap)
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
  (add-to-list 'eudc-user-attribute-names-alist
               '(githubid . "Github ID"))
  ;; keep byte-compiler happy
  (defvar eudc-bob-generic-keymap nil)
  ;; add support for querying up the hierarchy via manager
  (defvar apm-eudc-bob-query-keymap
    (let ((map (make-sparse-keymap)))
      (define-key map [return] 'apm-eudc-query-at-point)
      map))
  (set-keymap-parent apm-eudc-bob-query-keymap eudc-bob-generic-keymap)
  (add-to-list 'eudc-attribute-display-method-alist '("jpegphoto" . eudc-display-jpeg-inline))
  (add-to-list 'eudc-attribute-display-method-alist '("manager" . apm-eudc-display-query))
  (add-to-list 'eudc-attribute-display-method-alist '("utc offset" . apm-eudc-display-utc-offset))
  (add-to-list 'eudc-attribute-display-method-alist '("timezone name" . apm-eudc-display-timezone))
  (add-to-list 'eudc-attribute-display-method-alist '("launchpad id" . apm-eudc-display-launchpadid))
  (add-to-list 'eudc-attribute-display-method-alist '("github id" . apm-eudc-display-githubid))
  (add-to-list 'eudc-attribute-display-method-alist '("irc nick" . apm-eudc-display-nick))
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

  (defun apm-eudc-display-githubid (id)
    "Display ID as a clickable URL."
    (eudc-display-url (concat "https://github.com/" id)))

  (defun apm-eudc-display-nick (nick)
    "Display NICK as using colors from erc-hl-nicks."
    (insert (propertize nick
                        'face
                        (erc-hl-nicks-make-face nick))))

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
    (eudc-display-records (eudc-query  `((mozillaNickName . ,nick)))))

  (defun apm-eudc-lookup-launchpadid (&optional id)
    (interactive
     (list
      ;; some buffers are read-only in which case `word-at-point' returns a
      ;; read-only string so need to remove properties
      (let ((initial (substring-no-properties (or (word-at-point) ""))))
        (if (eq major-mode 'erc-mode)
            (completing-read "Id: " (erc-get-channel-nickname-list)
                             nil nil initial)
          (read-string "Id: " initial)))))
    (eudc-display-records (eudc-query  `((launchpadid . ,id))))))

(use-package eudc-capf
  :config
  (add-to-list 'eudc-capf-modes 'notmuch-message-mode))

(use-package eudcb-notmuch-address
  :load-path "vendor/"
  :config (add-to-list 'eudc-server-hotlist '("localhost" . notmuch-address)))

(use-package eshell
  :defer t
  :preface
  (defun apm-eshell-mode-setup ()
    "Initialise 'eshell-mode'."
    (eval-when-compile
      (require 'em-cmpl))
    (eshell-cmpl-initialize))
  :commands eshell
  :hook ((eshell-mode . apm-eshell-mode-setup)))

(use-package eshell-syntax-highlighting
  :ensure t
  :config (eshell-syntax-highlighting-global-mode 1))

(use-package eterm-256color
  :ensure t
  :hook ((term-mode . eterm-256color-mode))
  :init (unless (file-exists-p "~/.terminfo/e/eterm-color")
          (make-directory "~/.terminfo/e/" t)
          (shell-command "tic /snap/emacs/current/usr/share/emacs/27.0.50/etc/e/eterm-color.ti")))

(use-package executable
  :hook ((after-save . executable-make-buffer-file-executable-if-script-p)))

(use-package exec-path-from-shell
  :ensure t
  :init (when (memq window-system '(mac ns))
          (exec-path-from-shell-initialize)))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package files
  :bind (("C-c r b" . revert-buffer))
  :config
  :custom
  (view-read-only t)
  (save-some-buffers-default-predicate #'save-some-buffers-root))

(use-package flymake
  :hook (prog-mode . flymake-mode))

(use-package flymake-shellcheck
  :ensure t
  :hook (sh-mode . flymake-shellcheck-load))

(use-package flyspell
  :defer t
  :diminish flyspell-mode
  ;; improve performance by making flyspell less chatty
  :config (setq flyspell-issue-message-flag nil)
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode)))

(use-package flyspell-correct
  :ensure t
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))

(use-package forge
  :ensure t
  :after magit)

(use-package forge-launchpad
  :load-path "vendor/"
  :after forge)

(use-package fringe
  :custom (indicate-empty-line t)
  ;; ensure we indicate empty lines via fringe-mode with defaults
  :config (fringe-mode))

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

(use-package gitattributes-mode
  :ensure git-modes
  :defer t)

(use-package gitconfig-mode
  :ensure git-modes
  :defer t)

(use-package gitignore-mode
  :ensure git-modes
  :defer t)

(use-package gnu-elpa
  :defer t
  :ensure t)

(use-package gnu-elpa-keyring-update
  :ensure t)

(use-package gnuplot
  :ensure t)

(use-package gnus-art
  :config
  ;; add custom highlighting to gnus for launchpad security bugs
  (add-to-list 'gnus-emphasis-alist
               (list "\\(Private security bug reported\\)" 1 1 'error))
  (add-to-list 'gnus-emphasis-alist
               (list "\\(This bug is a security vulnerability\\)" 1 1 'warning))
  ;; github PRs I have been requested to review
  (add-to-list 'gnus-emphasis-alist
               (list "\\(requested your review on:\\)" 1 1 'error))
  ;; and for build failure emacs
  (add-to-list 'gnus-emphasis-alist
               (list "\\(State: failed to \\(build\\|upload\\)\\)" 1 1 'error))
  ;; don't fill long lines as breaks tables in emails
  (setq gnus-treat-fill-long-lines nil)
  ;; discourse emails use --- as signature separator
  (setq gnus-signature-separator '("^---? $" "^---? *$")))

(use-package gnus-dired
  :ensure gnus
  :config
  ;; make the `gnus-dired-mail-buffers' function also work on
  ;; message-mode derived modes, such as notmuch-message-mode
  (define-advice gnus-dired-mail-buffers (:around (orig-fun &rest args) include-message-mode-derived-buffers)
    "Return a list of active message buffers."
    (let (buffers)
      (save-current-buffer
        (dolist (buffer (buffer-list t))
          (set-buffer buffer)
          (when (and (derived-mode-p 'message-mode)
                     (null message-sent-message-via))
            (push (buffer-name buffer) buffers))))
      (append (apply orig-fun args) (nreverse buffers))))
  (setq gnus-dired-mail-mode 'notmuch-user-agent)
  (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode))

(use-package goggles
  :ensure t
  :config (goggles-mode 1))

(use-package go-mode
  :ensure t
  :hook ((before-save . gofmt-before-save)))

(use-package goto-addr
  :defer t
  :hook ((prog-mode . goto-address-prog-mode)
         (text-mode . goto-address-mode)))

(use-package goto-line-preview
  :ensure t
  :config (global-set-key [remap goto-line] 'goto-line-preview))

(use-package gud
  :defer t
  :hook ((gud-mode . gud-tooltip-mode)))

(use-package help-at-pt
  :custom (help-at-pt-display-when-idle t))

(use-package help-fns
  :config (setq describe-bindings-outline t))

(use-package helpful
  :ensure t
  :bind (([remap describe-key]      . helpful-key)
         ([remap describe-command]  . helpful-command)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-function] . helpful-callable)))

(use-package hideshow
  :diminish hs-minor-mode
  ;; use hs-minor-mode in programming and mail composing - TODO - get it
  ;; working during mail viewing as well to be able to hide quoted bits
  ;; - something like:
  :hook ((prog-mode message-mode) . hs-minor-mode))

(use-package hippie-exp
  :bind (("M-/" . hippie-expand)))

(use-package hl-line
  :config (global-hl-line-mode 1))

(use-package hl-todo
  :ensure t
  :defer t
  :functions global-hl-todo-mode
  :config
  (add-to-list 'hl-todo-keyword-faces '("@todo" . "#cc9393"))
  (global-hl-todo-mode 1))

(use-package hungry-delete
  :ensure t
  :diminish hungry-delete-mode
  :config (global-hungry-delete-mode 1))

(use-package imenu
  :bind (("M-i" . imenu)))

(use-package ispell
  :defer t
  :ensure-system-package aspell
  :custom
  (ispell-program-name "aspell")
  (ispell-dictionary "australian")
  (ispell-extra-args '("--sug-mode=ultra"))
  (ispell-silently-savep t))

(use-package keypression
  :ensure t)

(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package journalctl-mode
  :ensure t)

(use-package jq-mode
  :ensure t)

(use-package js
  :custom (js-indent-level 2))

(use-package js2-mode
  :ensure t
  :defer t
  :init (setq-default js2-basic-offset 2))

(use-package json-mode
  :ensure t)

(use-package ldap
  :ensure-system-package (ldapsearch . ldap-utils)
  :demand t
  :custom
  ;; Store password using secret-tool as follows:
  ;; secret-tool store --label='Canonical LDAP' host ldaps://ldap.canonical.com
  ;; then enter PASSWORD
  (ldap-host-parameters-alist '(("ldaps://ldap.canonical.com"
      base "ou=staff,dc=canonical,dc=com"
      binddn "cn=Alex Murray,ou=staff,dc=canonical,dc=com"
      auth-source t)))
  (ldap-default-host "ldaps://ldap.canonical.com"))

(use-package lin
  :ensure t
  :config (setq lin-face 'lin-cyan-override-fg))

(use-package link-hint
  :ensure t
  :bind
  ("C-c C-l C-o" . link-hint-open-link)
  ("C-c C-l C-c" . link-hint-copy-link)
  :config
  (defun link-hint--next-bug-reference (bound)
    "Find the next bug-reference."
    (let ((next (next-single-char-property-change
                 (point) 'bug-reference-url nil bound)))
      (unless (eq next bound)
        ;; check 'bug-reference-url is set at next - if it is then return
        ;; this
        (catch 'found
          (dolist (overlay (overlays-at next))
            (when (overlay-get overlay 'bug-reference-url)
              (throw 'found next)))
          ;; otherwise find the next one
          (setq next
                (next-single-char-property-change
                 next 'bug-reference-url nil bound))
          (unless (eq next bound) next)))))

  (defun link-hint--bug-reference-at-point-p ()
    "Return the bug-reference url at the point or nil."
    (car (get-char-property-and-overlay (point) 'bug-reference-url)))

  (link-hint-define-type 'bug-reference
    :next #'link-hint--next-bug-reference
    :at-point-p #'link-hint--bug-reference-at-point-p
    :vars '(bug-reference-mode)
    :open #'browse-url
    :copy #'kill-new)

  (push 'link-hint-bug-reference link-hint-types))

(use-package lxd-tramp
  :ensure t)

(use-package lp
  :load-path "/snap/gitlptools/current"
  :after magit
  :init (unless (executable-find "git-lp-open")
          (alert "Please install the gitlptools snap")))

(use-package magit
  :ensure t
  :preface
  (defun apm-magit-process-lp-merge-request-prompt-hook (_ str)
    "Hook to handle LP merge request prompt in git output in STR."
    (let* ((regex "remote: Create a merge proposal for '.*' on Launchpad by visiting:\\s-*\nremote:\\s-*\\(https://code.launchpad.net[^[:space:]]*\\)")
           (resize-mini-windows t))
      (when (string-match regex str)
        (let ((prompt (string-replace
                       "remote: " ""
                       (substring-no-properties
                        str (match-beginning 0) (match-end 0))))
              (url (string-trim
                    (substring-no-properties
                     str (match-beginning 1) (match-end 1)))))
          (when (y-or-n-p prompt)
            (browse-url url))))))
  :init
  (add-hook 'magit-process-prompt-functions #'apm-magit-process-lp-merge-request-prompt-hook)
  :custom (magit-diff-refine-hunk t)
  :bind (("C-x g" . magit-status))
  :demand t)

(use-package magit-patch-changelog
  :ensure t)

(use-package magit-popup
  ;; whilst magit doesn't need this anymore, other packages do and magit
  ;; doesn't provide it anymore so explicitly add it
  :ensure t)

(use-package mallard-mode
  :ensure t
  :defer t)

(use-package marginalia
  :ensure t
  :hook ((after-init . marginalia-mode)))

(use-package markdown-mode
  :ensure t
  :defer t
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :ensure-system-package markdown)

(use-package mb-depth
  :config (minibuffer-depth-indicate-mode 1))

(use-package meson-mode
  :ensure t)

(use-package message
  ;; use standard completion UI for message completion
  :custom
  (message-expand-name-standard-ui t)
  (message-citation-line-format "On %a, %Y-%m-%d at %T %z, %N wrote:\n")
  (message-citation-line-function #'message-insert-formatted-citation-line)
  ;; kill message buffer after sending rather than burying
  (message-kill-buffer-on-exit t))

(use-package message-attachment-reminder
  :ensure t)

(use-package message-view-patch
  :ensure t
  :hook ((gnus-part-display . message-view-patch-highlight)))

(use-package mml-sec
  :config
  (define-advice mml-secure-find-usable-keys
      (:after-until (context name usage &optional justone) prompt-for-missing-key)
    (when (y-or-n-p (format "No %s key for %s; do you want to manually choose one? "
                            usage name))
      (let ((keys (epa-select-keys context
                                   (format "Choose a key for %s " name))))
        (if (and justone (> (length keys) 0))
            (mml-secure-select-keys context name keys usage)
          keys))))
  ;; encrypt to self
  (setq mml-secure-openpgp-encrypt-to-self t)
  (setq mml-secure-openpgp-sign-with-sender t)
  (setq mml-secure-key-preferences '((OpenPGP
                                      (sign)
                                      (encrypt
                                       ("security@ubuntu.com"
                                        ;; taken from https://wiki.ubuntu.com/SecurityTeam/GPGKeyTable
                                        "76840A6FFB3DEA51B723185F0EB3E83D29117223"
                                        "50C4A0DDCF31E452CEB19B516569D855A744BE93"
                                        "EDC4830FBD39AB6AC51047FB052F367018D5C3D8"
                                        "415010F1BA23C8C720DFB1F5F32172599D8D2E97"
                                        "44DFFFE4C1A008E83229E205611FBDECD5946E0F"
                                        "7FE79B445728C8EA0042839E45BCE75B840B1F69"
                                        "88E9530BCBDDC200517B5EB0F498D2D9DE7DAD9C"
                                        "0ADCB2CFA6B3532E80641CD2906788EB31A737FF"
                                        "9027444394FA0EEB52BDE0B26D4A7990BDE2CC66"
                                        "4F90787345208AC1CF25E30BE1C6FD981B751883"
                                        "5F2395C9FCE8A66078A8E9CDCCAACB01128F5657"
                                        "6C18C4CAF651E8473C6603408A8F7B1C00993172"
                                        "1BCE99F129F1EAE991455C1CD75C2ACFC9409A08"
                                        "F763837EFAF24ADBF3567E5A0DC98647D37D791E"
                                        "07A4E60A50B0646436868D4244470A1D4730D394"
                                        "8F06E0BAC0796B7ED5A363D2538B7C0DCCB5A3C9"
                                        "62BCA0771D460DE73D4D5F0467419E45C3399EDD"
                                        "EEEF423A2E6C0B8AA07812BA6034DD32D0F08B59"
                                        "73176FE2008213C5BD9EBB796B5F8F2FE775FC48"
                                        "1AAF7A49D008258D6F22744B6F302D087E8BA936"
                                        "03827EEE014E2DAC02CC74F2EDD0EA1FDFCFE8FA"
                                        "9B693D7440802E8A6FF803DE13498F032CCFE9DA"
                                        "8866CD2EAAB6D399E4D213DD196D412138F33F64"
                                        "B35EBCD35C6717BC0ADEB08AEC873ACED468723C"
                                        "D9682DBE6C2672068404B967080BCAD50BC3E920"
                                        "2D3B10FE866E24CA8BF7D42DD60B83C90513BD4F"
                                        )))
                                     (CMS
                                      (sign)
                                      (encrypt)))))

(use-package modern-cpp-font-lock
  :ensure t
  :defer t
  :hook ((c++-mode . modern-c++-font-lock-mode)))

(use-package moinmoin-mode
  :load-path "vendor/")

(use-package mouse
  :config (context-menu-mode 1))

(use-package move-text
  :ensure t
  :bind (([(meta shift up)] . move-text-up)
         ([(meta shift down)] . move-text-down)))

(use-package mqtt-mode
  :ensure t
  :ensure-system-package (mosquitto_pub .  mosquitto-clients))

(use-package nano-agenda
  :ensure t)

(use-package notmuch
  :ensure t
  :ensure-system-package (notmuch afew)
  :diminish notmuch-hello-mode notmuch-show-mode notmuch-tree-mode notmuch-unthreaded-mode
  :preface
  (defvar apm-notmuch-discouraged-senders '((("text/plain") . ("forum@forum.snapcraft.io"
                                                               "bounce@websense.com"
                                                               "wsm-postmaster@intel.com"
                                                               "no-reply@onepointpay.com.au"))))
  (defun apm-notmuch-determine-discouraged (msg)
    "Determine is MSG wants text/plain to be discouraged."
    (let* ((headers (plist-get msg :headers))
           (from (or (plist-get headers :From) ""))
           (discouraged '("text/html" "multipart/related")))
      (dolist (discouraged-sender apm-notmuch-discouraged-senders)
        (dolist (sender (cdr discouraged-sender))
          (when (string-search sender from)
            (setq discouraged (car discouraged-sender)))))
      discouraged))

  :bind (("C-c m" . notmuch)
         :map notmuch-show-mode-map (("D" . apm-notmuch-toggle-deleted)
                                     ("J" . apm-notmuch-toggle-spam))
         :map notmuch-search-mode-map (("D" . apm-notmuch-toggle-deleted)
                                       ("J" . apm-notmuch-toggle-spam))
         :map notmuch-tree-mode-map (("D" . apm-notmuch-toggle-deleted)
                                     ("J" . apm-notmuch-toggle-spam)))
  :custom
  (notmuch-wash-wrap-lines-length 150)
  (notmuch-print-mechanism #'notmuch-print-ps-print/evince)
  :config
  (eval-and-compile
    (require 'notmuch)
    (require 'notmuch-show)
    (require 'notmuch-tree))
  (setq notmuch-multipart/alternative-discouraged 'apm-notmuch-determine-discouraged)
  (defun apm-notmuch-toggle-tag (tag)
    "Toggle TAG for the current message returning t if we set it."
    (let ((gettagsfun nil)
          (tagfun nil))
      (pcase major-mode
        ('notmuch-search-mode
         (setq gettagsfun #'notmuch-search-get-tags)
         (setq tagfun #'notmuch-search-tag))
        ('notmuch-show-mode
         (setq gettagsfun #'notmuch-show-get-tags)
         (setq tagfun #'notmuch-show-tag))
        ('notmuch-tree-mode
         (setq gettagsfun #'notmuch-tree-get-tags)
         (setq tagfun #'notmuch-tree-tag))
        (_
         (user-error "Must be called from notmuch mode")))
      (if (member tag (funcall gettagsfun))
          (funcall tagfun (list (concat "-" tag)))
        (funcall tagfun (list (concat "+" tag))))
      ;; return whether it is now set or not
      (member tag (funcall gettagsfun))))
  (defun apm-notmuch-toggle-deleted ()
    "Toggle the deleted tag for the current message."
    (interactive)
    (apm-notmuch-toggle-tag "deleted"))
  (defun apm-notmuch-toggle-spam ()
    "Toggle the spam tag for the current message."
    (interactive)
    (if (apm-notmuch-toggle-tag "spam")
        (when-let ((url (notmuch-show-get-header :X-MailControl-ReportSpam)))
          (and (y-or-n-p "Do you also want to report this message as spam to mailcontrol? ")
               (let ((subject (notmuch-show-get-subject)))
                 (url-retrieve (concat url)
                               (lambda (s)
                                 (let ((status (url-http-symbol-value-in-buffer
                                                'url-http-response-status (current-buffer))))
                                   (pcase status
                                     (200 (message "Reported '%s' as spam" subject))
                                     (_ (user-error "Failed to report as spam: %s" status))))) ))))))

  ;; place sent in Sent/ maildir with sent tag and remove unread or inbox tags
  (setq notmuch-fcc-dirs "Sent +sent -unread -inbox")
  ;; place drafts in Drafts/ maildir
  (setq notmuch-draft-folder "Drafts")
  (setq notmuch-archive-tags '("-inbox" "-unread"))
  (setq mail-user-agent 'notmuch-user-agent)
  ;; ensure kernel team daily bug report emails display without wrapping
  (add-hook 'notmuch-show-insert-text/plain-hook 'notmuch-wash-convert-inline-patch-to-part)

  ;; add gnus-art emphasis highlighting too
  (with-eval-after-load 'gnus-art
    (defun apm-notmuch-wash-article-emphasize (_msg _depth)
      (dolist (elem gnus-emphasis-alist)
        (let ((regexp (car elem))
              (invisible (nth 1 elem))
              (visible (nth 2 elem))
              (face (nth 3 elem))
              (props (append '(article-type emphasis)
                             gnus-hidden-properties)))
          (goto-char (point-min))
          (while (re-search-forward regexp nil t)
            (when (and (match-beginning visible) (match-beginning invisible))
              (gnus-article-hide-text
               (match-beginning invisible) (match-end invisible) props)
              (gnus-article-unhide-text-type
               (match-beginning visible) (match-end visible) 'emphasis)
              (gnus-put-overlay-excluding-newlines
               (match-beginning visible) (match-end visible) 'face face)
              (gnus-add-wash-type 'emphasis)
              (goto-char (match-end invisible)))))))

    (add-hook 'notmuch-show-insert-text/plain-hook 'apm-notmuch-wash-article-emphasize))

  ;; ensure when viewing parts we use a tmp dir which all snaps and regular
  ;; applications can access
  (setq mm-tmp-directory (expand-file-name "~/tmp"))
  (unless (file-exists-p mm-tmp-directory)
    (make-directory mm-tmp-directory))

  ;; periodically refresh all notmuch buffers every 5 minutes - actually
  ;; this causes point to move and so loses our place in the inbox buffer
  ;; when refresh happens so don't do this for now...
  (when nil
    (defvar apm-notmuch-refresh-timer nil)
    (when (timerp apm-notmuch-refresh-timer)
      (cancel-timer apm-notmuch-refresh-timer))
    (setq apm-notmuch-refresh-timer
          (run-at-time t 300 #'notmuch-refresh-all-buffers))

    ;; also ensure cursor doesn't move when notmuch buffers get refreshed
    (define-advice notmuch-refresh-this-buffer (:around (orig-fun &rest args) save-excursion-around-notmuch-refresh)
      "Save cursor position around notmuch-refresh-this-buffer."
      (save-excursion
        (apply orig-fun args))))

  ;; add a few helpful custom saved search queries
  (add-to-list 'notmuch-saved-searches '(:name "cvewebbot" :query "from:noreply+security-tools@canonical.com and subject:\"CVE webbot process errors\"" :key "c")))

(use-package notmuch-transient
  :ensure t)

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

(use-package message
  :custom (message-make-forward-subject-function 'message-forward-subject-fwd))

(use-package minibuffer
  :config
  (setq completion-styles '(basic orderless))
  (setq completion-category-defaults nil)
  (setq completion-category-overrides '((file (styles . (partial-completion)))
                                        (eglot (styles orderless)))))

(use-package orderless
  ;; for vertico
  :ensure t)

(use-package org
  :pin gnu
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link)
         ("C-c C-w" . org-refile)
         :map org-mode-map
         ("M-s i" . consult-org-heading))
  ;; ensure we always load org at startup
  :demand t
  :preface
  (defun apm-org-mode-setup ()
    ;; add * and = as electric pairs
    (setq-local electric-pair-pairs (append electric-pair-pairs '((?\* . ?\*)
                                                                  (?\= . ?\=)))))
  :hook (org-mode . apm-org-mode-setup)
  :config
  (setq org-log-repeat nil)
  (setq org-log-into-drawer t)
  (setq org-pretty-entities t)
  ;; use org-appear instead
  (setq org-hide-emphasis-markers t)
  (setq org-directory (expand-file-name "~/org-files/"))
  (setq org-agenda-files (mapcar #'(lambda (f)
                                     (expand-file-name f org-directory))
                                 '("personal.org" "canonical.org"
                                   "inbox.org" "tickler.org" "notes.org")))
  ;; don't indent org document sections etc
  (setq org-adapt-indentation nil)
  (setq org-imenu-depth 4)
  ;; @ = add note with time
  ;; ! = record only time of state change
  ;; | = remaining keywords are final states
  (setq org-todo-keywords '((sequence "TODO(t)" "WORK(w)" "|" "DONE")
                            (sequence "|" "CANCELLED(c@)" "DELEGATED(G@)")))
  ;; ensure it is harder to inadvertently delete collapsed parts of org
  ;; documents
  (setq org-catch-invisible-edits 'smart)
  (setq org-ctrl-k-protect-subtree t)
  (add-to-list 'org-file-apps '("\\.webm\\'" . "xdg-open %s"))
  (add-to-list 'org-file-apps '("\\.aup3?\\'" . "audacity %s")))

(use-package org-contrib
  :pin nongnu
  :ensure t)

(use-package org-crypt
  :ensure org
  :config
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance '("crypt"))
  (setq org-crypt-key '("88E9530BCBDDC200517B5EB0F498D2D9DE7DAD9C")))

(use-package org-appear
  :ensure t
  :hook (org-mode . org-appear-mode))

(use-package org-autolist
  :ensure t
  :diminish org-autolist-mode
  :hook (org-mode . org-autolist-mode))

(use-package org-refile
  :ensure org-contrib
  :config
  (setq org-refile-targets '(("~/org-files/canonical.org" :maxlevel . 4)
                             ("~/org-files/personal.org" :maxlevel . 2)
                             ("~/org-files/someday.org" :level . 1)
                             ("~/org-files/tickler.org" :maxlevel . 1)
                             ("~/org-files/notes.org" :maxlevel . 2)))
  ;; allow to refile as top-level items in files
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil))

;; add support for man: links in org documents
(use-package ol-man
  :ensure org-contrib
  :pin nongnu)

(use-package ol-notmuch
  :ensure t
  :pin melpa)

(use-package org-agenda
  :ensure org-contrib
  :pin nongnu
  :preface
  (defun apm-org-agenda-file-notify (_event)
    "Rebuild appointments when _EVENT specifies any org agenda files change."
    (org-agenda-to-appt t))
  (defun apm-org-agenda-skip-all-siblings-but-first-todo ()
    "Skip all but the first TODO entry."
    (let ((should-skip-entry nil))
      (unless (string= "TODO" (org-get-todo-state))
        (setq should-skip-entry t))
      (save-excursion
        (while (and (not should-skip-entry) (org-goto-sibling t))
          (when (string= "TODO" (org-get-todo-state))
            (setq should-skip-entry t))))
      (when should-skip-entry
        (or (outline-next-heading)
            (goto-char (point-max))))))
  :custom
  ;; add remove-match so grid lines which are already present in an entry
  ;; are not shown
  (org-agenda-time-grid '((daily today require-timed remove-match)
                          (800 1000 1200 1400 1600 1800 2000)
                          "......" "----------------"))
  :config
  ;; when modifying agenda files make sure to update appt
  (require 'filenotify)
  (dolist (file org-agenda-files)
    (file-notify-add-watch file '(change) #'apm-org-agenda-file-notify))
  (setq org-agenda-clockreport-parameter-plist '(:link t :maxlevel 3 :narrow 80))
  ;; when showing agenda, jump to now
  (add-hook 'org-agenda-finalize-hook
            #'org-agenda-find-same-or-today-or-agenda 90)
  ;; rebuild appointments now
  (org-agenda-to-appt t)
  (setq org-agenda-custom-commands
        '(("i" "TODO from inbox" todo "TODO"
           ((org-agenda-files '("~/org-files/inbox.org"))))
          ("c" "TODO from canonical" todo "TODO"
           ((org-agenda-files '("~/org-files/canonical.org"))))
          ("I" "Next TODO from all" todo "TODO" (
            (org-agenda-skip-function #'apm-org-agenda-skip-all-siblings-but-first-todo))))))

(use-package org-capture
  :preface
  :after org
  :config
  (let ((inbox-org (expand-file-name "inbox.org" org-directory))
        (tickler-org (expand-file-name "tickler.org" org-directory))
        (canonical-org (expand-file-name "canonical.org" org-directory)))
    (setq org-capture-templates
          `(("t" "todo" entry (file ,inbox-org)
             "* TODO %i%?
- %a")
            ("I" "tickler" entry (file ,tickler-org)
             "* %i%?
%U
- %a")
            ("p" "project" entry (file ,canonical-org)
             "* TODO %i%?
- %a")
            ("m" "meeting" entry (file+headline ,canonical-org "Meetings")
             "* %^{meeting title}
%^{meeting day+time}T
- %a%?")
            ("R" "snap-store-review" entry (file+olp ,canonical-org "Manual snap reviews")
             "** %^{snap name}
- %a%?" :clock-in t :clock-keep t)
            ("D" "snap-store-discussion" entry (file+olp ,canonical-org "snap forum store request discussions")
             "** %^{snap name}
- %a%?" :clock-in t :clock-keep t)
            ("T" "snap-store-tallied-processed" entry (file+olp ,canonical-org "snap forum store requests processed/tallied")
             "** %^{snap name}
- %a%?" :clock-in t :clock-keep t)
            ("P" "snapd-pr-review" entry (file+olp ,canonical-org "snapd PR reviews")
             "* [[https://github.com/snapcore/snapd/pull/%\\1][snapd PR #%^{number} %^{title}]]
- https://github.com/snapcore/snapd/pull/%\\1%?")
            ("r" "protocol" entry (file ,inbox-org)
             "* %^{Title}
Source: %u, %c
#+BEGIN_QUOTE
%i
#+END_QUOTE

%?")
            ("L" "Protocol Link" entry (file ,inbox-org)
             "* %? [[%:link][%:description]]
Captured On: %U")))))

(use-package org-clock
  :after org
  ;; ensure we always run org-clock-persistence-insinuate below
  :demand t
  :bind (("C-c g" . org-clock-goto)
         ("C-c i" . org-clock-in)
         ("C-c o" . org-clock-out))
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
        org-clock-persist-file (expand-file-name "org-clock-save.el" org-directory))
  (setq org-clock-x11idle-program-name "xprintidle")
  ;; reload any saved org clock information on startup
  (org-clock-persistence-insinuate))

(use-package org-clock-convenience
  :ensure t
  :bind (:map org-agenda-mode-map
              ("S-<up>" . org-clock-convenience-timestamp-up)
              ("S-<down>" . org-clock-convenience-timestamp-down)))

(use-package org-duration
  :ensure org-contrib
  ;; don't show days, only total hours as maximum value
  :config (setq org-duration-format (quote h:mm)))

(use-package orgit
  :ensure t)

(use-package org-src
  :ensure org-contrib
  :pin nongnu
  :config
  ;; preserve indentation of org src blocks
  (setq org-src-preserve-indentation t))

(use-package org-table-sticky-header
  :ensure t
  :after org
  :defer t
  :diminish org-table-sticky-header-mode
  :hook ((org-mode . org-table-sticky-header-mode)))

(use-package org-wild-notifier
  :ensure t
  :config (org-wild-notifier-mode 1))

(use-package ox-hugo
  :ensure t)

(use-package paradox
  :ensure t
  :commands (paradox-list-packages)
  :init (setq paradox-execute-asynchronously nil)
  :config
  (define-advice paradox-list-packages (:before (_no-fetch) set-github-token)
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
  (paradox-enable))

(use-package paredit
  :diminish paredit-mode
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
  :load-path "/snap/pdf-tools/current/usr/share/emacs/site-lisp/pdf-tools"
  :custom
  (pdf-info-epdfinfo-program "pdf-tools.epdfinfo")
  (pdf-info-epdfinfo-error-filename (expand-file-name "~/snap/pdf-tools/common/epdfinfo.log")))

(use-package pixel-scroll
  :config (if (fboundp 'pixel-scroll-precision-mode)
              (pixel-scroll-precision-mode 1)
            (pixel-scroll-mode 1)))

(use-package pod-mode
  :load-path "vendor/"
  :mode ("\\.pod$" . pod-mode))

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

(use-package project
  :ensure t
  :pin gnu
  :demand t
  ;; try forcing magit to be integrated with project-switch-commands
  :config (with-eval-after-load 'magit
            (require 'magit-extras))
  ;; make debian source packages work as projects
  (defun apm-project-try-debian (dir)
    "Find a debian source package from DIR."
    (let ((dir (locate-dominating-file dir "debian/control")))
      (and dir (cons 'debian dir))))

  (cl-defmethod project-root ((project (head debian)))
    (cdr project))

  (add-hook 'project-find-functions #'apm-project-try-debian))


(use-package project-mode-line-tag
  :ensure t
  :config (project-mode-line-tag-mode 1))

(use-package python
  :defer t
  :custom
  (python-indent-offset 4)
  (python-check-command (executable-find "flake8"))
  (python-flymake-command '("flake8" "-")))

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

(use-package recentf
  :config (recentf-mode 1))

(use-package region-state
  :ensure t
  :config (region-state-mode 1))

(use-package ripgrep
  :ensure t
  :ensure-system-package (rg . ripgrep))

(use-package rmsbolt
  :ensure t)

(use-package rnc-mode
  :ensure t)

(use-package rust-mode
  :ensure t)

;; save minibuffer history
(use-package savehist
  :init (savehist-mode 1)
  :config (setq history-length 25))

(use-package saveplace
  :config
  (setq save-place-file (expand-file-name ".places" user-emacs-directory))
  (save-place-mode 1))

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

(use-package shr
  :custom
  (shr-use-colors nil)
  (shr-use-fonts nil))

(use-package sideline-flymake
  :ensure t
  :hook (flymake-mode . sideline-mode)
  :diminish sideline-mode
  :init (setq sideline-backends-right '(sideline-flymake)))

(use-package simple
  :defer t
  :diminish visual-line-mode
  :init
  ;; save whatever is in the system clipboard to the kill ring before
  ;; killing something else into the kill ring
  (setq save-interprogram-paste-before-kill t)
  (setq next-error-message-highlight t)
  (setq visual-line-fringe-indicators
        '(left-curly-arrow right-curly-arrow)))

(use-package smiley
  :custom (smiley-style 'emoji))

(use-package smtpmail
  ;; store password using secret-tool as follows:
  ;; secret-tool store --label='Canonical SMTP' host smtp.canonical.com port 587 user amurray
  ;; then enter PASSWORD
  :custom
  (smtpmail-smtp-user "amurray")
  (smtpmail-local-domain "canonical.com")
  (smtpmail-smtp-server "smtp.canonical.com")
  (smtpmail-smtp-service 587))

(use-package so-long
  :config (global-so-long-mode 1))

(use-package spdx
  :ensure t)

(use-package strace-mode
  :ensure t)

(use-package sudo-edit
  :ensure t
  :commands (sudo-edit))

(use-package suggest
  :ensure t)

(use-package syslog-mode
  :ensure t)

(use-package systemd
  :ensure t)

(use-package time
  :bind ((("C-c z" . world-clock)))
  :config (let ((team '(("Australia/Adelaide" . ("amurray"))
                        ("Europe/Brussels" . ("ebarretto"))
                        ("Europe/London" . ("ChrisCoulson"))
                        ("Brazil/East" . ("deafpool" "leosilva" "pfsmorigo"))
                        ("America/Argentina/Buenos_Aires" . ("emitorino"))
                        ("US/Eastern" . ("dan" "shoonmcgregor" "sid" "vineetha1"))
                        ("Canada/Eastern" . ("mdeslaur"))
                        ("US/Pacific" . ("jj" "sarnold" "sbeattie")))))
            ;; validate team
            (dolist (member team)
              (unless (file-exists-p (expand-file-name (car member) "/usr/share/zoneinfo"))
                (user-error "TZ %s does not exist!" (car member))))
            (require 'erc-hl-nicks)
            (setq zoneinfo-style-world-list
                    ;; make nicks stand out
                    (mapcar #'(lambda (member)
                                (list (car member)
                                      (string-join (cdr member) ", ")))
                            team)))
  (setq display-time-default-load-average nil)
  (setq display-time-use-mail-icon t)
  (setq display-time-day-and-date t)
  (setq display-time-24hr-format t)
  (display-time))

(use-package tzc
  :ensure t
  :custom (tzc-favourite-time-zones-alist '(("UTC+0000" "UTC")
                                            ("Australia/Adelaide" "Adelaide"))))

(use-package uct
  :load-path "~/ubuntu/git/ubuntu-cve-tracker/scripts/"
  :demand t
  :bind (("C-c u u" . uct)
         ("C-c f c" . uct-find-cve)
         ("C-c u k" . uct-kernel-signoff-at-point))
  :init
  (add-hook 'magit-process-prompt-functions #'uct-magit-process-check-syntax-prompt-hook))

(use-package udev-mode
  :ensure t)

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config (global-undo-tree-mode 1))

(use-package uniquify
  :config (setq uniquify-buffer-name-style 'post-forward
                uniquify-separator ":"
                uniquify-after-kill-buffer-p t
                uniquify-ignore-buffers-re "^\\*"))

(use-package usn
  :load-path "~/ubuntu/git/usn-tool/"
  :commands (usn-compose-from-draft)
  :bind (("C-c f u" . usn-show-at-point)))

(use-package uvt
  :load-path "~/ubuntu/git/ubuntu-qa-tools/vm-tools/"
  :bind (("C-c v" . uvt)))

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

(use-package vertico
  :ensure t
  :hook ((after-init . vertico-mode))
  :custom (vertico-cycle t))

(use-package vertico-repeat
  :ensure vertico
  :hook (minibuffer-setup . vertico-repeat-save)
  :bind (("C-c r v" . vertico-repeat)))

(use-package vimrc-mode
  :ensure t
  :mode (("\\.vim\\(rc\\)?\\'" . vimrc-mode)))

(use-package virtual-auto-fill
  :ensure t
  :hook ((markdown-mode . virtual-auto-fill-mode)))

(use-package vterm
  :load-path "/snap/emacs-vterm/current/"
  :bind (("C-x m" . vterm)))

(use-package webpaste
  :ensure t
  :bind (("C-c C-p C-b" . webpaste-paste-buffer)
         ("C-c C-p C-r" . webpaste-paste-region)
         ("C-c C-p C-p" . webpaste-paste-region-or-region))
  :demand t
  :config
  (setq webpaste-paste-confirmation t)
  (setq webpaste-provider-priority '("paste.ubuntu.com")))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config (which-key-mode))

(use-package whitespace
  :diminish whitespace-mode global-whitespace-mode
  :init (setq-default whitespace-style
                      '(face tabs tab-mark trailing missing-newline-at-eof))
  :config
  ;; whitespace-mode is not useful for erc or magit-log buffers
  (setq whitespace-global-modes '(not erc-mode magit-log-mode vterm-mode))
  (global-whitespace-mode 1))

(use-package whitespace-cleanup-mode
  :ensure t
  :diminish whitespace-cleanup-mode
  :config (global-whitespace-cleanup-mode 1))

(use-package world-time-mode
  :ensure t)

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

(use-package xref
  :ensure t
  :custom (xref-search-program 'ripgrep))

(use-package ztree
  :ensure t
  :bind (("C-x C-d" . ztree-dir))
  :config
  (setq ztree-draw-unicode-lines t)
  (setq ztree-show-number-of-children t))

(provide 'init)

;;; init.el ends here
