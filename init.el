;;; init.el --- Starting point for Alex Murray's Emacs Configuration -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; prefer newer non-byte compiled sources to older byte compiled ones
(setq load-prefer-newer t)

;;; Package management
(require 'package)

;; must be set before loading use-package
(defvar use-package-enable-imenu-support t)
(setq use-package-enable-imenu-support t)
;; compute stats info - needs to be set before loading use-package as well - see
;; stats with `use-package-report'
;;(setq use-package-compute-statistics t)
;; uncomment to debug package loading times
;; (setq use-package-verbose t)

(eval-and-compile
  (require 'use-package))

(use-package package
  :custom (package-install-upgrade-built-in t)
  :config
  ;; add melpa archive and gnu-devel
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  (add-to-list 'package-archives '("gnu-devel" . "https://elpa.gnu.org/devel/"))

  ;; use gnu over non-gnu over melpa over gnu-devel
  (setq package-archive-priorities '(("gnu" . 4) ("nongnu" . 2) ("melpa" . 1) ("gnu-devel" . 0)))

  ;; but use erc from gnu-devel
  (setq package-pinned-packages '((erc . "gnu-devel"))))

(use-package gnu-elpa-keyring-update
  :ensure t)

;; load org early so everything else gets compiled against it
(use-package org
  :pin gnu
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link)
         ("C-c j" . consult-org-agenda)
         ("C-c C-w" . org-refile)
         :map org-mode-map
         ("M-s i" . consult-org-heading))
  ;; ensure we always load org at startup
  :demand t
  :preface
  (defun apm-org-mode-setup ()
    ;; add * = ~ as electric pairs
    (setq-local electric-pair-pairs (append electric-pair-pairs '((?\* . ?\*)
                                                                  (?\= . ?\=)
                                                                  (?\~ . ?\~)))))
  :hook (org-mode . apm-org-mode-setup)
  :config
  (setq org-log-repeat nil)
  (setq org-log-into-drawer t)
  (setq org-pretty-entities t)
  ;; org-appear is disabled to show emphasis markers instead
  (setq org-hide-emphasis-markers nil)
  (setq org-directory (expand-file-name "~/git/org-files/"))
  (setq org-agenda-files (mapcar #'(lambda (f)
                                     (expand-file-name f org-directory))
                                 '("personal.org"
                                   "inbox.org" "tickler.org" "notes.org")))
  ;; don't indent org document sections etc
  (setq org-adapt-indentation nil)
  (setq org-imenu-depth 4)
  ;; @ = add note with time
  ;; ! = record only time of state change
  ;; | = remaining keywords are final states
  (setq org-todo-keywords '((sequence "TODO(t)" "WORK(w)" "|" "CANCELLED(c@)" "DELEGATED(G@)" "DONE")))
  ;; ensure it is harder to inadvertently delete collapsed parts of org
  ;; documents
  (setq org-catch-invisible-edits 'smart)
  (setq org-ctrl-k-protect-subtree t)
  (add-to-list 'org-file-apps '("\\.webm\\'" . "xdg-open %s"))
  (add-to-list 'org-file-apps '("\\.aup3?\\'" . "audacity %s")))

(use-package erc
  :pin gnu-devel
  :ensure t
  :preface
  (eval-when-compile
    (require 'erc-log)
    (require 'erc-match))

  (defun apm-prompt-to-connect-to-irc ()
    "Prompt to connect to irc."
    (interactive)
    (let ((connectivity (string-trim
                         (shell-command-to-string "nmcli networking connectivity"))))
      (if (string= connectivity "full")
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
                     :nick "amurray" :password (concat "amurray/libera:"
                                                       (auth-source-pick-first-password
                                                        :user "amurray"
                                                        :host "znc.secret.server"
                                                        :port "7076"))))
        (message "Network connectivity is %s, not prompting to connect to IRC" connectivity)))

    (defgroup apm-erc nil
      "apm's erc customisations."
      :group 'erc))

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

  ;; :hook ((after-init . apm-prompt-to-connect-to-irc))
  :bind (:map erc-mode-map
              ("C-c f e" . apm-erc-find-logfile)
              ("M-s e" . apm-occur-in-erc)
              :map erc-fill-wrap-mode-map
              ("C-c a" . org-agenda))
  :config
  (eval-and-compile
    (require 'erc-button)
    (require 'erc-desktop-notifications)
    (require 'erc-fill)
    (require 'erc-join)
    (require 'erc-log)
    (require 'erc-match)
    (require 'erc-nicks)
    (require 'erc-networks)
    (require 'erc-notify)
    (require 'erc-services)
    (require 'erc-track))

  (setq erc-user-full-name user-full-name)
  (setq erc-nick (list user-login-name "alexmurray"))

  ;; make prompt more dynamic
  (setq erc-prompt #'erc-prompt-format)
  (setq erc-prompt-for-nickserv-password nil)

  (setq erc-use-auth-source-for-nickserv-password t)

  (setq erc-autojoin-timing 'ident)

  ;; since we connect to oftc directly, we need to autojoin channels there
  ;; - not needed for libera (since we use ZNC)
  (setq erc-autojoin-channels-alist nil)
  (setq erc-fill-function #'erc-fill-wrap)
  ;; account for really long names
  (setq erc-fill-static-center 22)
  ;; this fits on a dual horizontal split on my laptop
  (setq erc-fill-column 110)

  ;; use sensible buffer names with server as well
  (setq erc-rename-buffers t)

  ;; try harder to reconnect but wait longer each time since it may take a
  ;; while to get a DHCP lease etc
  (setq erc-server-reconnect-function #'erc-server-delayed-check-reconnect)
  (setq erc-server-auto-reconnect t)

  (setq erc-scrolltobottom-all t)

  (add-to-list 'erc-modules 'button)
  (add-to-list 'erc-modules 'log)
  (add-to-list 'erc-modules 'match)
  (add-to-list 'erc-modules 'nicks)
  (add-to-list 'erc-modules 'notifications)
  (add-to-list 'erc-modules 'scrolltobottom)
  (add-to-list 'erc-modules 'services)
  (add-to-list 'erc-modules 'services-regain)
  (add-to-list 'erc-modules 'spelling)
  (erc-update-modules)

  ;; format nicknames to show if user has voice(+), owner (~), admin (&),
  ;; operator (@)
  (setq erc-show-speaker-membership-status t)

  (setq erc-keywords '("alexmurray" "cve" "vulnerability" "apparmor" "seccomp" "exploit" "security" "esm" "@here" "@all" "@channel" "@security"))

  ;; when joining don't bring to front
  (setq erc-join-buffer 'bury)

  ;; ensure erc-track plays nicer with minions - https://github.com/tarsius/minions/issues/22
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
  ;; emacs channels are noisy
  (setq erc-track-exclude '("#emacs" "#emacsconf" "#ubuntu"))
  (setq erc-track-shorten-function nil)

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

  ;; make sure we identify to nickserv
  (erc-services-mode 1)

  ;; change header line face when disconnected
  (setq erc-header-line-face-method
        #'apm-erc-update-header-line-show-disconnected)

  ;; make sure any privmsg (which is via query buffers) show up as urgent
  ;; in track list
  (defadvice erc-track-select-mode-line-face (around erc-track-find-face-promote-query activate)
    (if (erc-query-buffer-p)
        (setq ad-return-value 'erc-current-nick-face)
      ad-do-it)))

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

(use-package diminish
  :ensure t)

(use-package bind-key
  :ensure t)


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
  (let ((theme (if (or (and (integerp dark) (= dark 1))
                       (and (stringp dark) (string= dark "prefer-dark")))
                   apm-preferred-dark-theme
                 apm-preferred-light-theme)))
    (when (and theme (not (eq theme apm-preferred-theme)))
      (when apm-preferred-theme
        (disable-theme apm-preferred-theme))
      (setq apm-preferred-theme theme)
      (load-theme apm-preferred-theme t)
      (run-hooks 'apm-load-preferred-theme-hook))))

(defun apm-desktop-portal-settings-changed (path var value)
  "Update preferred theme based on VALUE of VAR at PATH."
  (when (and (or (string-equal path "org.freedesktop.appearance")
                 (string-equal path "org.gnome.desktop.interface"))
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
    (let ((custom--inhibit-theme-enable nil))
      (if (eq apm-preferred-theme apm-preferred-dark-theme)
          (custom-theme-set-faces
           apm-preferred-theme
           `(erc-keyword-face ((t (:weight bold :foreground ,(doom-color 'yellow))))))
        (custom-theme-set-faces
         apm-preferred-theme
         `(erc-keyword-face ((t (:weight bold :foreground ,(doom-color 'yellow))))))))
    ;; ensure erc-nicks uses defined colors from doom-theme
    (with-eval-after-load 'erc-nicks
      (dolist (buffer-name '("Libera.Chat" "OFTC"))
        (when-let (buffer (get-buffer buffer-name))
          (with-current-buffer buffer
            (erc-nicks-refresh nil))))))
  :custom
  (doom-one-padded-modeline t)
  :config
  (doom-themes-visual-bell-config)
  (require 'org-indent) ;; https://github.com/doomemacs/themes/issues/807
  (doom-themes-org-config)
  (setq apm-preferred-dark-theme 'doom-vibrant)
  (setq apm-preferred-light-theme 'doom-one-light)
  ;; set customisations after loading the theme
  (add-hook 'apm-load-preferred-theme-hook #'apm-setup-doom-themes)
  (apm-set-preferred-theme))

(use-package kanagawa-themes
  :ensure t
  :disabled t
  :preface
  (defun apm-setup-kanagawa-themes ()
    (let ((custom--inhibit-theme-enable nil))
      (custom-theme-set-faces
       'kanagawa-wave
       ;; `(erc-input-face ((((class color) (min-colors 89)) (:foreground ,(car (alist-get 'wave-red kanagawa-dark-palette))))))
       ;; `(erc-keyword-face ((((class color) (min-colors 89)) (:foreground ,(car (alist-get 'ronin-yellow kanagawa-dark-palette))))))
       ;; `(erc-notice-face ((((class color) (min-colors 89)) (:foreground ,(car (alist-get 'sumi-ink-4 kanagawa-dark-palette))))))
       ;; `(erc-prompt-face ((((class color) (min-colors 89)) (:background ,(car (alist-get 'wave-blue-1 kanagawa-dark-palette))))))
       ;; `(erc-timestamp-face ((((class color) (min-colors 89)) (:foreground ,(car (alist-get 'autumn-green kanagawa-dark-palette))))))
       ;; `(message-mml ((((class color) (min-colors 89)) (:foreground ,(car (alist-get 'spring-green kanagawa-dark-palet
       `(sh-heredoc ((((class color) (min-colors 89)) (:weight bold :foreground ,(car (alist-get 'autumn-yellow kanagawa-themes-color-palette-list)))))))))
  :custom ((kanagawa-themes-org-height nil)
           (kanagawa-themes-org-height nil))
  :config
  (setq apm-preferred-dark-theme 'kanagawa-wave)
  (setq apm-preferred-light-theme 'kanagawa-wave)
  ;; set customisations after loading the theme
  (add-hook 'apm-load-preferred-theme-hook #'apm-setup-kanagawa-themes)
  (apm-set-preferred-theme))

(use-package alert
  :ensure t
  :config (when (eq system-type 'gnu/linux)
            (setq alert-default-style 'notifications)))

;;; General settings etc from C source so associate settings with emacs itself
(use-package emacs
  :preface
  ;; used in some of my yasnippet snippets
  (defun apm-camelize (s &optional delim)
    "Convert under_score string S to CamelCase string with optional DELIM."
    (interactive "s")
    (mapconcat 'identity (mapcar
                          #'(lambda (word) (capitalize (downcase word)))
                          (split-string s (if delim delim "_"))) ""))
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
  :config
  ;; use pipes for subprocess communication
  (setq-default process-connection-type nil)
  ;; performance increases as per https://emacs-lsp.github.io/lsp-mode/page/performance/
  (setq gc-cons-threshold 100000000)
  (setq read-process-output-max (* 1024 1024)) ;; 1mb

  ;; personalisation
  (setq user-full-name "Alex Murray")
  (setq user-mail-address "murray.alex@gmail.com")

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

  ;; system font doesn't seem to scale properly in emacs so set it manually
  (let ((preferred-font "Ubuntu Sans Mono-11"))
      (if (daemonp)
          (add-to-list 'default-frame-alist `(font . ,preferred-font))
        (if (null (font-info preferred-font))
            (alert "Please apt install fonts-ubuntu")
          ;; apply this to all existing and future frames
          (set-frame-font preferred-font nil t))))

  ;; make emacs less laggy
  (setq inhibit-compacting-font-caches t)

  (set-language-environment "UTF-8")

  ;; prompt when trying to switch out of a dedicated window
  (setq switch-to-buffer-in-dedicated-window 'prompt)

  ;; ensure scrolling forwards / backwards preserves original location such that
  ;; they undo each other
  (setq scroll-preserve-screen-position 'always)

  (bind-key [remap fill-paragraph] #'endless/fill-or-unfill)

  ;; case insensitive completion everywhere
  (setq read-file-name-completion-ignore-case t)
  (setq read-buffer-completion-ignore-case t)
  (setq completion-ignore-case t)

  ;; show empty lines
  (setq indicate-empty-lines t))

;;; Packages
(use-package abbrev
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
  :config (global-aggressive-indent-mode))

(use-package all-the-icons
  :ensure t
  :config
  (unless (file-exists-p (expand-file-name "~/.local/share/fonts/all-the-icons.ttf"))
    (alert "Installing all-the-icons font...")
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

(use-package apm-misc
  :load-path "lisp/"
  :bind (("C-c b l" . apm-browse-lp-bug-at-point)))

(use-package apparmor-mode
  :ensure t
  :config (add-to-list 'auto-mode-alist
                       `(,(expand-file-name "~/git/apparmor/profiles/apparmor.d/") . apparmor-mode)))

(use-package apropos
  :bind ("C-h a" . apropos))

(use-package arc-mode
  :config  (add-to-list 'auto-mode-alist '("\\.snap\\'" . archive-mode)))

(use-package asn1-mode
  :ensure t
  :defer t
  :mode (("\\.asn1?$" . asn1-mode)))

(use-package autorevert
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
  ;; prefer encrypted auth source to non-encrypted
  :init
  (setq auth-sources '("~/.authinfo.gpg" "~/.authinfo" "~/.netrc")))

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
  :config
  ;; beginend defines lots of different modes so diminish them all
  (dolist (m beginend-modes)
    (diminish (cdr m)))
  (beginend-global-mode 1))

(use-package bbdb
  :ensure t
  :config (bbdb-initialize 'message))

(use-package breadcrumb
  :ensure t
  :config (breadcrumb-mode 1))

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
                     ((string-prefix-p "SEC-" prefix)
                      (format "https://warthogs.atlassian.net/browse/SEC-%s" id))
                     (t (error (concat "Unknown bug prefix '%s'" prefix))))))
  :init
  (eval-when-compile
    (require 'bug-reference))
  (setq bug-reference-url-format #'apm-bug-reference-url-format
        bug-reference-bug-regexp "\\<\\(\\(\\([Ll][Pp]:?\\|bug\\) #?\\|CVE[ -]\\|[UL]SN[ -]\\|SEC-\\)\\([0-9Y][0-9N-]*\\)\\)\\>"))

(use-package calc
  :defer t
  ;; https://emacs.ch/@galdor/109759892132784831
  :custom (calc-multiplication-has-precedence nil))

(use-package calendar
  :custom (calendar-week-start-day 1)
  :config
  ;; show ISO week numbers in calendar
  (copy-face font-lock-constant-face 'calendar-iso-week-face)
  (set-face-attribute 'calendar-iso-week-face nil
                      :height 1.0 :foreground "salmon")
  (setq calendar-intermonth-text
        '(propertize
          (format "%2d"
                  (car
                   (calendar-iso-from-absolute
                    (calendar-absolute-from-gregorian (list month day year)))))
          'font-lock-face 'calendar-iso-week-face))

  (copy-face 'default 'calendar-iso-week-header-face)
  (set-face-attribute 'calendar-iso-week-header-face nil
                      :height 1.0 :foreground "salmon")
  (setq calendar-intermonth-header
        (propertize "Wk"
                    'font-lock-face 'calendar-iso-week-header-face)))

(use-package cargo
  :ensure t
  :init (unless (executable-find "cargo")
          (alert "Please install the rustup snap"))
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
  (defun apm-c-ts-mode-setup ()
    "Tweaks and customisations for `c-mode'."
    (let ((filename (buffer-file-name)))
      (if (and filename (string-match-p (expand-file-name "~/git/apparmor-kernel") filename))
          ;; upstream linux kernel style actually uses tabs... urgh
          (progn
            (setq indent-tabs-mode t)
            (setq c-ts-mode-indent-offset 8))))
    ;; always show trailing whitespace
    (setq show-trailing-whitespace t)
    ;; ensure fill-paragraph takes doxygen @ markers as start of new
    ;; paragraphs properly
    (setq paragraph-start "^[ ]*\\(//+\\|\\**\\)[ ]*\\([ ]*$\\|@param\\)\\|^\f"))
  :hook (((c-ts-mode c++-ts-mode) . apm-c-ts-mode-setup))
  :custom
  (c-ts-mode-indent-offset 4)
  (c-ts-mode-indent-style "linux")
  :config
  ;; treat linux style as safe for local variable
  (add-to-list 'safe-local-variable-values '(c-ts-mode-indent-style . linux)))

(use-package check-cves-mode
  :load-path "~/ubuntu/git/ubuntu-cve-tracker/scripts/"
  :mode ("check-cves\\..*\\'" . check-cves-mode))

(use-package clang-format
  :ensure t
  :hook '((c-mode-common . clang-format-on-save-mode)
          (c-ts-mode . clang-format-on-save-mode)))

(use-package comint
  :hook ((comint-output-filter-functions . comint-osc-process-output)))

(use-package comint-mime
  :ensure t
  :hook ((shell-mode . comint-mime-setup)))

(use-package copy-as-format
  :ensure t
  ;; add bindings for github, gitlab, jira, markdown, org-mode, and slack (aka
  ;; mattermost)
  :bind (("C-c w w" . copy-as-format)
         ("C-c w g" . copy-as-format-github)
         ("C-c w l" . copy-as-format-gitlab)
         ("C-c w j" . copy-as-format-jira)
         ("C-c w m" . copy-as-format-markdown)
         ("C-c w o" . copy-as-format-org-mode)
         ("C-c w s" . copy-as-format-slack)))

(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 3)
  :init
  (global-corfu-mode 1)
  (setq tab-always-indent 'complete)
  ;; cycle with at least 3 candidates
  (setq completion-cycle-threshold 3))

(use-package corfu-popupinfo
  :ensure corfu
  :hook (corfu-mode . corfu-popupinfo-mode)
  :bind (:map corfu-map
              ("M-p" . corfu-popupinfo-scroll-down)
              ("M-n" . corfu-popupinfo-scroll-up)
              ("M-d" . corfu-popupinfo-toggle)))

(use-package cape
  :ensure t
  :config
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package compile
  :hook ((shell-mode . compilation-shell-minor-mode))
  ;; automatically scroll to first error on output
  :config (setq compilation-scroll-output 'first-error)
  ;; use compilation-mode for _source.build files
  (add-to-list 'auto-mode-alist '("_\\(amd64\\|source\\)\\.build\\'" . compilation-mode)))

(use-package completion-preview
  :init (global-completion-preview-mode 1)
  :disabled t ; doesn't yet play well with corfu
  :bind (:map completion-preview-active-mode-map
              ("M-n" . completion-preview-next-candidate)
              ("M-p" . completion-preview-prev-candidate)
              ("M-i" . completion-preview-insert))
  :custom (completion-preview-minimum-symbol-length 2)
  :config
  ;; Org mode has a custom `self-insert-command'
  (push 'org-self-insert-command completion-preview-commands))

(use-package consult
  :ensure t
  :bind (
         ("C-c h" . consult-history)
         ("C-c s" . consult-clock-in)
         ("C-x b" . consult-buffer)
         ("C-x p b" . consult-project-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("M-i" . consult-imenu)
         ("M-s i" . consult-imenu)
         ("M-s l" . consult-line)
         ("M-s o" . consult-outline)
         ("C-y". consult-yank-replace)
         ("M-y". consult-yank-pop)
         ("M-g e" . consult-compile-error)
         ("M-g g" . consult-goto-line)   ;; orig. goto-line
         ("M-g M-g" . consult-goto-line) ;; orig. goto-line
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s f" . consult-find)
         ("M-s L" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         ;; https://emacsredux.com/blog/2021/11/25/redo-complex-command-with-consult/
         ([remap repeat-complex-command] . consult-complex-command)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)   ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history) ;; orig. isearch-edit-string
         ("M-s l" . consult-line)
         :map minibuffer-mode-map
         ("C-y" . yank))
  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  ;; from https://github.com/minad/consult/wiki#org-clock with a minor fix to
  ;; replace 'consult--candidate text property with 'org-marker since it seems
  ;; to be broken for me as provided upstream
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
                     :preview-key "M-."
                     :group
                     (lambda (cand transform)
                       (let* (;; (marker (get-text-property 0 'consult--candidate cand))
                              (marker (get-text-property 0 'org-marker cand))
                              (name (if (member marker org-clock-history)
                                        "*Recent*"
                                      (buffer-name (marker-buffer marker)))))
                         (if transform (substring cand (1+ (length name))) name)))))

(use-package consult-eglot
  :ensure t
  :after (consult eglot)
  :bind (:map eglot-mode-map
              ("M-s i" . consult-eglot-symbols)))

(use-package consult-xref-stack
  :vc (:url "https://github.com/brett-lempereur/consult-xref-stack/")
  :bind (("C-," . consult-xref-stack-backward)))

(use-package copilot
  :vc (:url "https://github.com/zerolfx/copilot.el/")
  :demand t
  :preface
  (defun apm-copilot-complete-or-accept ()
    (interactive)
    (if (copilot--overlay-visible)
        (copilot-accept-completion)
      (copilot-complete)))
  :bind (:map copilot-mode-map
              ;; enable binding is normal copilot map as doing it just in the
              ;; completion map doesn't seem sufficient and gets overridden
              ;; sometimes (and hence they don't work)
              ("C-<tab>" . apm-copilot-complete-or-accept)
              ("C-TAB" . apm-copilot-complete-or-accept)
              ("C-<backtab>" . copilot-accept-completion-by-word)
              ("C-<iso-lefttab>" . copilot-accept-completion-by-word)
              ("C-<end>" . copilot-accept-completion-by-line)
              :map copilot-completion-map
              ("C-<tab>" . apm-copilot-complete-or-accept)
              ("C-TAB" . apm-copilot-complete-or-accept)
              ("C-<backtab>" . copilot-accept-completion-by-word)
              ("C-<iso-lefttab>" . copilot-accept-completion-by-word)
              ("C-<end>" . copilot-accept-completion-by-line)
              ("M-n" . copilot-next-completion)
              ("M-p" . copilot-previous-completion))
  :hook ((prog-mode . copilot-mode))
  :custom (copilot-server-executable "/snap/bin/copilot-language-server"))

(use-package crontab-mode
  :ensure t)

;; show suspicious c constructs automatically
(use-package cwarn
  :init (global-cwarn-mode 1))

(use-package cycle-at-point
  :ensure t
  :bind (("M-p" . cycle-at-point)))

(use-package deadgrep
  :ensure t
  :init (unless (executable-find "rg")
          (alert "Please apt install ripgrep")))

(use-package debian-el
  :ensure t)

(use-package debbugs
  :ensure t)

(use-package define-word
  :ensure t
  :bind (("C-c d" . define-word-at-point)
         ("C-c D" . define-word)))

(use-package devhelp
  :ensure t)

(use-package diff-mode
  :mode (("\\.debdiff\\'" . diff-mode)))

(use-package dired
  :custom
  (dired-dwim-target t)
  (dired-vc-rename-file t)
  :hook ((dired-mode . dired-hide-details-mode)))

(use-package disk-usage
  :ensure t)

(use-package display-fill-column-indicator
  :hook ((prog-mode . display-fill-column-indicator-mode)
         (text-mode . display-fill-column-indicator-mode)))

(use-package disproject
  :ensure t
  ;; Replace `project-prefix-map' with `disproject-dispatch'.
  :bind (:map ctl-x-map
              ("p" . disproject-dispatch)))

(use-package dpkg-dev-el
  :init (unless (executable-find "debputy")
          (alert "Please apt install dh-debputy"))
  :ensure t)

(use-package debian-autopkgtest-control-mode
  :ensure dpkg-dev-el
  :hook ((debian-autopkgtest-control-mode . eglot-ensure))
  :config (with-eval-after-load 'eglot
            (add-to-list 'eglot-server-programs '(debian-autopkgtest-control-mode . ("debputy" "lsp" "server")))))

(use-package debian-changelog-mode
  :ensure dpkg-dev-el
  :hook ((debian-changelog-mode . eglot-ensure))
  :config (let ((pockets '("" "-updates" "-security"))
                (releases (delete-dups (append (split-string
                                                (shell-command-to-string
                                                 "distro-info --supported"))
                                               (split-string
                                                (shell-command-to-string
                                                 "distro-info --supported-esm"))
                                               (split-string
                                                (shell-command-to-string
                                                 "distro-info --devel"))))))
            (dolist (release releases)
              (dolist (pocket pockets)
                (add-to-list 'debian-changelog-allowed-distributions (concat release pocket)))))
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '(debian-changelog-mode . ("debputy" "lsp" "server")))))

(use-package debian-control-mode
  :ensure dpkg-dev-el
  :hook ((debian-control-mode . eglot-ensure))
  :config (with-eval-after-load 'eglot
            (add-to-list 'eglot-server-programs '(debian-control-mode . ("debputy" "lsp" "server")))))

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
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-keep-variants nil)
  (ediff-split-window-function 'split-window-horizontally))

(use-package eglot
  :preface
  ;; compose all eldoc messages together so eglot plays nicely with flymake etc
  ;; https://www.masteringemacs.org/article/seamlessly-merge-multiple-documentation-sources-eldoc
  (defun apm-eglot-compose-eldoc ()
    (setq eldoc-documentation-strategy
          'eldoc-documentation-compose))
  :hook ((prog-mode . eglot-ensure)
         (yaml-mode . eglot-ensure)
         (eglot-managed-mode . apm-eglot-compose-eldoc)
         (eglot-managed-mode . eglot-inlay-hints-mode))
  :custom
  (eglot-extend-to-xref t)
  ;; improve performance by logging less
  (eglot-events-buffer-config '(:size 20000 :format short))
  :config
  ;; speed up performance
  (fset #'jsonrpc--log-event #'ignore)
  (add-to-list 'eglot-server-programs '(cmake-ts-mode "cmake-language-server" :initializationOptions (:buildDirectory "build/")))
  ;; enable formatting in vscode-json-languageserver
  (add-to-list 'eglot-server-programs '(json-mode "vscode-json-languageserver" "--stdio"
                                                  :initializationOptions (:provideFormatter t)))
  (add-to-list 'eglot-server-programs '(markdown-mode "vscode-markdown-languageserver" "--stdio"
                                                      :initializationOptions (:markdownFileExtensions ["md"])))
  ;; use vale-lsp over markdown-languageserver as the former still doesn't work
  ;; as a snap...
  (add-to-list 'eglot-server-programs '(markdown-mode "vale.vale-ls"))
  (add-to-list 'eglot-server-programs `(vimrc-mode "vim-language-server" "--stdio"
                                                   :initializationOptions (:vim-runtime ,(car (file-expand-wildcards "/usr/share/vim/vim*"))))))

(use-package eglot-booster
  :vc (:url "https://github.com/jdtsmith/eglot-booster/")
  :after eglot
  :init (unless (executable-find "emacs-lsp-booster")
          (alert "Please install emacs-lsp-booster to ~/bin from https://github.com/blahgeek/emacs-lsp-booster/releases"))
  :config (eglot-booster-mode))

(use-package eglot-inactive-regions
  :ensure t
  :after eglot
  :config (eglot-inactive-regions-mode 1))

(use-package eldoc
  :config (global-eldoc-mode 1))

(use-package eldoc-box
  :ensure t
  ;; tie in with eglot
  :hook ((eglot-managed-mode . eldoc-box-hover-mode)))

(use-package eldoc-diffstat
  :ensure t
  :config (global-eldoc-diffstat-mode 1))

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
  :hook ((emacs-lisp-mode ielm-mode) . elisp-def-mode))

(use-package elisp-mode
  ;; ensure flymake-mode is off for elisp files by default to minimise the
  ;; chance of RCE -
  ;; https://eshelyaron.com/posts/2024-11-27-emacs-aritrary-code-execution-and-how-to-avoid-it.html
  :hook (emacs-lisp-mode . flymake-mode-off))

(use-package epg
  :config
  (setq epg-user-id "murray.alex@gmail.com"))

(use-package erc-goodies
  :ensure erc
  ;; ensure this is set and we don't inadvertently unset it
  :bind (:map erc-mode-map ("C-c C-c" . nil))
  ;; use erc-keep-place-indicator-mode
  :hook (erc-mode . erc-keep-place-indicator-mode)
  :config
  (progn
    (setq erc-interpret-controls-p t)
    (add-to-list 'erc-modules 'keep-place)
    (erc-update-modules)))

(use-package erc-image
  :ensure t
  :after erc
  :config
  (add-to-list 'erc-modules 'image)
  (erc-update-modules))

(use-package erc-view-log
  :ensure t
  :config
  (add-to-list 'auto-mode-alist
               `(,(format "%s/.*\\.[log|txt]"
                          (regexp-quote
                           (expand-file-name
                            erc-log-channels-directory))) . erc-view-log-mode)))

(use-package erlang
  :load-path "vendor/erlang")

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

(use-package expreg
  :ensure t
  :bind (("C-'" . expreg-expand)
         ("C-\"" . expreg-contract)))

(use-package files
  :bind (("C-c r b" . revert-buffer))
  :config
  :custom
  (view-read-only t)
  (save-some-buffers-default-predicate #'save-some-buffers-root))

(use-package flymake
  :hook (prog-mode . flymake-mode)
  :custom (flymake-mode-line-lighter "🪰")
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error)))

(use-package flymake-codespell
  :ensure t
  :init (unless (executable-find "codespell")
          (alert "Please apt install codespell"))
  :hook (prog-mode . flymake-codespell-setup-backend))

(use-package flymake-ruff
  :ensure t
  :init (unless (executable-find "ruff")
          (alert "Please snap install ruff"))
  :preface (defun apm-flymake-ruff-load ()
             (when (and (derived-mode-p 'python-base-mode)
                        (not (eq buffer-file-name nil)))
               (flymake-ruff-load)))
  ;; load via eglot - but this should only be done in python-mode buffers
  :hook (eglot-managed-mode . apm-flymake-ruff-load))

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

(use-package gh-notify
  :ensure t)

(use-package gitattributes-mode
  :ensure git-modes
  :defer t)

(use-package gitconfig-mode
  :ensure git-modes
  :defer t)

(use-package gitignore-mode
  :ensure git-modes
  :defer t)

(use-package gnuplot
  :ensure t)

(use-package gnu-elpa
  :defer t
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
               (list "\\(requested your review on:\\|amurray\\|alexmurray\\|Alex Murray\\)" 1 1 'error))
  ;; and for build failure emacs
  (add-to-list 'gnus-emphasis-alist
               (list "\\(State: failed to \\(build\\|upload\\)\\)" 1 1 'error))
  ;; don't fill long lines as breaks tables in emails
  (setq gnus-treat-fill-long-lines nil)
  ;; discourse emails use --- as signature separator
  (setq gnus-signature-separator '("^---? $" "^---? *$")))

(use-package goggles
  :ensure t
  :config (goggles-mode 1))

(use-package go-mode
  :ensure t
  ;; plug go-ts-mode into gofmt
  :preface (defun apm-gofmt-before-save ()
             (interactive)
             (when (member major-mode '(go-mode go-ts-mode))
               (gofmt)))
  :hook ((before-save . apm-gofmt-before-save)))

(use-package gotest
  :ensure t
  ;; snapd uses the check.v1 package for defining test suites rather than
  ;; testify so we can't specify a single test using -testify.m - instead this
  ;; needs to be -check.f - but gotest hardcodes the use of testify so
  ;; unfortunately we can't customise this - instead just ensure than whenever
  ;; go-test--go-test is called that we replace any instance of -testify.m with
  ;; -check.f
  :config (define-advice go-test--go-test (:filter-args (args) apm-go-test--go-test)
            ;; args is a list containing the arguments passed to
            ;; go-test--go-test - which is the command-line arguments as a
            ;; single string and an optional env which we need to retain
            (append (list (replace-regexp-in-string "-testify\.m" "-check\.f" (car args)))
                    (cdr args))))

(use-package go-translate
  :ensure t
  :bind (("C-c t" . gt-do-translate))
  :config
  (setq gt-default-translator
        (gt-translator
         ;; add translate support for chinese to english
         :taker (gt-taker :langs '(zh en))
         :engines (list (gt-google-engine) (gt-bing-engine)))))

(use-package goto-addr
  :defer t
  :hook ((prog-mode . goto-address-prog-mode)
         (text-mode . goto-address-mode)))

(use-package goto-line-preview
  :ensure t
  :config (global-set-key [remap goto-line] 'goto-line-preview))

(use-package gptel
  :ensure t
  ;; configure to use local ollama instance
  :config (setq
 gptel-model 'qwen2.5-coder:latest
 gptel-backend (gptel-make-ollama "Ollama"
                 :host "localhost:11434"
                 :stream t
                 :models '(qwen2.5-coder qwen3:latest)))


  )
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
  ;; use hs-minor-mode in programming and mail composing - TODO - get it
  ;; working during mail viewing as well to be able to hide quoted bits
  ;; - something like:
  :hook ((prog-mode message-mode) . hs-minor-mode))

(use-package hippie-exp
  :bind (("M-/" . hippie-expand)))

(use-package hl-printf
  :ensure t
  :hook ((prog-mode . hl-printf-mode)))

(use-package hl-todo
  :ensure t
  :config
  (global-hl-todo-mode 1)
  ;; add flymake backend too
  (when (fboundp 'flymake-hl-todo)
    ;; TODO - apparently should use depth nil and set 'local as well...
    (add-hook 'flymake-diagnostic-functions #'flymake-hl-todo)))

(use-package hungry-delete
  :ensure t
  :config (global-hungry-delete-mode 1))

(use-package imenu
  :bind (("M-i" . imenu)))

(use-package ispell
  ;; using jinx for spell checking
  :disabled t
  :defer t
  :init (unless (executable-find "aspell")
          (alert "Please apt install aspell"))
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


(use-package jinx
  :ensure t
  :preface
  ;; jinx uses emacs modules and we need to ensure it compiles with gcc from the
  ;; emacs snap
  (define-advice jinx--load-module (:around (orig-fun &rest args) apm-jinx--load-module)
    "Ensure that the module is compiled with the correct gcc."
    (let* ((sysroot (file-name-as-directory (concat (file-name-as-directory (getenv "EMACS_SNAP_USER_COMMON")) "sysroot")))
           (process-environment (append `(,(concat "CC=" sysroot "usr/bin/gcc-14" )
                                          ,(concat "PKG_CONFIG_PATH=" (car (file-expand-wildcards (concat sysroot "usr/lib/*/pkgconfig")))))
                                        process-environment))
           (jinx--compile-flags (append jinx--compile-flags
                                               (list (concat "--sysroot=" sysroot)
                                                     (concat "-Wl,-rpath=" sysroot "/lib/x86_64-linux-gnu")))))
      (apply orig-fun args)))
  :bind (("M-$" . jinx-correct)
         :map jinx-mode-map ("C-;" . jinx-correct))
  :hook ((emacs-startup . global-jinx-mode)))

(use-package journalctl
  :ensure t
  :vc (:url "https://github.com/WJCFerguson/journalctl/"))

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
  :ensure t
  :preface (defun apm-recentf-ignore-lxd-tramp (file)
             "Do not keep FILE if it is on a lxd remote."
             ;; return non-nil to keep in recentf-list
             (not (string-match "^/lxd:.*" file)))
  :config (with-eval-after-load 'recentf
            (add-to-list 'recentf-keep 'apm-recentf-ignore-lxd-tramp)))

(use-package lp
  :load-path "/snap/gitlptools/current"
  :after magit
  :init (unless (executable-find "git-lp-open")
          (alert "Please install the gitlptools snap")))

(use-package magit
  :ensure t
  :custom ((magit-diff-refine-hunk t)
           (magit-format-file-function #'magit-format-file-all-the-icons))
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
  :hook ((emacs-startup . marginalia-mode)))

(use-package markdown-mode
  :ensure t
  :init (unless (executable-find "markdown")
          (alert "Please apt install discount"))
  ;; ensure it is loaded since is used by eglot
  :demand t
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

(use-package mb-depth
  :config (minibuffer-depth-indicate-mode 1))

(use-package mermaid-mode
  :ensure t
  :defer t)

(use-package meson-mode
  :ensure t)

(use-package minibuffer
  :config
  (setq completion-styles '(substring orderless basic))
  (setq completion-category-defaults nil)
  (setq completion-category-overrides '((file (styles basic partial-completion))
                                        (eglot (styles orderless))))
  (setq completion-auto-help 'visible)
  (setq completion-auto-select 'second-tab))

(use-package minions
  :ensure t
  :config
  (add-to-list 'minions-prominent-modes 'flymake-mode)
  (minions-mode 1))

(use-package modern-cpp-font-lock
  :ensure t
  :defer t
  :hook ((c++-mode . modern-c++-font-lock-mode)))

(use-package mouse
  :config (context-menu-mode 1))

(use-package move-text
  :ensure t
  :bind (([(meta shift up)] . move-text-up)
         ([(meta shift down)] . move-text-down)))

(use-package nano-agenda
  :ensure t)

(use-package nhexl-mode
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

(use-package orderless
  ;; for vertico
  :ensure t)

(use-package org-block-capf
  :after org
  :vc (:url "https://github.com/xenodium/org-block-capf/")
  :hook (org-mode . org-block-capf-add-to-completion-at-point-functions))

(use-package org-crypt
  :ensure org
  :config
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance '("crypt"))
  (setq org-crypt-key '("88E9530BCBDDC200517B5EB0F498D2D9DE7DAD9C")))

(use-package org-id
  :ensure org
  :config
  (setq org-id-link-to-org-use-id t))

(use-package org-autolist
  :ensure t
  :after org
  :hook (org-mode . org-autolist-mode))

(use-package org-refile
  :ensure org
  :config
  (setq org-refile-targets '(("~/git/org-files/personal.org" :maxlevel . 2)
                             ("~/git/org-files/someday.org" :level . 1)
                             ("~/git/org-files/tickler.org" :maxlevel . 1)
                             ("~/git/org-files/notes.org" :maxlevel . 2)))
  ;; allow to refile as top-level items in files
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil))

;; add support for man: links in org documents
(use-package ol-man
  :ensure org)

(use-package org-agenda
  :ensure org
  :preface
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
  (setq org-agenda-clockreport-parameter-plist '(:link t :maxlevel 4 :narrow 80 :tags t :hidefiles t))
  ;; when showing agenda, jump to now
  (add-hook 'org-agenda-finalize-hook
            #'org-agenda-find-same-or-today-or-agenda 90)
  (setq org-agenda-custom-commands
        '(("i" "TODO from inbox" todo "TODO"
           ((org-agenda-files '("~/git/org-files/inbox.org"))))
          ("p" "TODO from personal" todo "TODO"
           ((org-agenda-files '("~/git/org-files/personal.org")))))))

(use-package org-capture
  :preface
  :after org
  :config
  (let ((inbox-org (expand-file-name "inbox.org" org-directory))
        (tickler-org (expand-file-name "tickler.org" org-directory)))
    (setq org-capture-templates
          `(("t" "todo" entry (file ,inbox-org)
             "* TODO %i%?
- %a")
            ("I" "tickler" entry (file ,tickler-org)
             "* %i%?
%U
- %a")
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
  :preface
  (defun apm-org-clock-weekly-report-formatter (ipos tables params)
    "Generate a weekly task report with the given `IPOS', `TABLES' and `PARAMS'.
The `IPOS' is the point position. `TABLES' should be a list of table data.
The `PARAMS' should be a property list of table keywords and values.

See `org-clocktable-write-default' if you want an example of how the standard
clocktable works."
    (let* ((lang (or (plist-get params :lang) "en"))
           (block (plist-get params :block))
           (emph (plist-get params :emphasize))
           (header (plist-get params :header))
           (show-time (plist-get params :show-time)))
      (goto-char ipos)
      (insert-before-markers
       (or header
           ;; Format the standard header.
           (format "#+CAPTION: %s %s%s\n"
                   (org-clock--translate "Clock summary at" lang)
                   (format-time-string (org-time-stamp-format t t))
                   (if block
                       (let ((range-text
                              (nth 2 (org-clock-special-range
                                      block nil t
                                      (plist-get params :wstart)
                                      (plist-get params :mstart)))))
                         (format ", for %s." range-text))
                     ""))))
      (let '(total-time (apply #'+ (mapcar #'cadr tables)))
        (when (and total-time (> total-time 0))
          (pcase-dolist (`(, file-name , file-time , entries) tables)
            (when (and file-time (> file-time 0))
              (pcase-dolist (`(,level ,headline ,tgs ,ts ,time ,props) entries)
                (insert-before-markers
                 ;; indent with level
                 (if (= level 1)
                     "- "
                   (concat (make-string level ? ) "- "))
                 headline
                 (if show-time
                     (concat " ["
                             (org-duration-from-minutes time)
                             "]")
                   "")
                 "\n"))))
          (when show-time
            (insert-before-markers
             "\n"
             "Total time"
             (concat " ["
                     (format "%s" (org-duration-from-minutes total-time))
                     "]")))))))
  ;; ensure we always run org-clock-persistence-insinuate below
  :demand t
  :bind (("C-c g" . org-clock-goto)
         ("C-c i" . org-clock-in)
         ("C-c o" . org-clock-out))
  :config
  ;; include the current clocked in task in clock reports
  (setq org-clock-report-include-clocking-task t)
  ;; save running clock and all history when exiting emacs
  (setq  org-clock-persist t)
  ;; resume clocking task on clock-in if the clock is open
  (setq  org-clock-in-resume t)
  ;; persist clock data into org-directory
  (setq org-clock-persist-file (expand-file-name "org-clock-save.el" org-directory))
  ;; reload any saved org clock information on startup
  (org-clock-persistence-insinuate))

(use-package org-clock-convenience
  :ensure t
  :after org
  :bind (:map org-agenda-mode-map
              ("S-<up>" . org-clock-convenience-timestamp-up)
              ("S-<down>" . org-clock-convenience-timestamp-down)))

(use-package org-duration
  :ensure org
  :after org
  ;; don't show days, only total hours as maximum value
  :config (setq org-duration-format (quote h:mm)))

(use-package orgit
  :ensure t
  :after org)

(use-package orgit-forge
  :ensure t)

(use-package org-src
  :ensure org
  :config
  ;; prefer org src blocks to not be indented with surrounding content
  (setq org-edit-src-content-indentation 0))

(use-package org-table-sticky-header
  :ensure t
  :after org
  :defer t
  :hook ((org-mode . org-table-sticky-header-mode)))

(use-package ox-gfm
  :ensure t
  :after ox)

(use-package paredit
  :ensure t
  ;; don't steal occur prefix
  :bind (:map paredit-mode-map ("M-s" . nil))
  :hook ((emacs-lisp-mode . enable-paredit-mode)))

(use-package paren
  :defer t
  :commands show-paren-mode
  :custom
  (show-paren-context-when-offscreen 'child-frame)
  :config
  (show-paren-mode 1))

(use-package paren-face
  :ensure t
  :config (global-paren-face-mode 1))

(use-package pcap-mode
  :ensure t
  :mode ("\\.pcapng\\'" . pcap-mode))

(use-package pdf-tools
  :pin nongnu
  :ensure t
  :config
  (pdf-loader-install))

(use-package perl-mode
  :preface (defun apm-perl-mode-setup ()
             (setq-local indent-tabs-mode t))
  :hook ((perl-mode . apm-perl-mode-setup))
  :custom (perl-indent-level 8))

(use-package posframe
  :ensure t
  :config (setq posframe-mouse-banish nil))

(use-package pr-review
  :ensure t)

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
            (require 'magit-extras)))

(use-package project-mode-line-tag
  :ensure t
  :config (project-mode-line-tag-mode 1))

(use-package projection
  :ensure t
  :hook (emacs-startup . global-projection-hook-mode)
  :bind-keymap ("C-x P" . projection-map)
  :config (with-eval-after-load 'project
            (require 'projection)))

(use-package python
  :defer t
  :custom
  (python-indent-offset 4)
  (python-check-command (executable-find "flake8"))
  (python-flymake-command '("flake8" "-")))

(use-package pythontest
  :ensure t
  ;; use the default unittest runner since it supports unittest.subTest() etc
  :custom (pythontest-test-runner "unittest"))

(use-package rainbow-mode
  :ensure t
  :defer t
  :hook ((css-mode html-mode) . rainbow-mode))

(use-package recentf
  :preface
  (defun apm-recentf-ignore-sudo (file)
    "Do not keep FILE if it requires sudo to read."
    ;; return non-nil to keep in recentf-list
    (not (string-match "^/sudo:.*" file)))
  :custom (recentf-keep '(apm-recentf-ignore-sudo recentf-keep-default-predicate))
  :config (recentf-mode 1))

(use-package region-state
  :ensure t
  :config (region-state-mode 1))

(use-package rfn-eshadow
  :config
  ;; make minibuffer show shadowed files and play nicer with vertico
  (when (require 'vertico nil t)
    (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy))
  (file-name-shadow-mode 1))

(use-package ripgrep
  :ensure t
  :init (unless (executable-find "rg")
          (alert "Please apt install ripgrep")))

(use-package rnc-mode
  :ensure t)

(use-package rust-mode
  :ensure t
  ;; ensure rust-analyzer is installed via the rustup snap
  :config
  (unless (executable-find "rust-analyzer")
    (alert "Please install the rustup snap")))

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

(use-package sh-script
  :init (setq-default sh-basic-offset 2
                      sh-indentation 2))

(use-package sideline
  :ensure t)

(use-package sideline-flymake
  :ensure t
  :hook (flymake-mode . sideline-mode)
  :custom
  (sideline-backends-right '((sideline-flymake . down))))

(use-package simple
  :defer t
  :init
  ;; save whatever is in the system clipboard to the kill ring before
  ;; killing something else into the kill ring
  (setq save-interprogram-paste-before-kill t)
  (setq next-error-message-highlight t)
  (setq visual-line-fringe-indicators
        '(left-curly-arrow right-curly-arrow)))

(use-package smiley
  :custom (smiley-style 'emoji))

(use-package so-long
  :config (global-so-long-mode 1))

(use-package spacious-padding
  :ensure t
  :config (setq spacious-padding-widths
                '( :internal-border-width 0
                   :header-line-width 4
                   :mode-line-width 4
                   :tab-width 4
                   :right-divider-width 0
                   :scroll-bar-width 0))
  (spacious-padding-mode 1))

(use-package strace-mode
  :ensure t)

(use-package sudo-edit
  :ensure t
  :commands (sudo-edit))

(use-package suggest
  :ensure t)

(use-package syslog-mode
  :ensure t)

(use-package time
  :config
  (setq display-time-default-load-average nil)
  (setq display-time-use-mail-icon t)
  (setq display-time-day-and-date t)
  (setq display-time-24hr-format t)
  (display-time))

(use-package treesit-auto
  :ensure t
  :custom (treesit-auto-install 'prompt)
  :config (global-treesit-auto-mode 1))

(use-package udev-mode
  :ensure t)

(use-package ultra-scroll
  :vc (:url "https://github.com/jdtsmith/ultra-scroll/")
  :init
  (setq scroll-conservatively 101 ; important!
        scroll-margin 0)
  :config
  (ultra-scroll-mode 1))

(use-package undo-tree
  :ensure t
  :config
  (when (require 'no-littering nil t)
    (setq undo-tree-history-directory-alist
          `((,(concat "\\`" (file-name-as-directory temporary-file-directory)))
            ("\\`/tmp/" . nil)
            ("\\`/dev/shm/" . nil)
            ("." . ,(no-littering-expand-var-file-name "undo-tree-hist/")))))
  (global-undo-tree-mode 1))

(use-package uniquify
  :config (setq uniquify-buffer-name-style 'post-forward
                uniquify-separator ":"
                uniquify-after-kill-buffer-p t
                uniquify-ignore-buffers-re "^\\*"))

(use-package vc-auto-commit
  ;; used to automatically commit changes in org-files git repo
  :ensure t
  :config (vc-auto-commit-activate 1))

(use-package vertico
  :ensure t
  :hook ((emacs-startup . vertico-mode))
  :custom
  (vertico-cycle t)
  (vertico-resize nil))

(use-package vertico-multiform
  :ensure vertico
  :config
  ;; enhance display of jinx to use a grid
  (add-to-list 'vertico-multiform-categories
               '(jinx grid (vertico-grid-annotate . 20)))
  (vertico-multiform-mode 1))

(use-package vertico-repeat
  :ensure vertico
  :hook (minibuffer-setup . vertico-repeat-save)
  :bind (("C-c r v" . vertico-repeat)))

(use-package vimrc-mode
  :ensure t
  :mode (("\\.vim\\(rc\\)?\\'" . vimrc-mode)))

(use-package virtual-auto-fill
  :ensure t
  :hook (((markdown-mode rst-mode) . virtual-auto-fill-mode)))

(use-package visual-replace
  :ensure t
  :config (visual-replace-global-mode 1))

(use-package webpaste
  :ensure t
  :bind (("C-c C-p C-b" . webpaste-paste-buffer)
         ("C-c C-p C-r" . webpaste-paste-region)
         ("C-c C-p C-p" . webpaste-paste-region-or-region))
  :demand t
  :config
  (setq webpaste-paste-confirmation t)
  (setq webpaste-provider-priority '("dpaste.org")))

(use-package which-key
  :ensure t
  :config (which-key-mode))

(use-package whitespace
  :init (setq-default whitespace-style
                      '(face tabs tab-mark trailing missing-newline-at-eof))
  :config
  ;; whitespace-mode is not useful for erc or magit-log buffers
  (setq whitespace-global-modes '(not erc-mode magit-log-mode))
  (global-whitespace-mode 1))

(use-package whitespace-cleanup-mode
  :ensure t
  :config (global-whitespace-cleanup-mode 1))

(use-package wgrep-deadgrep
  :ensure t)

(use-package world-time-mode
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package yasnippet
  :ensure t
  :config (yas-global-mode 1))

(use-package yasnippet-capf
  :vc (:url "https://github.com/elken/yasnippet-capf/")
  :after cape
  :config (add-to-list 'completion-at-point-functions #'yasnippet-capf))

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
  :bind (("C-x C-d" . ztree-diff))
  :config
  (setq ztree-draw-unicode-lines t)
  (setq ztree-show-number-of-children t))

(provide 'init)

;;; init.el ends here
