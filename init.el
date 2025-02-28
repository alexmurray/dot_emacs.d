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
                                 '("personal.org" "canonical.org"
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

  :hook ((after-init . apm-prompt-to-connect-to-irc))
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
  ;; - not needed for libera (since we use ZNC) or canonical matterircd
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

;; support :vc integration in use-package - this is in emacs 30 but not emacs-29
;; so install it manually if needed
(unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))
(eval-and-compile
  (require 'vc-use-package))

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
           `(erc-keyword-face ((t (:weight bold :foreground ,(doom-color 'yellow)))))
           ;; make some notmuch elements have more contrast
           `(notmuch-message-summary-face ((t (:foreground ,(doom-color 'constants)))))
           `(notmuch-wash-cited-text ((t (:foreground ,(doom-color 'base6))))))
        (custom-theme-set-faces
         apm-preferred-theme
         `(erc-keyword-face ((t (:weight bold :foreground ,(doom-color 'yellow)))))
         ;; revert some elements for light theme
         `(notmuch-message-summary-face ((t (:foreground ,(doom-color 'grey)))))
         `(notmuch-wash-cited-text ((t (:foreground ,(doom-color 'base4))))))))
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
       `(notmuch-search-unread-face ((((class color) (min-colors 89)) (:weight bold))))
       `(notmuch-tag-added ((((class color) (min-colors 89)) (:underline ,(car (alist-get 'spring-green kanagawa-themes-color-palette-list))))))
       `(notmuch-tag-deleted ((((class color) (min-colors 89)) (:strike-through ,(car (alist-get 'wave-red kanagawa-themes-color-palette-list))))))
       `(notmuch-tag-face ((((class color) (min-colors 89)) (:weight bold :foreground ,(car (alist-get 'wave-blue-2 kanagawa-themes-color-palette-list))))))
       `(notmuch-tag-unread ((((class color) (min-colors 89)) (:foreground ,(car (alist-get 'wave-red kanagawa-themes-color-palette-list))))))
       `(notmuch-wash-cited-text ((((class color) (min-colors 89)) (:foreground ,(car (alist-get 'fuji-gray kanagawa-themes-color-palette-list)) :slant ,(if kanagawawa-theme-keyword-italic 'italic 'normal)))))
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
  ;; via notmuch below
  (setq user-mail-address "alex.murray@canonical.com")

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
    (if (font-info preferred-font)
        ;; apply this to all existing and future frames
        (set-frame-font preferred-font nil t)
      (alert "Please apt install fonts-ubuntu")))

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

(use-package blacken
  :ensure t
  :init (unless (executable-find "black")
          (alert "Please install the black snap")))

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
         ("M-s e" . consult-isearch)
         ;; https://emacsredux.com/blog/2021/11/25/redo-complex-command-with-consult/
         ([remap repeat-complex-command] . consult-complex-command)
         :map isearch-mode-map
         ("M-e" . consult-isearch)   ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch) ;; orig. isearch-edit-string
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

(use-package consult-notmuch
  :ensure t)

(use-package consult-xref-stack
  :vc (:fetcher github :repo brett-lempereur/consult-xref-stack)
  :bind (("C-," . consult-xref-stack-backward)))

(use-package copilot
  :vc (:fetcher github :repo zerolfx/copilot.el)
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
  :custom ((copilot-server-executable "/snap/bin/copilot-language-server")))

(use-package crontab-mode
  :ensure t)

(use-package cve-mode
  :load-path "~/ubuntu/git/ubuntu-cve-tracker/scripts"
  :mode (("CVE-[[:digit:]]\\{4\\}-[[:digit:]]\\{4,\\}\\'" . cve-mode)
         ("00boilerplate.*\\'" . cve-mode)
         (("boilerplate" . cve-mode)))
  :hook ((cve-mode . flymake-mode))
  ;; this is used for Notes in CVE files which the web pages link to the
  ;; corresponding launchpad user
  :custom
  (cve-mode-default-user "alexmurray")
  (cve-mode-prefix-notes-lines 'all))

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

(use-package cvelist
  :load debian-cvelist
  :load-path "~/ubuntu/git/security-tracker/conf/"
  :mode ("list\\'" . debian-cvelist-mode))

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

(use-package discourse-mode
  :vc (:fetcher codeberg :repo "glenneth/discourse-emacs"))

(use-package disk-usage
  :ensure t)

(use-package display-fill-column-indicator
  :hook ((prog-mode . display-fill-column-indicator-mode)
         (text-mode . display-fill-column-indicator-mode)))

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
  :vc (:fetcher github :repo jdtsmith/eglot-booster)
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
  (setq epg-user-id "alex.murray@canonical.com"))

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

(use-package erc-matterircd
  :load-path "vendor/"
  :disabled t
  :after erc
  :config
  ;; don't clutter view with context ids
  (setq erc-matterircd-replace-context-id "↩")
  (setq erc-matterircd-updatelastviewed-on-buffer-switch t)
  (setq erc-matterircd-server "chat.canonical.com")
  (setq erc-matterircd-team "canonical")
  (let ((token (auth-source-pick-first-password :host "matterircd")))
    (if (null token)
        (alert (format "Please store matterircd token in ~/.authinfo.gpg with machine matterircd"))
      (setq erc-matterircd-password (concat "token=" token))))
  (add-to-list 'erc-modules 'matterircd)
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
  (add-to-list 'eudc-attribute-display-method-alist '("wikipage" . apm-eudc-display-wikipage))
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

  (defun apm-eudc-display-wikipage (id)
    "Display ID as a clickable URL."
    (eudc-display-url (concat "https://wiki.canonical.com/" id)))

  (defun apm-eudc-display-nick (nick)
    "Display NICK as using colors from erc-nicks."
    (insert (propertize nick
                        'face
                        ;; colorise as is done on Libera.Chat by erc-nicks
                        (ignore-errors
                          (with-current-buffer "Libera.Chat"
                            (erc-nicks--highlight nick))))))

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
  :preface (defun apm-setup-eudc-capf ()
             (add-hook 'completion-at-point-functions #'eudc-capf-complete -1 t))
  :hook ((notmuch-message-mode . apm-setup-eudc-capf))
  :config
  (add-to-list 'eudc-capf-modes 'notmuch-message-mode))

(use-package eudcb-notmuch-address
  ;; https://www.mail-archive.com/notmuch@notmuchmail.org/msg53258.html
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


(use-package jinx
    :ensure t
    :preface
    ;; jinx uses emacs modules and we need to ensure it compiles with gcc from the
    ;; emacs snap
    (define-advice jinx--load-module (:around (orig-fun &rest args) apm-jinx--load-module)
      "Ensure that the module is compiled with the correct gcc."
      (let* ((emacs-snap-dir (file-name-as-directory (getenv "EMACS_SNAP_DIR")))
             (process-environment (append `(,(concat "CC=" emacs-snap-dir "usr/bin/gcc-14" )
                                            ,(concat "CXX=" emacs-snap-dir "usr/bin/g++-14")
                                            ,(concat "CFLAGS=--sysroot=" emacs-snap-dir " -B" emacs-snap-dir "usr/lib/gcc")
                                            ,(concat "CPATH=" (file-name-directory (car (file-expand-wildcards (concat emacs-snap-dir "usr/include/*/bits")))))
                                            ,(concat "CPPFLAGS=--sysroot=" emacs-snap-dir)
                                            ,(concat "LDFLAGS=--sysroot=" emacs-snap-dir " -L" emacs-snap-dir "usr/lib")
                                            ,(concat "PKG_CONFIG_PATH=" (car (file-expand-wildcards (concat emacs-snap-dir "usr/lib/*/pkgconfig")))))
                                          process-environment)))
        (apply orig-fun args)))
    :bind (("M-$" . jinx-correct)
           :map jinx-mode-map ("C-;" . jinx-correct))
    :hook ((emacs-startup . global-jinx-mode)))

(use-package journalctl
  :ensure t
  :vc (:fetcher github :repo WJCFerguson/journalctl))

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
  :init (unless (executable-find "ldapsearch")
          (alert "Please apt install ldap-utils"))
  :demand t
  :custom
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
  :ensure t
  :preface (defun apm-recentf-ignore-lxd-tramp (file)
             "Do not keep FILE if it is on a lxd remote."
             ;; return non-nil to keep in recentf-list
             (not (string-match "^/lxd:.*" file)))
  :config (with-eval-after-load 'recentf
            (add-to-list 'recentf-keep 'apm-recentf-ignore-lxd-tramp))

(use-package lp
  :load-path "/snap/gitlptools/current"
  :after magit
  :init (unless (executable-find "git-lp-open")
          (alert "Please install the gitlptools snap")))

(use-package magit
  :ensure t
  :pin melpa
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

(use-package mailscripts
  :load-path "vendor/"
  :init (unless (executable-find "maildir-import-patch")
          (alert "Please apt install mailscripts")))

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

(use-package message
  :custom
  ;; use standard completion UI for message completion
  (message-expand-name-standard-ui t)
  (message-citation-line-format "On %a, %Y-%m-%d at %T %z, %N wrote:\n")
  (message-citation-line-function #'message-insert-formatted-citation-line)
  (message-make-forward-subject-function 'message-forward-subject-fwd)
  ;; kill message buffer after sending rather than burying
  (message-kill-buffer-on-exit t)
  ;; disable filling of long lines
  (message-fill-column nil))

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

(use-package message-attachment-reminder
  :ensure t)

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

(use-package nano-agenda
  :ensure t)

(use-package nhexl-mode
  :ensure t)

(use-package notmuch
  :ensure t
  :init
  (unless (executable-find "notmuch")
    (alert "Please apt install notmuch"))
  (unless (executable-find "afew")
    (alert "Please apt install afew"))
  :preface
  ;; discourage the use of text/plain for certain senders
  (defvar apm-notmuch-discouraged-senders '((("text/plain") . ("forum@forum.snapcraft.io"
                                                               "noreply@discourse.ubuntu.com"
                                                               "noreply@discourse.canonical.com"
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

  (defun apm-notmuch-show-view-lp-build-log ()
    "Show the build log for the current message in a new buffer."
    (interactive)
    ;; find the build log URL in the current message, and open it in a new buffer
    ;; with compilation-mode to view the log
    (unless (eq major-mode 'notmuch-show-mode)
      (error "Not in notmuch-show-mode"))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "Build Log:" nil t)
        (re-search-forward "https?://[^[:space:]]+" nil t)
        (when-let ((url (thing-at-point 'url)))
          (message "Fetching build log... %s" url)
          (condition-case err
              (let ((buffer (url-retrieve-synchronously url t)))
                (with-current-buffer buffer
                  (rename-buffer (format "*Build Log: %s*" url) t)
                  (compilation-mode)
                  (pop-to-buffer buffer)
                  ;; also run analyse-build-log when available to pinpoint the line
                  ;; of interest then scroll to that line
                  (if (executable-find "analyse-build-log")
                      (let ((line))
                        (with-temp-buffer
                          (insert-buffer-substring buffer)
                          ;; lpci logs add :: prefix to lines which confuses
                          ;; the analysis so remove this first
                          (save-excursion
                            (save-match-data
                              (goto-char (point-min))
                              (while (re-search-forward "^:: " nil t)
                                (replace-match ""))))
                          (message "Analysing build log...")
                          (shell-command-on-region (point-min) (point-max) "analyse-build-log /dev/stdin" t t)
                          (compilation-mode)
                          (save-excursion
                            (goto-char (point-min))
                            (when (re-search-forward "\\(Issue found at line\\|Failed line:\\) \\([0-9]+\\)" nil t)
                              (setq line (string-to-number (match-string 2)))
                              ;; show the full output to the user
                              (message (buffer-substring (point-min) (point-max))))))
                        (when line
                          (with-current-buffer buffer
                            (forward-line (1- line)))))
                    (message "analyse-build-log not found - install python3-buildlog-consultant or buildlog-consultant to support automatic error finding"))))
            (error (message "Failed to download build log or analyse it: %s" (cdr err))))))))
  :bind (("C-c m" . notmuch)
         :map notmuch-show-mode-map
         ("C-c C-l C-b" . apm-notmuch-show-view-lp-build-log))
  :custom
  (notmuch-wash-wrap-lines-length 150)
  (notmuch-print-mechanism #'notmuch-print-ps-print/evince)
  :config
  (eval-and-compile
    (require 'notmuch)
    (require 'notmuch-show)
    (require 'notmuch-tree))
  (setq notmuch-multipart/alternative-discouraged 'apm-notmuch-determine-discouraged)
  (defun apm-prompt-to-report-spam (subject url)
    (and (y-or-n-p (format "Do you also want to report this message \"%s\" as spam to mailcontrol? " subject))
         (url-retrieve (concat url)
                       (lambda (s)
                         (let ((status (url-http-symbol-value-in-buffer
                                        'url-http-response-status (current-buffer))))
                           (pcase status
                             (200 (message "Reported '%s' as spam" subject))
                             (_ (user-error "Failed to report as spam: %s" status))))) ))    )

  (defun apm-get-websense-blocklist-url ()
    "Get websense blocklist URL via the most recently received summary email."
    ;; the summary email is sent with subject "Personal Email Subscription - Forcepoint Email Security Cloud"
    (let ((summary-email (shell-command-to-string "notmuch show --include-html --sort=newest-first --limit 1  subject:\"Personal Email Subscription - Forcepoint Email Security Cloud\"")))
      (when (string-match "\\(https://\\(admin.websense.net\\|www.mailcontrol.com\\)/r/[^?]*\\).*Manage Allow/Block Lists" summary-email)
        (match-string 1 summary-email))))

  (defun apm-prompt-to-add-email-to-forcepoint-blocklist (email description)
    "Add EMAIL to the forcepoint blocklist with DESCRIPTION."
    (let* ((blocklist-url (or (apm-get-websense-blocklist-url)
                              (user-error "No URL found for managing websense blocklist")))
           (url (concat blocklist-url "?page=bw_add"))
           (n-similar (+ (string-to-number (shell-command-to-string (format "notmuch count from:%s" email)))
                         (string-to-number (shell-command-to-string (format "notmuch count from:%s and tag:spam" email)))))
           (response (if (> n-similar 1)
                         (cadr
                          (read-multiple-choice
                           (format "Add %s to the blocklist with description '%s' (%d total emails from this sender)? " email description n-similar)
                           '((?y "yes" "Yes - using the suggested description")
                             (?e "edit" "Yes - but using a different description")
                             (?n "no" "No - do not add to the blocklist"))
                           nil nil (and (not use-short-answers)
                                        (not (use-dialog-box-p)))))
                       (message "Only 1 email from %s so not prompting to add to blocklist" email))))
      (unless (equal response "no")
        (when (equal response "edit")
          (setq description (read-string "Description: " description)))
        (let ((url-request-method "POST")
              (url-request-extra-headers
               '(("Content-Type" . "application/x-www-form-urlencoded")))
              (url-request-data (concat "action=save&action_general=deny&"
                                        "email_1=" (url-encode-url email) "&"
                                        "description_1=" (url-encode-url description))))
          (url-retrieve url
                        (lambda (_)
                          (let ((status (url-http-symbol-value-in-buffer
                                         'url-http-response-status (current-buffer))))
                            (pcase status
                              (200 (message "Added %s to the blocklist with description '%s'" email description))
                              (_ (user-error "Failed to add %s to the blocklist" email))))))
          t))))
  ;; requires to have set the following in ~/.notmuch-config so that the X-MailControl-ReportSpam header is available
  ;;
  ;; [show]
  ;; extra_headers=X-MailControl-ReportSpam;Archived-At
  (define-advice notmuch-show-tag (:around (orig-fun &rest args) prompt-report-spam-around-notmuch-show-tag)
    "If tagging as spam then prompt to report to mailcontrol when supported"
    (let ((tag-changes (car args)))
      (when (seq-contains-p tag-changes "+spam" #'string=)
        (let ((subject (notmuch-show-get-subject))
              (sender (mail-extract-address-components (notmuch-show-get-from))))
          (when-let ((url (notmuch-show-get-header :X-MailControl-ReportSpam)))
            (and (apm-prompt-to-report-spam subject url)
                 (apm-prompt-to-add-email-to-forcepoint-blocklist (cadr sender) (car sender)))))))
    (apply orig-fun args))

  ;; requires to have set the following in ~/.notmuch-config so that the Archived-At header is available
  ;;
  ;; [show]
  ;; extra_headers=X-MailControl-ReportSpam;Archived-At
  (define-advice notmuch-show-stash-mlarchive-link (:around (orig-fun &rest args) use-archived-at-header-around-notmuch-show-stash-mlarchive-link)
    "Offer use of the Archived-At header if present."
    (let ((archived-at (notmuch-show-get-header :Archived-At)))
      (if archived-at
          (let ((notmuch-show-stash-mlarchive-link-alist
                 (append `(("Archived-At" . ,(lambda (id)
                                               ;; strip any leading and trailing </>
                                               (string-trim archived-at "<" ">"))))
                         notmuch-show-stash-mlarchive-link-alist))
                (notmuch-show-stash-ml-archive-link-default "Archived-At"))
            (apply orig-fun args))
        (apply orig-fun args))))

  ;; place sent in Sent/ maildir with sent tag and remove unread or inbox tags
  (setq notmuch-fcc-dirs "Sent +sent -unread -inbox")
  ;; place drafts in Drafts/ maildir
  (setq notmuch-draft-folder "Drafts")
  (setq notmuch-archive-tags '("-inbox" "-unread"))
  (setq mail-user-agent 'notmuch-user-agent)
  ;; ensure kernel team daily bug report emails display without wrapping
  (add-hook 'notmuch-show-insert-text/plain-hook 'notmuch-wash-convert-inline-patch-to-part)

  (defun apm-notmuch-wash-lp-build-log (_msg _depth)
    "Wash LP build logs in the current message."
    (apm-notmuch-show-view-lp-build-log))

  ;; automatically display and download failed LP build logs
  (add-hook 'notmuch-show-insert-text/plain-hook 'apm-notmuch-wash-lp-build-log)

  (defun apm-notmuch-wash-gfm (_msg _depth)
    "Format entire message as GFM if supported."
    ;; get entire message, use a tempt buffer to format it as GFM and then
    ;; replace message with that
    (when (fboundp 'gfm-mode)
      (let ((message (buffer-substring (point-min) (point-max))))
        (with-temp-buffer
          (delay-mode-hooks
            (gfm-mode))
          (insert message)
          (font-lock-ensure)
          (setq message (buffer-string)))
        (delete-region (point-min) (point-max))
        (insert message))))

  ;; TODO - make this configurable basd on the message itself and only run when
  ;; it looks like a plain text email with markdown contents
  ;; (add-hook 'notmuch-show-insert-text/plain-hook 'apm-notmuch-wash-gfm)

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

    (add-hook 'notmuch-show-insert-text/plain-hook 'apm-notmuch-wash-article-emphasize)
    ;; ensure hyphenated words are highlighted correctly
    (modify-syntax-entry ?- "w" notmuch-show-mode-syntax-table))

  (with-eval-after-load 'epa
    (defun apm-notmuch-wash-pgp-armor (_msg _depth)
      (let ((epa-replace-original-text t))
        (epa-decrypt-armor-in-region (point-min) (point-max))))
    (add-hook 'notmuch-show-insert-text/plain-hook 'apm-notmuch-wash-pgp-armor))

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
  (add-to-list 'notmuch-saved-searches '(:name "cvewebbot" :query "from:noreply+security-tools@canonical.com and subject:\"CVE webbot process errors\"" :key "c"))
  (add-to-list 'notmuch-saved-searches '(:name "emacs-devel" :query "tag:lists/emacs-devel and tag:inbox" :key "e"))
  (add-to-list 'notmuch-saved-searches '(:name "vince-updates" :query "from:cert+donotreply@cert.org and subject:\"New Post in Case Discussion\"" :key "v"))
  (dolist (rel '("noble" "oracular" "plucky"))
    (add-to-list 'notmuch-saved-searches `(:name ,(concat rel "-changes") :query ,(concat "tag:lists/" rel "-changes and tag:unread") :key ,(substring rel 0 1)))))

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
  :vc (:fetcher github :repo xenodium/org-block-capf)
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
  (setq org-refile-targets '(("~/git/org-files/canonical.org" :maxlevel . 4)
                             ("~/git/org-files/personal.org" :maxlevel . 2)
                             ("~/git/org-files/someday.org" :level . 1)
                             ("~/git/org-files/tickler.org" :maxlevel . 1)
                             ("~/git/org-files/notes.org" :maxlevel . 2)))
  ;; allow to refile as top-level items in files
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil))

;; add support for man: links in org documents
(use-package ol-man
  :ensure org)

(use-package ol-notmuch
  :ensure t
  :pin melpa)

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
          ("c" "TODO from canonical" todo "TODO"
           ((org-agenda-files '("~/git/org-files/canonical.org"))))
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
             "* %i%?
- %a")
            ("m" "meeting" entry (file+headline ,canonical-org "Meetings")
             "* %^{meeting title}     :meeting:
%^{meeting day+time}T
- %a%?")
            ("D" "snap-store-discussion" entry (file+olp ,canonical-org "snap forum store request discussions")
             "** %^{snap name}
- %a%?" :clock-in t :clock-keep t)
            ("T" "snap-store-tallied-processed" entry (file+olp ,canonical-org "snap forum store requests processed/tallied")
             "** %^{snap name}
- %a%?" :clock-in t :clock-keep t)
            ("P" "snapd-pr-review" entry (file+olp ,canonical-org "snapd PR reviews")
             "* [[https://github.com/snapcore/snapd/pull/%\\1][snapd PR #%^{number} %^{title}]]
- https://github.com/snapcore/snapd/pull/%\\1%?
- %a")
            ("h" "hiring-take-home-test" entry (file+olp ,canonical-org "Hiring")
             "* Review and grade take home test (%?) (=%^{application_id\\=}=)     :hiring:
- %a
- %^{link}")
            ("i" "hiring-interview" entry (file+olp ,canonical-org "Hiring")
             "* %^{candidate name} %^{role} %^{interview type} (=%^{application_id\\=}=)     :hiring:
- %a
- %^{greenhouse link}
** TODO Prepare for interview
:SCHEDULED: %^{prep day+time}T
- %\\5
- [ ] Review job description and interview notes
- [ ] Resume
- [ ] %?
- [ ] Draft questions
** Conduct interview     :meeting:
%^{interview day+time}T
** TODO Scorecard
:SCHEDULED: %^{scorecard day+time}T
- %\\5")
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

(use-package org-jira
  :ensure t
  :after org
  :preface
  (defun apm-org-jira-get-issues-assigned-to-me ()
    "Get all issues assigned to me."
    (interactive)
    (org-jira-get-issues-from-custom-jql '((:jql "assignee = currentUser() AND resolution = Unresolved ORDER BY priority DESC, updated DESC"
                                                 :limit 1000
                                                 :filename "jira-assigned-to-me"))))
  (defun apm-org-jira-get-roadmap-issues-assigned-to-me ()
    "Get all issues assigned to me tagged in the current roadmap cycle."
    (interactive)
    (org-jira-get-issues-from-custom-jql '((:jql "assignee = currentUser() AND resolution = Unresolved AND labels = '24.10' ORDER BY priority DESC, updated DESC"
                                                 :limit 1000
                                                 :filename "jira-assigned-to-me-roadmap"))))
  :config
  (setq jiralib-url "https://warthogs.atlassian.net")
  (setq org-jira-progress-issue-flow
        '(("Untriaged" . "Triaged")
          ("Triaged" . "In Progress")
          ("In Progress" . "Blocked")
          ("Blocked" . "In Review")
          ("In Review" . "To Be Deployed")
          ("To Be Deployed" . "Done")
          ("Done" . "Rejected"))))

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

(use-package ox-hugo
  :after ox
  :ensure t)

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

(use-package pod-mode
  :vc (:fetcher github :repo renormalist/emacs-pod-mode)
  :mode ("\\.pod$" . pod-mode))

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
            (require 'magit-extras))
  ;; make debian source packages work as projects
  (defun apm-project-try-debian (dir)
    "Find a debian source package from DIR but only for local files."
    (unless (file-remote-p dir)
      (let ((dir (locate-dominating-file dir "debian/control")))
        (and dir (cons 'debian dir)))))

  (cl-defmethod project-root ((project (head debian)))
    (cdr project))

  (add-hook 'project-find-functions #'apm-project-try-debian))

(use-package project-mode-line-tag
  :ensure t
  :config (project-mode-line-tag-mode 1))

(use-package projection
  :ensure t
  :hook (emacs-startup . global-projection-hook-mode)
  :bind-keymap ("C-x P" . projection-map)
  :config (with-eval-after-load 'project
            (require 'projection)))

(use-package projection-multi
  :ensure projection
  :bind (("C-x C-m" . projection-multi-compile)))

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

(use-package quilt
  :ensure t)

(use-package rainbow-mode
  :ensure t
  :defer t
  :hook ((css-mode html-mode) . rainbow-mode))

(use-package recentf
  :preface
  (defun apm-recentf-ignore-sudo (file)
    "Do not keep FILE if it requires sudo to read."
    ;; return non-nil to keep in recentf-list
    (not (string-match "^/sudo:.*" file))))
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

(use-package shr
  :custom
  (shr-use-colors nil)
  (shr-use-fonts nil))

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

(use-package smtpmail
  :custom
  (smtpmail-smtp-user "amurray")
  (smtpmail-local-domain "canonical.com")
  (smtpmail-smtp-server "smtp.canonical.com")
  (smtpmail-smtp-service 587))

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

(use-package time
  :bind ((("C-c z" . world-clock)))
  :config (let ((team '(("Australia/Adelaide" . ("amurray"))
                        ("Europe/Brussels" . ("ebarretto"))
                        ("Europe/London" . ("ChrisCoulson"))
                        ("America/Sao_Paulo" . ("leosilva" "pfsmorigo"))
                        ("America/Argentina/Buenos_Aires" . ("emitorino"))
                        ("America/New_York" . ("dan"))
                        ("America/Toronto" . ("mdeslaur"))
                        ("America/Los_Angeles" . ("jj" "sarnold" "sbeattie")))))
            ;; validate team
            (dolist (member team)
              (unless (file-exists-p (expand-file-name (car member) "/usr/share/zoneinfo"))
                (user-error "TZ %s does not exist!" (car member)))))
  (setq display-time-default-load-average nil)
  (setq display-time-use-mail-icon t)
  (setq display-time-day-and-date t)
  (setq display-time-24hr-format t)
  (display-time))

(use-package treesit-auto
  :ensure t
  :custom (treesit-auto-install 'prompt)
  :config (global-treesit-auto-mode 1))

(use-package uct
  :load-path "~/ubuntu/git/ubuntu-cve-tracker/scripts/"
  :demand t
  :bind (("C-c u u" . uct)
         ("C-c f c" . uct-find-cve)
         ("C-c u k" . uct-kernel-signoff-at-point))
  :init
  (add-hook 'magit-process-prompt-functions #'uct-magit-process-git-commit-hook))

(use-package udev-mode
  :ensure t)

(use-package ultra-scroll
  :vc (:fetcher github :repo jdtsmith/ultra-scroll)
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

(use-package usn
  :load-path "~/ubuntu/git/usn-tool/"
  :commands (usn-compose-from-draft)
  :bind (("C-c f u" . usn-show-at-point)))

(use-package uvt
  :load-path "~/ubuntu/git/ubuntu-qa-tools/vm-tools/"
  :bind (("C-c v" . uvt)))

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
  :vc (:fetcher github :repo elken/yasnippet-capf)
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
