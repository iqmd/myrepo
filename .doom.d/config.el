;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Mohammad Iqbal"
      user-mail-address "mdiqofficial@gmail.com")


;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
(setq doom-font (font-spec :family "JetBrainsMono Nerd Font Mono" :size 18)
;; (setq doom-font (font-spec :family "Hack Nerd Font Mono" :size 20)
      doom-variable-pitch-font (font-spec :family "Hack Nerd Font")
      doom-symbol-font (font-spec :family "JoyPixels")
      doom-big-font (font-spec :family "JetBrainsMono Nerd Font Mono" :size 22))
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;;

;; in ~/.doom.d/config.el
(setq doom-theme 'doom-gruvbox)

(custom-set-faces!
  '(default :background "#181818")
  '(hl-line :weight bold :background nil))

(use-package whitespace
  :hook (prog-mode . whitespace-mode))

(custom-set-faces
 '(whitespace-space ((t (:foreground "#272727"))))
 '(whitespace-trailing ((t (:background "background-color" :foreground "#272727"))))
 )

(setq whitespace-line-column 10000)


;; (defun my-indent-line ()
;;   "Indent the current line by 4 spaces."
;;   (interactive)
;;   (save-excursion
;;     (beginning-of-line)
;;     (insert "    ")))


(map!
 ;; press v multiple time to expand region
 :v "v" #'er/expand-region
 ;; 
 ;; :n "=" #'my-indent-line
 )
;; (map! :leader
;;       (:prefix-map ("a" . "applications")
;;        (:prefix ("j" . "journal")
;;         :desc "New journal entry" "j" #'org-journal-new-entry
;;         :desc "Search journal entry" "s" #'org-journal-search)))
 ;; (setq-default indent-tabs-mode nil
 ;;               tab-width 4)

;; (add-hook 'c-mode-hook
;;           (lambda ()
;;             (setq indent-tabs-mode nil)
;;             (setq tab-width 4)
;;             (setq-default evil-shift-width 4)))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

(setq org-hide-emphasis-markers t)

(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

 ;; (use-package org-roam
 ;;   :ensure t
 ;;   :custom
 ;;   (org-roam-directory (file-truename "~/roam/"))
 ;;   (org-roam-capture-templates
 ;;    '(("d" "default" plain
 ;;       "%?"
 ;;       :if-new (file+head "${slug}.org" "#+title: ${title}\n")
 ;;       :unnarrowed t)))
 ;;   :config
 ;;   (org-roam-db-autosync-enable)
 ;;   )

(use-package consult-org-roam
   :ensure t
   :after org-roam
   :init
   (require 'consult-org-roam)
   ;; Activate the minor mode
   (consult-org-roam-mode 1)
   :custom
   ;; Use `ripgrep' for searching with `consult-org-roam-search'
   (consult-org-roam-grep-func #'consult-ripgrep)
   ;; Configure a custom narrow key for `consult-buffer'
   (consult-org-roam-buffer-narrow-key ?r)
   ;; Display org-roam buffers right after non-org-roam buffers
   ;; in consult-buffer (and not down at the bottom)
   (consult-org-roam-buffer-after-buffers nil)
   :config
   ;; Eventually suppress previewing for certain functions
   (consult-customize
    consult-org-roam-forward-links
    :preview-key "M-."
    consult-org-roam-search
    :preview-key nil
    )
   :bind
   ;; Define some convenient keybindings as an addition
   ("C-c n e" . consult-org-roam-file-find)
   ("C-c n b" . consult-org-roam-backlinks)
   ("C-c n B" . consult-org-roam-backlinks-recursive)
   ("C-c n l" . consult-org-roam-forward-links)
   ("C-c n r" . consult-org-roam-search))

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/roam/"))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (setq org-roam-dailies-directory "journal")
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

;;smartparens
(sp-local-pair '(c++-mode) "<" ">" :unless '(sp-point-after-word-p))

(use-package! diff-hl
  :config
  (custom-set-faces!
    `((diff-hl-change)
      :foreground ,(doom-blend (doom-color 'bg) (doom-color 'blue) 0.5))
    `((diff-hl-insert)
      :foreground ,(doom-blend (doom-color 'bg) (doom-color 'green) 0.5)))
  )

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-height 25)
  (doom-modeline-bar-width 1)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-buffer-file-name-style 'file-name-with-project)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-buffer-modification-icon t)
  (doom-modeline-minor-modes nil)
  (doom-modeline-enable-word-count nil)
  (doom-modeline-buffer-encoding t)
  (doom-modeline-indent-info nil)
  (doom-modeline-checker-simple-format t)
  (doom-modeline-vcs-max-length 12)
  (doom-modeline-env-version t)
  (doom-modeline-irc-stylize 'identity)
  (doom-modeline-github-timer nil)
  (doom-modeline-gnus-timer nil)
  (doom-modeline-always-show-macro-register t)
  :config
  (setq doom-modeline-persp-name t)
  )

;;
;; Load custom Evil clipboard-bypass functions
(load! "extras/evil-clipboard-bypass.el")

;; (defmacro without-evil-mode (&rest do-this)
;;   ;; Check if evil-mode is on, and disable it temporarily
;;   `(let ((evil-mode-is-on (evil-mode?)))
;;      (if evil-mode-is-on
;;          (disable-evil-mode))
;;      (ignore-errors
;;        ,@do-this)
;;      (if evil-mode-is-on
;;          (enable-evil-mode))))

;; (defmacro evil-mode? ()
;;   "Checks if evil-mode is active. Uses Evil's state to check."
;;   `evil-state)

;; (defmacro disable-evil-mode ()
;;   "Disable evil-mode with visual cues."
;;   `(progn
;;      (evil-mode 0)
;;      (message "Evil mode disabled")))

;; (defmacro enable-evil-mode ()
;;   "Enable evil-mode with visual cues."
;;   `(progn
;;      (evil-mode 1)
;;      (message "Evil mode enabled")))

;; ;;;; Clipboard bypass

;; ;; delete: char
;; (evil-define-operator evil-destroy-char (beg end type register yank-handler)
;;   :motion evil-forward-char
;;   (evil-delete-char beg end type ?_))

;; ;; delete: char (backwards)
;; (evil-define-operator evil-destroy-backward-char (beg end type register yank-handler)
;;   :motion evil-forward-char
;;   (evil-delete-backward-char beg end type ?_))

;; ;; delete: text object
;; (evil-define-operator evil-destroy (beg end type register yank-handler)
;;   "Vim's 's' without clipboard."
;;   (evil-delete beg end type ?_ yank-handler))

;; ;; delete: to end of line
;; (evil-define-operator evil-destroy-line (beg end type register yank-handler)
;;   :motion nil
;;   :keep-visual t
;;   (interactive "<R><x>")
;;   (evil-delete-line beg end type ?_ yank-handler))

;; ;; delete: whole line
;; (evil-define-operator evil-destroy-whole-line (beg end type register yank-handler)
;;   :motion evil-line
;;   (interactive "<R><x>")
;;   (evil-delete-whole-line beg end type ?_ yank-handler))

;; ;; change: text object
;; (evil-define-operator evil-destroy-change (beg end type register yank-handler delete-func)
;;   (evil-change beg end type ?_ yank-handler delete-func))

;; ;; paste: before
;; (defun evil-destroy-paste-before ()
;;   (interactive)
;;   (without-evil-mode
;;      (delete-region (point) (mark))
;;      (evil-paste-before 1)))

;; ;; paste: after
;; (defun evil-destroy-paste-after ()
;;   (interactive)
;;   (without-evil-mode
;;      (delete-region (point) (mark))
;;      (evil-paste-after 1)))

;; ;; paste: text object
;; (evil-define-operator evil-destroy-replace (beg end type register yank-handler)
;;   (evil-destroy beg end type register yank-handler)
;;   (evil-paste-before 1 register))
;; ;; Clipboard bypass key rebindings
;; (define-key evil-normal-state-map "d" 'evil-destroy)
;; (define-key evil-normal-state-map "D" 'evil-destroy-line)
;; (define-key evil-normal-state-map "c" 'evil-destroy-change)
;; (define-key evil-normal-state-map "x" 'evil-destroy-char)
;; (define-key evil-normal-state-map "X" 'evil-destroy-whole-line)
;; (define-key evil-normal-state-map "Y" 'evil-copy-to-end-of-line)
;; (define-key evil-visual-state-map "P" 'evil-destroy-paste-before)
;; (define-key evil-visual-state-map "p" 'evil-destroy-paste-after)

;;
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;; NOTE
;; - Use `M-x rainbow-mode' to enable color behind the hex code
