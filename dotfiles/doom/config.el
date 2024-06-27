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
(setq doom-theme 'doom-monokai-spectrum)

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
 ;; :n "=" #'my-indent-line
 )

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

 (use-package org-roam
   :ensure t
   :custom
   (org-roam-directory (file-truename "~/roam/"))
   (org-roam-capture-templates
    '(("d" "default" plain
       "%?"
       :if-new (file+head "${slug}.org" "#+title: ${title}\n")
       :unnarrowed t)))
   :config
   )

;;smartparens
(sp-local-pair '(c++-mode) "<" ">" :unless '(sp-point-after-word-p))

;;
;;use-package! rainbow-mode

;; Whenever you reconfigure a package, make sure to wrap your config in an
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
