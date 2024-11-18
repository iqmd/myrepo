;; Set default custom file
(setq-default custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Write to it if it does not exist
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))

;; Load custom file. Don't hide errors. Hide success message
(load custom-file nil t)
;;---
;; show trailing whitespace
(setq-default show-trailing-whitespace nil)
(defun show-trailing-whitespace-hook ()
  "Hook to enable displaying trailing whitespace."
  (setq show-trailing-whitespace t))
(add-hook 'prog-mode-hook 'show-trailing-whitespace-hook)
(add-hook 'text-mode-hook 'show-trailing-whitespace-hook)
(add-hook 'org-mode-hook 'show-trailing-whitespace-hook)
(add-hook 'markdown-mode-hook 'show-trailing-whitespace-hook)

;; do not dynamically resize line number column when a digit needs to be added
(setq display-line-numbers-width-start t)

;; HACK prevent M-x from shrinking line number width
;; https://github.com/abo-abo/swiper/issues/1940#issuecomment-465374308
(setq display-line-numbers-width 3)

;; prompt y/n instead of yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;; turn off ad-redef warnings https://andrewjamesjohnson.com/suppressing-ad-handle-definition-warnings-in-emacs/
(setq ad-redefinition-action 'accept)

;; aspell setup
(setq ispell-dictionary "english"
      ;; force English dictionary, support camelCase
      ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together"))

 ;; Disable unnecessary UI elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)


;; some editor settings
(setq-default indent-tabs-mode nil    ;; indent with spaces
              tab-width 4             ;; 1 tab <=> 4 spaces
              c-basic-offset 4        ;; indentation for cc modes
              c-default-style "linux" ;; https://en.wikipedia.org/wiki/Indentation_style
              fill-column 80          ;; wrap at 80 characters for auto-fill-mode
              word-wrap t             ;; do not wrap characters in the middle of words
              truncate-lines t)       ;; do not wrap by default

;; performance stuff
;; increase amount of data which emacs can read from processes (mainly for LSP mode https://emacs-lsp.github.io/lsp-mode/page/performance/)
(setq read-process-output-max (* 1024 1024) ;; 1 MB
      ;; increase the gc threshold
      gc-cons-threshold 100000000)

;; fringe setup (left . right)
(set-fringe-mode '(4 . 4))

;; relative line numbers
(setq display-line-numbers-type 'relative)


;; I don't want line numbers in help files, dired, etc.
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'display-line-numbers-mode)
(add-hook 'conf-mode-hook 'display-line-numbers-mode)

;;---

;; highligh current line
(global-hl-line-mode 1)
;;
;; themes
(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-badger t)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;;Font
(set-face-attribute 'default nil :font "Hack Nerd Font Mono" :height 100)

;; Better startup message
(setq inhibit-startup-screen t)
;; Bootstrap `use-package`
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t) ;; enable :ensure t for all packages

;; VIM mode
(use-package evil
  :init
  (setq evil-search-module 'evil-search
	evil-ex-complete-emacs-commands nil
	evil-vsplit-window-right t
	evil-want-C-i-jump t
	evil-want-integration t
	evil-want-keybinding nil
	evil-split-window-below t
	evil-shift-round nil
	evil-want-C-u-scroll t
	evil-want-integration t
	evil-want-keybinding nil
	evil-normal-state-cursor 'box
	evil-undo-system 'undo-fu
	evil-respect-visual-line-mode t
	evil-shift-width tab-width)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-an))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; evil-commentary
(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode))

;; evil surround
(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

;; show evil actions
(use-package evil-goggles
  :after evil
  :init
  (setq evil-goggles-duration 0.1)
  ;; disable slow actions
  (setq evil-goggles-enable-change nil)
  (setq evil-goggles-enable-delete nil)
  :config
  (evil-goggles-mode))

;; evil org
(use-package evil-org
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))
;;---
;; anzu - show number of matches of search in modeline
(use-package evil-anzu
  ;:after-call evil-ex-start-search evil-ex-start-word-search evil-ex-search-activate-highlight
  :diminish
  :after evil
  :config
  (global-anzu-mode +1))

;; better isearch
(use-package ctrlf
  :config
  (ctrlf-mode +1))
;;undo
;; undo-fu/vundo stack
(use-package undo-fu
  :after evil
  :config
  ;; increase history limits
  ;; https://github.com/emacsmirror/undo-fu#undo-limits
  (setq undo-limit 6710886400 ;; 64mb.
        undo-strong-limit 100663296 ;; 96mb.
        undo-outer-limit 1006632960) ;; 960mb.
  )

(use-package undo-fu-session
  :after undo-fu
  :init
  (undo-fu-session-global-mode)
  :config
  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'")))

(use-package vundo
  ;; :straight (vundo :type git :host github :repo "casouri/vundo")
  :config
  (setq vundo-compact-display t))
;;--

;; editor config
(use-package editorconfig
  :config
  (editorconfig-mode 1))
;; smartparens
(use-package smartparens
  :config
  (require 'smartparens-config)
  ;; https://github.com/doomemacs/doomemacs/blob/a570ffe16c24aaaf6b4f8f1761bb037c992de877/modules/config/default/config.el#L108-L120
  ;; Expand {|} => { | }
  ;; Expand {|} => {
  ;;   |
  ;; }
  (dolist (brace '("(" "{" "["))
    (sp-pair brace nil
             :post-handlers '(("||\n[i]" "RET") ("| " "SPC"))
             :unless '(sp-point-before-word-p sp-point-before-same-p)))
  (defun +sp-c-setup ()
    (sp-with-modes '(c++-mode c-mode)
      ;; HACK to get around lack of ability to set a negative condition (i.e. all but these commands) for delayed insertion
      (sp-local-pair "<" ">" :when '(("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"
                                      "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z")))
      (sp-local-pair "/*" "*/" :actions '(:rem insert))))
  ;; the block comment pair seems to be overwritten after c++-mode inits, so +sp-c-setup is added as a hook for c++-mode (and c-mode)
  (+sp-c-setup)


  ;; (sp-local-pair 'tuareg-mode "sig" nil :actions :rem)
  ;; do not highlight new block when pressing enter after creating set of new parens
  ;; https://stackoverflow.com/a/26708910
  (setq sp-highlight-pair-overlay nil
        sp-highlight-wrap-overlay nil
        sp-highlight-wrap-tag-overlay nil
        show-paren-delay 0) ;; no delay for showing matching parens

  (smartparens-global-mode))
;;---

;;Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)


;; HYDRA
(use-package hydra)


;; simple hydra for resizing windows
(defhydra hydra-window (:hint nil)
  "
^Movement^  ^Resize^
---------------------------------------
_h_ ←       _H_ X←
_j_ ↓       _J_ X↓
_k_ ↑       _K_ X↑
_l_ →       _L_ X→
"
  ("h" windmove-left)
  ("j" windmove-down)
  ("k" windmove-up)
  ("l" windmove-right)

  ("H" hydra-move-splitter-left)
  ("J" hydra-move-splitter-down)
  ("K" hydra-move-splitter-up)
  ("L" hydra-move-splitter-right))

(defun hydra-move-splitter-left (arg)
  "Move window splitter left."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (shrink-window-horizontally arg)
    (enlarge-window-horizontally arg)))

(defun hydra-move-splitter-right (arg)
  "Move window splitter right."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (enlarge-window-horizontally arg)
    (shrink-window-horizontally arg)))

(defun hydra-move-splitter-up (arg)
  "Move window splitter up."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (enlarge-window arg)
    (shrink-window arg)))

(defun hydra-move-splitter-down (arg)
  "Move window splitter down."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (shrink-window arg)
    (enlarge-window arg)))

;; buffer/frame zoom hydra
(defhydra hydra-zoom (:hint nil)
  "
Buffer Zoom
-------------------------
_=_   text-scale-increase
_-_   text-scale-decrease
_r_   reset text scale

Frame Zoom
-------------------------
_M-=_ zoom-in
_M--_ zoom-out
_k_   zoom-in
_j_   zoom-out
_R_   reset frame zoom
"
  ("=" text-scale-increase)
  ("-" text-scale-decrease)
  ("r" (lambda () (interactive) (text-scale-adjust 0)))

  ("M-=" zoom-in)
  ("M--" zoom-out)
  ("k" zoom-in)
  ("j" zoom-out)
  ("R" (lambda () (interactive) (zoom-in/out 0))))


;;----

;;;; DEFUNS
;; revert buffer without confirmation
;; http://www.emacswiki.org/emacs-en/download/misc-cmds.el
(defun revert-buffer-no-confirm ()
    "Revert buffer without confirmation."
    (interactive)
    (revert-buffer :ignore-auto :noconfirm))

;; kill all other buffers
;; https://www.emacswiki.org/emacs/KillingBuffers#h5o-2
(defun kill-other-buffers ()
     "Kill all other buffers."
     (interactive)
     (mapc #'kill-buffer (delq (current-buffer) (buffer-list))))

;; open init.el
(defun open-init-file ()
  "Open the init file."
  (interactive)
  (find-file user-init-file))

;; inspired by https://owoga.com/how-to-zap-whitespace-in-emacs/
(defun delete-whitespace-left-of-cursor ()
  "Delete all whitespace to the left of cursor's current position."
  (interactive)
  (let ((skip-chars "\t\n\r ")
        (old-point (point)))
    (skip-chars-backward skip-chars)
    (let ((start (point)))
      (delete-region start old-point))))

;; I like the behaviour of evil-delete-backward-word over backward-kill-word, this is a small wrapper to make it more usable for me
(defun my-backward-kill-word ()
  "Wrapper around evil-delete-backward-word."
  (interactive)
  (if (or (bolp) (eq (current-column) (current-indentation)))
      (delete-whitespace-left-of-cursor)
    (evil-delete-backward-word)))

;; basically the same as my-backward-kill-word except it creates a space when merging lines
;; TODO repeated code, this should likely be merged into my-backward-kill-word
(defun my-backward-kill-line ()
  "Same as my-backward-kill-word, but insert a space after merging lines."
  (interactive)
  (if (or (bolp) (eq (current-column) (current-indentation)))
      (progn
        (my-backward-kill-word)
        (insert " "))
    (evil-delete-backward-word)))


;; backward-kill-word without copying to kill-ring
;; https://www.emacswiki.org/emacs/BackwardDeleteWord
(defun delete-word (arg)
  "Delete characters forward until encountering the end of a word ARG times."
  (interactive "p")
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (delete-region (point) (progn (forward-word arg) (point)))))

;; this is mainly for allowing me to use C-w to delete words in vertico buffers (see general.el for hotkeys)
(defun backward-delete-word (arg)
  "Delete characters backward until encountering the end of a word ARG times."
  (interactive "p")
  (delete-word (- arg)))


;; https://github.com/hlissner/doom-emacs/blob/master/core/autoload/text.el#L293
(defun toggle-indent-style ()
  "Toggle use of tabs or spaces."
  (interactive)
  (setq indent-tabs-mode (not indent-tabs-mode))
  (message "Indent style changed to %s" (if indent-tabs-mode "tabs" "spaces")))

;; https://www.emacswiki.org/emacs/ToggleWindowSplit
(defun toggle-window-split ()
  "Toggle horizontal/vertical split."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

;; useful to have on an easily accessible key
(defun open-scratch-buffer ()
  "Open *scractch* buffer."
  (interactive)
  (switch-to-buffer "*scratch*"))

;; useful for emacs daemon
(defun +reload-config ()
  "Reload `init.el' without closing Emacs."
  (interactive)
  (load-file user-init-file))

(defun string-equality (s1 s2)
  "Quickly test equality of two strings s1 s2."
  (interactive "sEnter first string: \nsEnter second string: ")
  (if (string= s1 s2)
      (message "Equal!")
    (message "Not equal!")))

(defun +which-function ()
  "Interactive wrapper of `which-function'"
  (interactive)
  (message (which-function)))
;;----
;; WORKSPACES/TABS
(use-package tab-bar
  :init
  ;; remember window configuration changes
  (tab-bar-history-mode 1)
  :custom
  ;; hide tab back/forward buttons for tab-bar-history-mode
  (tab-bar-format '(tab-bar-format-tabs tab-bar-separator tab-bar-format-add-tab))

  :config
  (setq tab-bar-show nil
        tab-bar-close-button-show nil
        tab-bar-new-button-show nil))


(defun +tab-bar--tab-names-all ()
  "Return list of names of all tabs in current frame."
  (let ((tabs (tab-bar-tabs)))
    (mapcar (lambda (tab)
              (alist-get 'name tab))
            tabs)))

(defun +tab-bar--tabline ()
  "Build string containing tab list, with the current tab highlighted."
  (let ((num-tabs (length (tab-bar-tabs)))
        (curr-index (tab-bar--current-tab-index))
        (tab-names (+tab-bar--tab-names-all)))
    (mapconcat
     #'identity
     (cl-loop for index from 0 to (1- num-tabs)
              collect
              (propertize (format " [%d] %s " (1+ index) (nth index tab-names))
                          'face (if (= curr-index index)
                                    'highlight
                                  'default)))
     " ")))

(defun +tab-bar--message (msg &optional face-type)
  "Build string containig tab tabline and custom message MSG with optional face type FACE-TYPE."
  (concat (+tab-bar--tabline)
          (propertize " | " 'face 'font-lock-comment-face)
          (propertize (format "%s" msg)
                      'face face-type))) ;; TODO check for face-type

(defun +tab-bar-message (msg &optional face-type)
  "Display tab tabline and custom message MSG with optional face type FACE-TYPE."
  (message "%s" (+tab-bar--message msg face-type)))

(defun +tab-bar/display ()
  "Display tab tabline at the bottom of the screen."
  (interactive)
  (message "%s" (+tab-bar--tabline)))


(defun +tab-bar/switch-by-index (index)
  "Switch to tab at index INDEX, if it exists."
  (interactive "P")
  (let ((curr-index (tab-bar--current-tab-index))
        (num-tabs (length (tab-bar-tabs))))
    (if (>= index num-tabs)
        (+tab-bar-message (format "Invalid tab index %d" (1+ index)) 'error)
      (if (eq index curr-index)
          (+tab-bar-message (format "Already in tab %d" (1+ index)) 'warning)
        (tab-bar-select-tab (1+ index)) ;; NOTE this index starts at 1 for this function
        (+tab-bar/display)))))


(defun +tab-bar/add-new ()
  "Create a new tab at the end of the list."
  (interactive)
  (let ((index (length (tab-bar-tabs))))
    (tab-bar-new-tab index)
    ;(open-scratch-buffer)
    (+tab-bar/display)))

(defun +tab-bar/close-tab ()
  "Close current tab and display tabline."
  (interactive)
  (let ((curr-index (tab-bar--current-tab-index))
        (num-tabs (length (tab-bar-tabs))))
    (if (eq num-tabs 1)
        (+tab-bar-message (format "Cannot kill last tab [%s]" (1+ curr-index)) 'error)
      (tab-bar-close-tab)
      (+tab-bar-message (format "Killed tab [%s]" (1+ curr-index)) 'success))))

(defun +tab-bar/close-all-tabs-except-current ()
  "Close all tabs other than the current tab."
  (interactive)
  (let ((curr-index (tab-bar--current-tab-index))
        (num-tabs (length (tab-bar-tabs))))
    (mapc (lambda
            (index)
            (unless (eq index (1+ curr-index))
              (tab-bar-close-tab index)))
          ;; reverse list because going from first to last tab breaks indexing
          (reverse (number-sequence 1 num-tabs)))
    (+tab-bar-message (format "Killed all tabs other than [%s]" (1+ curr-index)) 'success)))

(defun +tab-bar/switch-to-next-tab ()
  "Switch to the next tab."
  (interactive)
  (tab-bar-switch-to-next-tab)
  (+tab-bar/display))

(defun +tab-bar/switch-to-prev-tab ()
  "Switch to the previous tab."
  (interactive)
  (tab-bar-switch-to-prev-tab)
  (+tab-bar/display))

(defun +tab-bar/switch-to-recent-tab ()
  "Switch to the most recently visited tab."
  (interactive)
  (tab-bar-switch-to-recent-tab)
  (+tab-bar/display))
;;----


;;; GENERAL.EL
;; https://github.com/hlissner/doom-emacs/blob/master/modules/config/default/config.el#L6
(defvar default-minibuffer-maps
  (append '(minibuffer-local-map
            minibuffer-local-ns-map
            minibuffer-local-completion-map
            minibuffer-local-must-match-map
            minibuffer-local-isearch-map
            read-expression-map))
  "A list of all the keymaps used for the minibuffer.")

;; general keybindings
(use-package general
  :config
  (general-evil-setup t)
  (defconst my-leader "SPC")
  (general-create-definer my-leader-def
    :prefix my-leader)
  (general-override-mode) ;; https://github.com/noctuid/general.el/issues/99#issuecomment-360914335
  ;; doomesque hotkeys using spacebar as prefix
  (my-leader-def
    :states '(motion normal visual)
    :keymaps 'override ;; https://github.com/noctuid/general.el/issues/99#issuecomment-360914335

    ;; map universal argument to SPC-u
    "u" '(universal-argument :which-key "Universal argument")
    ";" '(eval-region :which-key "eval-region")
    "SPC" '(projectile-find-file :which-key "Projectile find file")
    "C-SPC" '(projectile-find-file-other-frame :which-key "Projectile find file (new frame)")
    "S-SPC" '(projectile-find-file-other-frame :which-key "Projectile find file (new frame)")
    "." '(find-file :which-key "Find file")
    ">" '(find-file-other-frame :which-key "Find file (new frame)")
    "," '(consult-buffer :which-key "consult-buffer")
    ;":" '(execute-extended-command :which-key "M-x")
    "x" '(open-scratch-buffer :which-key "Open scratch buffer")
    "d" '(dired-jump :which-key "dired-jump")
    "/" '(+consult/ripgrep :which-key "+consult/ripgrep")
    "?" '(consult-ripgrep :which-key "consult-ripgrep")
    ;"[" '(+tab-bar/switch-to-prev-tab :which-key "+tab-bar/switch-to-prev-tab")
    ;"]" '(+tab-bar/switch-to-next-tab :which-key "+tab-bar/switch-to-next-tab")
    "v" '(vterm-toggle :which-key "vterm-toggle")
    "a" '(ace-window :which-key "ace-window")
    "l" '(ace-window :which-key "ace-window")

    ;; editor
    "e" '(:ignore t :which-key "Editor")
    "eu" '(vundo :which-key "vundo")
    "ev" '(vundo :which-key "vundo")
    "er" '(query-replace :which-key "query-replace")
    "ec" '(consult-theme :which-key "consult-theme")
    "ep" '(point-to-register :which-key "point-to-register")
    "es" '(consult-register-store :which-key "consult-register-store")
    "ej" '(jump-to-register :which-key "jump-to-register")
    "ef" '(:ignore t :which-key "Fold")
    "efh" '(hs-hide-block :which-key "hs-hide-block")
    "efs" '(hs-show-block :which-key "hs-show-block")
    "efa" '(hs-show-all :which-key "hs-show-all")

    ;; consult
    "c" '(:ignore t :which-key "consult")
    ;"cf" '(consult-flycheck :which-key "consult-flycheck")
    "cf" '(consult-flymake :which-key "consult-flymake")

    ;; buffer
    ;"TAB" '(switch-to-prev-buffer :which-key "Prev buffer")
    "b" '(:ignore t :which-key "Buffer")
    "bb" '(consult-buffer :which-key "consult-buffer")
    "b[" '(previous-buffer :which-key "Previous buffer")
    "b]" '(next-buffer :which-key "Next buffer")
    "bd" '(kill-current-buffer :which-key "Kill buffer")
    "bk" '(kill-current-buffer :which-key "Kill buffer")
    "bl" '(evil-switch-to-windows-last-buffer :which-key "Switch to last buffer")
    "br" '(revert-buffer-no-confirm :which-key "Revert buffer")
    "bK" '(kill-other-buffers :which-key "Kill other buffers")

    ;; open
    "o" '(:ignore t :which-key "Open")
    "oc" '(open-init-file :which-key "Open init.el")

    ;; project
    "p" '(:ignore t :which-key "Project")
    "pp" '(projectile-switch-project :which-key "Switch Project")
    "po" '(projectile-find-other-file :which-key "projectile-find-other-file")

    ;; help
    "h" '(:ignore t :which-key "Help")
    "hf" '(helpful-callable :which-key "describe-function")
    "hk" '(helpful-key :which-key "describe-key")
    "hv" '(helpful-variable :which-key "describe-variable")
    "ho" '(helpful-symbol :which-key "describe-symbol")
    "hm" '(describe-mode :which-key "describe-mode")
    "hF" '(describe-face :which-key "describe-face")
    "hw" '(where-is :which-key "where-is")
    "h." '(display-local-help :which-key "display-local-help")

    ;; zoom
    ;; the hydra is nice but the rest is kind of janky, need to play around with this more
    "=" '(text-scale-increase :which-key "text-scale-increase")
    "-" '(text-scale-decrease :which-key "text-scale-decrease")
    "z" '(:ignore t :which-key "zoom")
    "z=" '(zoom-in :which-key "zoom-in")
    "z-" '(zoom-out :which-key "zoom-out")
    "zz" '(hydra-zoom/body :which-key "hydra-zoom")

    ;; window
    "w" '(:ignore t :which-key "Window")
    "ww" '(ace-window :which-key "ace-window")
    "wt" '(toggle-window-split :which-key "toggle-window-split")
    "wa" '(ace-window :which-key "ace-window")
    "wr" '(hydra-window/body :which-key "hydra-window")
    "wv" '(evil-window-vsplit :which-key "vertical window split")
    "wc" '(evil-window-delete :which-key "delete window")

    ;; toggles
    "t" '(:ignore t :which-key "Toggles")
    "ta" '(corfu-mode :which-key "corfu-mode") ;; 'a' for autocomplete
    "ts" '(flyspell-mode :which-key "flyspell-mode")
    ;"tc" '(flycheck-mode :which-key "flycheck-mode")
    "tc" '(flymake-mode :which-key "flymake-mode")
    ;"tm" '(minimap-mode :which-key "minimap-mode")
    "tg" '(evil-goggles-mode :which-key "evil-goggles")
    "tI" '(toggle-indent-style :which-key "Indent style")
    "tv" '(visual-line-mode :which-key "visual-line-mode")

    ;; notes/org
    "n" '(:ignore t :which-key "Notes")
    "nf" '(org-roam-node-find :which-key "find-node")
    "ni" '(org-roam-node-insert :which-key "insert-node")
    "nt" '(org-roam-dailies-goto-today :which-key "org-roam-dailies-goto-today")
    "n/" '(+consult/org-roam-ripgrep :which-key "+consult/org-roam-ripgrep")
    "na" '(org-agenda :which-key "org-agenda")

    ;; narrow
    "N" '(:ignore t :which-key "Narrow")
    "Nr" '(narrow-to-region :which-key "narrow-to-region")
    "Nw" '(widen :which-key "widen")

    ;; tabs
    "TAB" '(:ignore t :which-key "Tabs")
    "TAB TAB" '(tab-bar-switch-to-tab :which-key "tab-bar-switch-to-tab")
    "TAB [" '(+tab-bar/switch-to-prev-tab :which-key "+tab-bar/switch-to-prev-tab")
    "TAB ]" '(t+ab-bar/switch-to-next-tab :which-key "+tab-bar/switch-to-next-tab")
    "TAB n" '(+tab-bar/add-new :which-key "+tab-bar/add-new")
    "TAB k" '(+tab-bar/close-tab :which-key "+tab-bar/close-tab")
    "TAB d" '(+tab-bar/close-tab :which-key "+tab-bar/close-tab")
    "TAB K" '(+tab-bar/close-all-tabs-except-current :which-key "+tab-bar/close-all-tabs-except-current")
    "TAB r" '(tab-rename :which-key "tab-rename")

    ;; quick tab switching
    "1" '((lambda () (interactive) (+tab-bar/switch-by-index 0)) :which-key nil)
    "2" '((lambda () (interactive) (+tab-bar/switch-by-index 1)) :which-key nil)
    "3" '((lambda () (interactive) (+tab-bar/switch-by-index 2)) :which-key nil)
    "4" '((lambda () (interactive) (+tab-bar/switch-by-index 3)) :which-key nil)
    "5" '((lambda () (interactive) (+tab-bar/switch-by-index 4)) :which-key nil)
    "6" '((lambda () (interactive) (+tab-bar/switch-by-index 5)) :which-key nil)
    "7" '((lambda () (interactive) (+tab-bar/switch-by-index 6)) :which-key nil)
    "8" '((lambda () (interactive) (+tab-bar/switch-by-index 7)) :which-key nil)
    "9" '((lambda () (interactive) (+tab-bar/switch-by-index 8)) :which-key nil)

    ;; git
    "g" '(:ignore t :which-key "Git") ; prefix
    "gg" '(magit-status :which-key "Git status"))

  ;; minibuffer keybindings
  (general-define-key
    :keymaps default-minibuffer-maps
    [escape] 'abort-recursive-edit ;; escape should always quit

    "C-a" 'move-beginning-of-line
    "C-e" 'move-end-of-line

    "DEL" 'backward-delete-word
    "C-v" 'yank)

  ;; evil bindings
  ;; TODO this is a bit of a mess, I need to go through the state hierarchy to define hotkeys in highest priority
  ;; normal/visual mode hotkeys
  (general-define-key
    :states '(normal visual)
    ;; evil numbers
    "g=" 'evil-numbers/inc-at-pt
    "g-" 'evil-numbers/dec-at-pt

    ;; go to references
    "gr" 'xref-find-references
    "gD" 'xref-find-references

    ;; flyspell correct
    "z=" 'flyspell-correct-wrapper
    "C-;" 'flyspell-correct-wrapper

    ;; movement
    "C-n" 'evil-next-visual-line ;; TODO should be in motion? doesn't seem to go down to these states? DELETEME
    "C-p" 'evil-previous-visual-line
    "M-n" 'flymake-goto-next-error
    "M-p" 'flymake-goto-prev-error
    "s" 'avy-goto-char-timer)

  ;; insert mode hotkeys
  (general-define-key
    :states 'insert
    "C-SPC" 'completion-at-point ;; bring up corfu completion
    "C-v" 'yank ;; C-v should paste clipboard contents

    "C-<backspace>" 'my-backward-kill-word
    "M-<backspace>" 'my-backward-kill-line

    ;; some emacs editing hotkeys inside insert mode
    "C-a" 'evil-beginning-of-visual-line
    "C-e" 'evil-end-of-visual-line
    "C-n" 'evil-next-visual-line
    "C-p" 'evil-previous-visual-line
    "C-k" 'kill-whole-line
    )

  ;; motion mode hotkeys, inherited by normal/visual
  (general-define-key
    :states 'motion
    "?" '+consult-line

    ;; window management
    ;"C-w C-u" 'winner-undo
    ;"C-w u" 'winner-undo
    "C-w C-u" 'tab-bar-history-back
    "C-w u" 'tab-bar-history-back

    "C-w a" 'ace-window
    "C-w C-w" 'ace-window
    "C-w w" 'ace-window

    "C-w C-l" 'evil-window-right
    "C-w C-h" 'evil-window-left)

  ;; unbind C-z from evil
  (general-unbind '(motion insert) "C-z")

  ;; key bindings for evil search ('/')
  ;; there could be a better way to do this, but this works so whatever
  (general-define-key
    ;; NOTE evil-ex-map is different from evil-ex-search-keymap
    :keymaps 'evil-ex-search-keymap
    ;; C-v should paste clipboard contents
    "C-v" 'yank)

  ;; global
  (general-define-key
    ;; more traditional zoom keys
    "C-=" 'text-scale-increase
    "C--" 'text-scale-decrease
    "C-M-=" 'zoom-in
    "C-M--" 'zoom-out

    "C-M-SPC" 'eldoc-box-eglot-help-at-point ;; show documentation for function at point

     ;; C-v to paste (or "yank" in emacs jargon) from clipboard, useful for minibuffers (such as query-replace and M-x)
    "C-v" 'yank

    ;; buffer management
    ;; TODO figure this out
    "C-a" 'bury-buffer
    "C-S-a" 'unbury-buffer

    ;; tab cycling
    "C-<tab>" '+tab-bar/switch-to-next-tab
    "C-<iso-lefttab>" '+tab-bar/switch-to-prev-tab
    "C-S-<tab>" '+tab-bar/switch-to-prev-tab
    "<backtab>" '+tab-bar/switch-to-recent-tab

    ;; quick tab switching
    "M-1" (lambda () (interactive) (+tab-bar/switch-by-index 0))
    "M-2" (lambda () (interactive) (+tab-bar/switch-by-index 1))
    "M-3" (lambda () (interactive) (+tab-bar/switch-by-index 2))
    "M-4" (lambda () (interactive) (+tab-bar/switch-by-index 3))
    "M-5" (lambda () (interactive) (+tab-bar/switch-by-index 4))
    "M-6" (lambda () (interactive) (+tab-bar/switch-by-index 5))
    "M-7" (lambda () (interactive) (+tab-bar/switch-by-index 6))
    "M-8" (lambda () (interactive) (+tab-bar/switch-by-index 7))
    "M-9" (lambda () (interactive) (+tab-bar/switch-by-index 8)))

  ;; magit
  (general-define-key
    ;; https://github.com/emacs-evil/evil-magit/issues/14#issuecomment-626583736
    :keymaps 'transient-base-map
    "<escape>" 'transient-quit-one)

  ;; magit keybindings
  ;; TODO refactor within use-package
  (general-define-key
    :states '(normal visual)
    :keymaps 'magit-mode-map
    ;; rebind "q" in magit-status to kill the magit buffers instead of burying them
    "q" '+magit/quit

    ;; tab switching within magit
    "M-1" (lambda () (interactive) (+tab-bar/switch-by-index 0))
    "M-2" (lambda () (interactive) (+tab-bar/switch-by-index 1))
    "M-3" (lambda () (interactive) (+tab-bar/switch-by-index 2))
    "M-4" (lambda () (interactive) (+tab-bar/switch-by-index 3))
    "M-5" (lambda () (interactive) (+tab-bar/switch-by-index 4))
    "M-6" (lambda () (interactive) (+tab-bar/switch-by-index 5))
    "M-7" (lambda () (interactive) (+tab-bar/switch-by-index 6))
    "M-8" (lambda () (interactive) (+tab-bar/switch-by-index 7))
    "M-9" (lambda () (interactive) (+tab-bar/switch-by-index 8)))

  ;; org mode specific evil binding
  ;; unbind the return (enter) key so it becomes org-return
  ;; the return key is not that useful here anyways
  (general-define-key
    :states 'motion
    :keymaps 'org-mode-map
    :major-modes t
    "RET" 'org-return))
;;----

;;; VERTICO
;; vertico - completion engine
(use-package vertico
  :init
  (vertico-mode)
  ;; https://systemcrafters.cc/emacs-tips/streamline-completions-with-vertico/
  :general
  (:keymaps 'vertico-map
    "C-j" 'vertico-next
    "C-k" 'vertico-previous)
  :config
  (setq vertico-resize nil
        vertico-count 17
        ;; enable cycling for `vertico-next' and `vertico-previous'
        vertico-cycle t))

;; Optionally use the `orderless' completion style. See
;; `+orderless-dispatch' in the Consult wiki for an advanced Orderless style
;; dispatcher. Additionally enable `partial-completion' for file path
;; expansion. `partial-completion' is important for wildcard support.
;; Multiple files can be opened at once with `find-file' if you enter a
;; wildcard. You may also give the `initials' completion style a try.
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; marginalia
(use-package marginalia
  :general
  ("M-A" 'marginalia-cycle)
  (:keymaps 'minibuffer-local-map
    "M-A" 'marginalia-cycle)
  :init
  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. NOTE that this forces loading the package.
  (marginalia-mode))

;; consult
(use-package consult
  :config
  ;; Use `consult-completion-in-region' if Vertico is enabled.
  ;; Otherwise use the default `completion--in-region' function.
  (setq completion-in-region-function
        (lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args))))
;; integration with flycheck
(use-package consult-flycheck
  :after (consult flycheck))


;; consult functions
(defun +consult/find-file (DIR)
  "Open file in directory DIR."
  (interactive "DSelect dir: ")
  (let ((selection (completing-read "Find file: " (split-string (shell-command-to-string (concat "find " DIR)) "\n" t))))
    (find-file selection)))

(defun +consult/ripgrep (DIR)
  "Ripgrep directory DIR."
  (interactive "DSelect dir: ")
  ;(let ((consult-ripgrep-args "rg --null --multiline --max-columns=1000 --path-separator /\ --smart-case --no-heading --line-number .")))
  (consult-ripgrep DIR))

(defun +consult/org-roam-ripgrep ()
  "Ripgrep org-directory."
  (interactive)
  (consult-ripgrep org-directory))

;; allow me to use `evil-ex-search-next' and `evil-ex-search-previous' on result from `consult-line'
(defun +consult-line ()
  "Wrapper around `consult-line' that populates evil search history."
  (interactive)
  (consult-line)
  (let ((search-pattern (car consult--line-history)))
    ;; HACK manually set the search pattern and evil ex highlighting
    (setq evil-ex-search-pattern (evil-ex-make-search-pattern search-pattern))
    (evil-ex-search-activate-highlight evil-ex-search-pattern)))


;; embark
(use-package embark
  :general
  ("C-l" 'embark-act)
  ("<mouse-3>" 'embark-act) ;; right click
  (:keymaps 'evil-normal-state-map
    "C-l" 'embark-act
    "<mouse-3>" 'embark-act)
  :init
  ;; let "C-h" after a prefix command bring up a completion search using consult
  ;; https://www.reddit.com/r/emacs/comments/otjn19/comment/h6vyx9q/?utm_source=share&utm_medium=web2x&context=3
  (setq prefix-help-command #'embark-prefix-help-command
        which-key-use-C-h-commands nil))

(use-package embark-consult
  :after (embark consult))


;; posframe
(use-package posframe)
;;----


(use-package deadgrep)


;; spelling
(use-package flyspell
  ;; turn on flyspell for magit commit
  :hook (git-commit-mode . git-commit-turn-on-flyspell))

;; spelling correction menu using completing-read (so consult)
(use-package flyspell-correct
  :after flyspell)
;;---

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))
;;--
;; which-key
(use-package which-key
    :init
    (setq which-key-sort-order #'which-key-key-order-alpha
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10)
    (which-key-mode))

;; helpful
(use-package helpful
  :init
  (defvar read-symbol-positions-list nil)
  :config
  ;; redefine help keys to use helpful functions instead of vanilla
  ;; https://github.com/Wilfred/helpful#usage
  :general ;; global
  ("C-h f" 'helpful-callable)
  ("C-h v" 'helpful-variable)
  ("C-h o" 'helpful-symbol)
  ("C-h k" 'helpful-key))

;; scrolling config
(setq scroll-step            1 ;; smooth scroll
      scroll-conservatively  10000
      fast-but-imprecise-scrolling t

      ;; https://github.com/hlissner/doom-emacs/blob/master/core/core-ui.el#L150
      hscroll-margin 2
      hscroll-step 1
      scroll-margin 0
      scroll-preserve-screen-position t
      ;; Reduce cursor lag by a tiny bit by not auto-adjusting `window-vscroll'
      ;; for tall lines.
      auto-window-vscroll nil)

;; smoother scrolling (especially for trackpad) via emacs 29
(pixel-scroll-precision-mode 1)


;; PROJECT
;; projectile
(use-package projectile
  :init
  ;; some configs that doom uses https://github.com/doomemacs/doomemacs/blob/bc32e2ec4c51c04da13db3523b19141bcb5883ba/core/core-projects.el#L29
  (setq projectile-auto-discover nil ;; too slow to discover projects automatically, use `projectile-discover-projects-in-search-path' instead
        projectile-enable-caching t  ;; big performance boost, especially for `projectile-find-file'
        projectile-globally-ignored-files '(".DS_Store" "TAGS")
        projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o"))
        ;; projectile-project-search-path '("~/Documents/Code"))
  :config
  (projectile-mode +1))
;;----

;; ace window
(use-package ace-window)

;; GIT
;; magit
(use-package magit
  ;; refresh status when you save file being tracked in repo
  :hook (after-save . magit-after-save-refresh-status)
  ;; start magit comm it in insert mode https://emacs.stackexchange.com/a/20895
  :hook (git-commit-mode . evil-insert-state)
  :config
  ;; display magit status in current buffer (no popup) https://stackoverflow.com/a/58554387/11312409
  (setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1
        magit-auto-revert-mode t
        ;; highlight commit message after 50 characters
        git-commit-summary-max-length 50
        ;; NOTE this is apparently DEPRECATED but it seems to do exactly what I want (autowrap commmit body at 72 chars)
        git-commit-fill-column 72))

;; magit functions
;; inpsired by https://www.manueluberti.eu/emacs/2018/02/17/magit-bury-buffer/
(defun magit-kill-buffers ()
  "Restore window configuration and kill all magit buffers."
  (let ((buffers (magit-mode-get-buffers)))
    (mapc #'kill-buffer buffers)))

;; TODO also kill diff buffer
(defun +magit/quit (&optional kill-buffer)
  "Bury the current magit Buffer.
If KILL-BUFFER, kill this buffer instead of burying it.
If the buried/killed magit buffer was the last magit buffer open for this repo,
kill all magit buffers for this repo."
  (interactive "P")
  (let ((toplevel (magit-toplevel)))
    (progn
      (funcall magit-bury-buffer-function kill-buffer))
      (unless (cl-find-if (lambda (win)
                            (with-selected-window win
                              (and (derived-mode-p 'magit-mode)
                                   (equal magit--default-directory toplevel))))
                          (window-list))
        (magit-kill-buffers))))

;; show todos in magit status
(use-package magit-todos)


(use-package git-gutter
  :init
  (global-git-gutter-mode 1))

(use-package git-gutter-fringe
  :config
  ;; pretty diff indicators
  ;; https://github.com/hlissner/doom-emacs/blob/master/modules/ui/vc-gutter/config.el#L106
  (setq-default fringes-outside-margins t)
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))
;;----

;; ORG
(setq +org-google-dir "~/")

(use-package org
  ;; HACK (?) prevents needed `org-reload' to fix org agenda (which seems to break org mode)
  ;; https://www.reddit.com/r/emacs/comments/rr203h/using_straightel_and_usepackage_to_configure_org/hqdzpc5/
  ;; :straight (:type built-in)
  ;:hook (org-mode . org-indent-mode)  ;; indent org stuff
  :hook (org-mode . visual-line-mode) ;; wrap lines
  :hook (org-mode . flyspell-mode)    ;; spelling
  :hook (org-tab-first-hook . +org-indent-maybe-h) ;; doom's TAB key behaviour
  :config
  (setq org-agenda-span 10 ; https://stackoverflow.com/a/32426234
        org-agenda-start-on-weekday nil
        org-agenda-files (list (format "%s/roam/agenda" +org-google-dir)) ;; https://stackoverflow.com/a/11384907
        org-agenda-window-setup 'current-window ;; open agenda in current window
        org-todo-keywords '((sequence "TODO(t)" "EXAM(e)" "WAIT(w)" "|" "DONE(d)" "KILL(k)" "SKIPPED(s)" "LATE(s)"))
        org-return-follows-link t
        org-directory +org-google-dir
        org-src-tab-acts-natively t ;; https://stackoverflow.com/a/27236621/11312409
        org-src-preserve-indentation t
        org-edit-src-content-indentation 0 ;; https://www.reddit.com/r/orgmode/comments/mobien/org_mode_code_block_indentation/gu3jjkg/
        org-fontify-whole-heading-line t
        org-fontify-quote-and-verse-blocks t))

;; small snippet that I often use in org
(defun +org-insert-source-block ()
  "Insert code block, force insert mode."
  (interactive)
  (insert "#+BEGIN_SRC

#+END_SRC")
  (forward-line -1)
  (goto-char (line-end-position))
  (evil-insert 0))

;;org-roam
(use-package org-roam
  :custom
  (org-roam-directory (file-truename (format "%s/roam" +org-google-dir)))
  :config
  (setq org-roam-completion-everywhere t)
  (org-roam-db-autosync-mode))

;; prettier headings
(use-package org-superstar)
(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))

;;---
(defun my/eglot-capf ()
  (setq-local completion-at-point-functions
              (list (cape-capf-super
                     #'eglot-completion-at-point
                     #'cape-file))))

;;Languages
(use-package eglot
  ;; https://github.com/minad/corfu/wiki
  :init
  (setq completion-category-overrides '((eglot (styles orderless))))
  :config
  ;; https://github.com/joaotavora/eglot/discussions/898#discussioncomment-2609402
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              ;; Show flymake diagnostics first.
              ;; Set up custom CAPF for eglot with cape
              (my/eglot-capf)
              (setq eldoc-documentation-functions
                    (cons #'flymake-eldoc-function
                          (remove #'flymake-eldoc-function eldoc-documentation-functions)))
              ;; Show all eldoc feedback.
              (setq eldoc-documentation-strategy #'eldoc-documentation-compose)
              ;; disable inlays
              (eglot-inlay-hints-mode -1)))
  ;; prevent multi-line prompts in minibuffer
  ;; https://github.com/joaotavora/eglot/discussions/734#discussioncomment-1286838
  (setq eldoc-echo-area-use-multiline-p nil)
        ;;; Show flymake diagnostics first.
        ;eldoc-documentation-functions (cons #'flymake-eldoc-function (remove #'flymake-eldoc-function eldoc-documentation-functions))
        ;;; Show all eldoc feedback.
        ;eldoc-documentation-strategy #'eldoc-documentation-compose)

;  ;; do not show eldoc in minibuffer
;  (add-to-list 'eglot-ignored-server-capabilites :hoverProvider)
;  (setq eldoc-documentation-strategy 'eldoc-documentation-default)
  (add-to-list 'eglot-server-programs
               '(java-mode . ("jdtls" "~/.m2/repository/org/projectlombok/lombok/1.18.20/lombok-1.18.20.jar"))) ;; Optional: Add Lombok support
  :hook
  (java-mode . eglot-ensure))


(use-package consult-eglot)

;; don't show the doc in the minibuffer (I find it distracting)
;(use-package eldoc-box
;  :hook (eglot-mode . eldoc-box-hover-mode)
;  ;; TODO doesn't work, using hover mode for now
;  ;; hide eglot's eldoc in the minibuffer
;  ;:hook (eglot-mode . eldoc-box-quit-frame)
;  )

;; dumb jump (indexless code navigation)
;; TODO configure with lsp
(use-package dumb-jump
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read ;; use vertico to handle ambiguous cases
        dumb-jump-force-searcher 'rg) ;; force dumb jump to use ripgrep (NOTE still defaults to git-grep if file is in git project)

  ;; preserve jump list in evil https://skeptric.com/dumbjump/
  (defun evil-set-jump-args (&rest ns) (evil-set-jump))
  (advice-add 'dumb-jump-goto-file-line :before #'evil-set-jump-args))


;; TODO treesitter

;; CODE AUTOCOMPLETE
;; TODO configure
(use-package cape
  ;; Bind dedicated completion commands
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p a" . cape-abbrev)
         ("C-c p i" . cape-ispell)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-tex)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-tex)
)

(advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)

(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-preview-current nil)    ;; Disable current candidate preview
  (corfu-count 14)
  (corfu-echo-documentation t)
  (corfu-quit-at-boundary nil)     ;; Stay active even if input is incomplete
  (corfu-preview-current nil)      ;; Don't preview the first candidate
  :init
  (global-corfu-mode))             ;; Enable Corfu globally

;;java
(use-package jarchive
  :ensure t
  :after eglot
  :config
  (jarchive-setup))


;; (defvar +eglot/initialization-options-map (make-hash-table :size 5))

;; (cl-defmethod eglot-initialization-options ((server eglot-lsp-server))
;;     (if-let ((init-options (gethash (eglot--major-mode server) +eglot/initialization-options-map)))
;;         init-options
;;       eglot--{}))

;; (add-to-list 'eglot-server-programs
;;                `(java-mode "jdtls"
;;                            "-configuration" ,(expand-file-name "cache/language-server/java/jdtls/config_linux" user-emacs-directory)
;;                            "-data" ,(expand-file-name "cache/java-workspace" user-emacs-directory)
;;                            ,(concat "--jvm-arg=-javaagent:" (expand-file-name "~/.m2/repository/org/projectlombok/lombok/1.18.20/lombok-1.18.20.jar"))
;;                            ))

;; (puthash 'java-mode
;;            `(:settings
;;              (:java
;;               (:configuration
;;                (:runtime [(:name "JavaSE-1.8" :path "/usr/local/jdk-8")
;;                           (:name "JavaSE-11" :path "/usr/local/graalvm-ce-java11-22.0.0.2")
;;                           (:name "JavaSE-17" :path "/usr/local/graalvm-ce-java17-22.0.0.2" :default t)])
;;                :format (:settings (:url ,(expand-file-name (locate-user-emacs-file "cache/eclipse-java-google-style.xml"))
;;                                         :profile "GoogleStyle"))
;;                ;; NOTE: https://github.com/redhat-developer/vscode-java/issues/406#issuecomment-356303715
;;                ;; > We enabled it by default so that workspace-wide errors can be reported (eg. removing a public method in one class would cause compilation errors in other files consuming that method).
;;                ;; for large workspaces, it may make sense to be able to disable autobuild if it negatively impacts performance.
;;                :autobuild (:enabled t)
;;                ;; https://github.com/dgileadi/vscode-java-decompiler
;;                :contentProvider (:preferred "fernflower")))
;;              ;; WIP: support non standard LSP `java/classFileContents', `Location' items that have a `jdt://...' uri
;;              ;; https://github.com/eclipse/eclipse.jdt.ls/issues/1384
;;              ;; nvim impl demo: https://github.com/mfussenegger/dotfiles/commit/3cddf73cd43120da2655e2df6d79bdfd06697f0e
;;              ;; lsp-java impl demo: https://github.com/emacs-lsp/lsp-java/blob/master/lsp-java.el
;;              :extendedClientCapabilities (:classFileContentsSupport t)
;;              ;; bundles: decompilers, etc.
;;              ;; https://github.com/dgileadi/dg.jdt.ls.decompiler
;;              :bundles ,(let ((bundles-dir (expand-file-name (locate-user-emacs-file "cache/language-server/java/bundles" user-emacs-directory)))
;;                              jdtls-bundles)
;;                          (->> (when (file-directory-p bundles-dir)
;;                                 (directory-files bundles-dir t "\\.jar$"))
;;                               (append jdtls-bundles)
;;                               (apply #'vector))))
;;            +eglot/initialization-options-map)

;; (add-hook 'java-mode-hook #'eglot-ensure)
;;---
;; json
(use-package json-mode)


;; yaml
(use-package yaml-mode)


;; html/css
(use-package web-mode
  :mode "\\.html\\'")


;; javascript
(use-package js2-mode
  :mode "\\.js\\'"
  ;; indent with spaces https://stackoverflow.com/a/7957258/11312409
  :hook (js2-mode . (lambda () (set-variable 'indent-tabs-mode nil))))

;; typscript
(use-package typescript-mode
  :mode "\\.ts\\'")
;; flymake
(use-package flymake
  :config
  (setq help-at-pt-display-when-idle t))

(use-package flymake-collection
  :hook (after-init . flymake-collection-hook-setup))
;;----
