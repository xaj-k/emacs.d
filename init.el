;;;; .emacs

;(package-initialize)

;; Set up the location(s) where emacs packages are to be obtained
(setq package-archives
      `(("org" . "http://orgmode.org/elpa/")
        ("melpa" . "http://melpa.org/packages/")
        ("gnu" . "http://elpa.gnu.org/packages/")
        ("local-misc" . ,(concat user-emacs-directory "local-elpa-misc/"))))

;; Uncomment the next line below to use a local cache of elpa/melpa instead
;(setq package-archives `(("local-elpa" . ,(concat user-emacs-directory "local-elpa/"))))

;; Settings modified via the Customize interface get their own file. Set this up
;; early in case any of the other init functions use the `customize' interface.
(setq custom-file (concat user-emacs-directory "settings.el"))

;; Set load paths for lisp files
(setq site-lisp (concat user-emacs-directory "site-lisp/"))
(add-to-list 'load-path site-lisp)

(require 'nispio/misc-utils)
(setq site-lisp-subdirs
      ;; The "nispio" dir is excluded because its packages are provided as
      ;; (provide 'nispio/package-name)
      (nispio/directory-subdirs site-lisp nil '(".hg" ".git" "nispio")))
(setq load-path (append site-lisp-subdirs load-path))

;; Make custom themes available
(customize-set-value 'custom-theme-directory (concat site-lisp "themes/"))

;; White text on black background
(setq default-frame-alist
      '((background-color . "black")
        (foreground-color . "white smoke")
        (cursor-color . "orchid")
        (font . "DejaVu Sans Mono-12")))

;; My "must-have" key bindings get set before anything can go wrong.
(global-set-key (kbd "<C-tab>") 'next-multiframe-window)
(global-set-key (kbd "<C-iso-lefttab>") 'previous-multiframe-window)
(global-set-key (kbd "M-1") 'delete-other-windows)

;; Configure UI features
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1)) ; Disable scroll bars
(when (fboundp 'fringe-mode) (fringe-mode '(nil . 0))) ; Left fringes only
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))     ; Disable toolbar
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))     ; Disable menu bar

;; Basic editor configuration
(setq-default truncate-lines t         ; Truncate lines by default
              fill-column 80           ; Set the default fill-column to 80
              tab-width 4              ; Set tab size to 4 spaces
              )
(setq inhibit-startup-screen t         ; Disable splash screen
      visible-bell t                   ; Disable system beep
      transient-mark-mode t            ; Enable visual feedback on selections
      x-stretch-cursor t               ; Cursor as wide as the glyph under it
      scroll-step 1                    ; Only scroll by one line at top/bottom
      require-final-newline t          ; Always end a file with a newline
      frame-title-format "emacs - %b"  ; Set frame title to "emacs - <buffer name>"
      enable-recursive-minibuffers t   ; Allow recursive minibuffers
      ;; Use unix line endings by default
      default-buffer-file-coding-system 'utf-8-unix
      ;; Put tab stops every 4 columns
      tab-stop-list (number-sequence 4 100 4)
      )
(minibuffer-depth-indicate-mode 1)     ; Show the current minibuffer depth if depth > 1

;; Use a tab-width of 8 by default for lisp files
(add-hook 'emacs-lisp-mode-hook (defun elisp-set-tab-width () (setq tab-width 8)))

;; Make sure that the cygwin bash executable can be found (Windows Emacs)
(when (eq system-type 'windows-nt)
  (setq explicit-shell-file-name "C:/cygwin/bin/bash.exe"
        shell-file-name explicit-shell-file-name)
  (add-to-list 'exec-path "C:/cygwin/bin"))

;; Make ibuffer auto-revert
(add-hook 'ibuffer-mode-hook 'nispio/ibuffer-auto-revert-setup)

;; Load my own minor mode for personal keybindings
(require 'nispio/my-mode)
(enable-my-global-mode)

;;
;; Set up basic keybindings in my-map
;;
(nispio/bind-keys my-map
  '(("<pause>"             . disable-my-global-mode)
    ("M-0"                 . delete-window)
    ("M-1"                 . delete-other-windows)
    ("<M-tab>"             . next-multiframe-window)
    ("<M-iso-lefttab>"     . previous-multiframe-window)
    ("<C-tab>"             . next-multiframe-window)
    ("<C-iso-lefttab>"     . previous-multiframe-window)
    ("C-x <f1>"            . nispio/buffer-file-name)
    ("<f11>"               . nispio/toggle-fullscreen)
    ("<menu>"              . menu-bar-open)
    ([remap list-buffers]  . ibuffer)

    ("C-j"         . newline-and-indent)
    ("C-H-]"       . abort-recursive-edit)
    ("H-e"         . nispio/eval-and-replace)
    ("C-h o"       . describe-face)
    ("<M-up>"      . nispio/meta-up)
    ("<M-down>"    . nispio/meta-down)
    ("<M-return>"  . nispio/meta-return)
    ("<C-m>"       . nispio/meta-return)
    ("<S-prior>"   . nispio/scroll-down-lines)
    ("<S-next>"    . nispio/scroll-up-lines)

    ("C-c c"       . comment-region)
    ("C-c u"       . uncomment-region)
    ("C-x DEL"     . nispio/strip-1)
    ("C-c C-n"     . nispio/end-of-column)
    ("C-c C-p"     . nispio/beginning-of-column)
    ("M-s s"       . nispio/sort-this-column)

    ("<f5>"        . nispio/revert-buffer)
    ("C-x <f5>"    . nispio/remove-properties)
    ("C-x <f6>"    . add-file-local-variable)
    ("C-H-\\"      . nispio/switch-to-scratch-and-back)
    ("C-x K"       . nispio/delete-this-file)

    ("M-s n"       . find-name-dired)
    ("M-s N"       . nispio/dired-find-exts)
    ("M-s O"       . multi-occur-in-matching-buffers) ; TODO: evaluate
    ("M-s ."       . isearch-forward-symbol-at-point)

    ("C-S-<left>"  . shrink-window-horizontally)
    ("C-S-<right>" . enlarge-window-horizontally)
    ("C-S-<up>"    . shrink-window)
    ("C-S-<down>"  . enlarge-window)
    ("M-o 6 d"     . shrink-window-horizontally)
    ("M-o 6 c"     . enlarge-window-horizontally)
    ("M-o 6 a"     . shrink-window)
    ("M-o 6 b"     . enlarge-window)
    )) ;; End keybinding for `my-map'

;; This is a hack because my M-s keybinding disappear in some modes
(define-key my-map (kbd "M-s") (lookup-key global-map (kbd "M-s")))

;; A few bindings that belong in the global map
(global-set-key (kbd "<pause>") 'enable-my-global-mode)
(global-set-key (kbd "<C-m>") (kmacro meta-return "<M-return>"))

;; Load init files as appropriate, turning errors into messages
(with-demoted-errors "INIT ERROR: %s"
  ;; Simple minor modes
  (show-paren-mode 1)                ; Show matching parenthesis
  (global-font-lock-mode 1)          ; Enable syntax highlighting
  (column-number-mode t)             ; Show column number on mode line
  (ido-mode 1)                       ; "Interactively Do" things

  ;; Set up the emacs package system, and install `use-package'
  (require 'nispio/package-config)

  ;; Utilities for finding, describing, and generally dealing with key bindings
  (require 'nispio/key-utils)
  (nispio/unbind-digit-arguments)
  (define-key my-map (kbd "C-h C-k") 'nispio/insert-key-description)
  (define-key my-map (kbd "C-h k") 'nispio/locate-key-binding)
  (global-set-key (kbd "C-h C-M-k") 'nispio/unbind-local-key)
  (global-set-key (kbd "C-h k") 'nispio/locate-key-binding)

  ;; ;; Add greek characters to "C-x 8" map
  ;; (use-package nispio/unicode
  ;;   :ensure nil
  ;;   :init (autoload 'iso-transl-ctl-x-8-map "iso-transl")
  ;;   :bind-keymap (;:map iso-transl-ctl-x-8-map
  ;;                 ("C-x 8 g" . greek-small)
  ;;                 ("C-x 8 G" . greek-large))
  ;;   :config (require 'nispio/unicode))

  ;; Add convenience function for printing a buffer to PS/PDF
  (use-package nispio/print :ensure nil
               :bind (:map my-map ("<S-print>" . nispio/ps-print-buffer)))

  (use-package nispio/rect-utils
               :ensure nil
               :bind (:map my-map
                           ("C-x r Y" . nispio/yank-rectangle-from-kill-ring)
                           ("C-x r D" . delete-whitespace-rectangle)))

  ;; Display line numbers in all programming buffers
  (use-package linum
    :ensure nil
    :hook (prog-mode . linum-mode)
    :config
    (setq linum-format "%3d"))

  ;; Install updated org-mode features from ELPA
  (use-package org
    :ensure org-plus-contrib
    :bind (:map org-mode-map
           ("M-n" . org-next-visible-heading)
           ("M-p" . org-previous-visible-heading)))

  (use-package dired-x
    :ensure nil
    :bind (:map dired-mode-map
           ("/" . dired-isearch-filenames)
           ("F" . nispio/find-marked-files)
           ("W" . nispio/dired-copy-filename)))

  ;; Extend dired functionality
  (use-package dired+
    :bind (:map dired-mode-map ([mouse-2] . diredp-mouse-find-file))
    :custom (diredp-hide-details-initially-flag nil)
    :config (diredp-toggle-find-file-reuse-dir 1))

  ;; TODO: increase list of files grayed out by dired

  ;; The package is "python" but the mode is "python-mode":
  (use-package python
    :ensure nil
    :mode ("\\.py\\'" . python-mode)
    :interpreter ("python" . python-mode))

  ;; Move current line up and down with M-up and M-down
  ;; Define them in the global keymap so that major modes can override
  (use-package move-text
    :bind (("<M-up>" . move-text-up)
           ("<M-down>" . move-text-down)))

  ;; Easily re-arrange buffers within the frame
  ;; (source: http://www.emacswiki.org/emacs/download/buffer-move.el)
  (use-package buffer-move
    :bind (:map my-map
           ("C-c <C-up>"    . buf-move-up)
           ("C-c <C-down>"  . buf-move-down)
           ("C-c <C-left>"  . buf-move-left)
           ("C-c <C-right>" . buf-move-right)))

  ;; Add support for isearch functionality with multiple cursors
  ;; (source: https://github.com/zk-phi/phi-search)
  (use-package phi-search
    :custom (phi-search-case-sensitive 'guess)
    :bind (:map my-map
           ("C-s"   . phi-search)
           ("C-r"   . phi-search-backward)
           ("H-C-s" . isearch-forward)
           :map phi-search-default-map
           ("<S-return>" . phi-search-complete-with-selection)))

  ;; Add support for editing with multiple cursors
  ;; (source: https://github.com/magnars/multiple-cursors.el)
  (use-package multiple-cursors
    :bind (:map global-map
           ("C->"        . mc/mark-next-like-this)
           ("C-<"        . mc/mark-previous-like-this)
           ("C-c C-<"    . mc/mark-all-like-this)
           ("C-c C->"    . mc/mark-more-like-this-extended)
           ("H-C-SPC"    . nispio/fake-cursor-at-point)
           ("<H-return>" . multiple-cursors-mode)
           :map my-map
           ("C->"              . mc/mark-next-like-this)
           ("C-<"              . mc/mark-previous-like-this)
           ("C-c C-<"          . mc/mark-all-like-this)
           ("C-c C->"          . mc/mark-more-like-this-extended)
           ("C-S-c C-S-c"      . mc/edit-lines)
           ("C-S-c C-<"        . mc/mark-all-in-region)
           ("H-C-SPC"          . nispio/fake-cursor-at-point)
           ("<H-return>"       . multiple-cursors-mode)
           ("<H-C-return>"     . nispio/mc-many-cursors)
           ("<C-down-mouse-1>" . mc/toggle-cursor-on-click)
           ("<C-mouse-1>"      . ignore)
           ("M-s M-s"          . mc--mark-symbol-at-point)
           ("C-c S-C-n"        . nispio/mark-this-column)
           :map mc/keymap
           ("C-c C-v" . mc/vertical-align-with-space)
           ("M-x"     . execute-extended-command)
           ("C-!"     . nispio/mc-insert-numbers-1)
           ("C-1"     . mc/insert-numbers))
    :config
    (require 'nispio/mc-extra)
    (nispio/mc-setup-mark-lines))

  ;; Add extended interoperability between phi-search and multiple cursors
  ;; (source: https://github.com/knu/phi-search-mc.el)
  (use-package phi-search-mc
    :after (phi-search multiple-cursors)
    :config
    (phi-search-mc/setup-keys)
    (phi-search-from-isearch-mc/setup-keys))

  ;; Use phi-rectangle for rectangular selections
  (use-package phi-rectangle
    :bind (:map my-map
           ("C-c C-SPC" . phi-rectangle-set-mark-command)
           ("C-w"       . phi-rectangle-kill-region)
           ("M-w"       . phi-rectangle-kill-ring-save)
           ("C-y"       . phi-rectangle-yank)))

  ;; Add support for editing matlab files
  ;; (source: http://matlab-emacs.cvs.sourceforge.net/viewvc/matlab-emacs/matlab-emacs/?view=tar)
  (use-package "matlab"
    :ensure matlab-mode
    :mode ("\\.m\\'" . matlab-mode)
    :hook (matlab-mode . linum-mode)
    :config
    (require 'nispio/matlab-debug)
    (setq matlab-comment-column 50)
    (when (>= emacs-major-version 24) (matlab-cedet-setup)))

  ;; Enable column markers at column 91 to warn of long lines
  ;; (source: http://www.emacswiki.org/emacs/download/column-marker.el)
  (use-package column-marker
    :commands (column-marker-1)
    :hook (prog-mode . nispio/column-marker)
    :init
    (defvar-local nispio/column-marker-column 91
      "The column that should be highlighted as a long-line warning.")
    (defun nispio/column-marker (&optional col)
      "Place column-marker-1 at the column specified by `nispio/column-marker-column'"
      (interactive)
      (column-marker-1 (or col nispio/column-marker-column))))

  (use-package treemacs
    :init
    (defvar treemacs-no-load-time-warnings t
    "Suppress treemacs warnings that happen at load time")
    :bind (:map my-map
           ("M-9"       . treemacs-select-window)
           ("C-x t 1"   . treemacs-delete-other-windows)
           ("C-x t t"   . treemacs)
           ("C-x t B"   . treemacs-bookmark)
           ("C-x t C-t" . treemacs-find-file)
           ("C-x t M-t" . treemacs-find-tag)))

  (use-package treemacs-projectile
    :after (treemacs projectile))

  (use-package treemacs-icons-dired
    :after (treemacs dired)
    :config (treemacs-icons-dired-mode))

  (use-package ace-window :ensure t)

  (use-package tex-site
    :ensure auctex
    :hook ((LaTeX-mode . visual-line-mode)
           (LaTeX-mode . flyspell-mode)
           (LaTeX-mode . LaTeX-math-mode)
           (LaTeX-mode . TeX-PDF-mode)
           (LaTeX-mode . turn-on-reftex))
    :config
    (setq TeX-auto-save t
          TeX-parse-self t
          TeX-source-correlate-method (quote synctex)
          TeX-source-correlate-mode t
          TeX-source-correlate-start-server t
          reftex-plug-into-AUCTeX t)
    (setq-default TeX-master nil))

  ;; ;; SrSpeedbar allows a speedbar that is "docked" in the current frame
  ;; (use-package sr-speedbar :ensure t)
  ;; (define-key my-map (kbd "C-c M-SPC") 'sr-speedbar-toggle)
  ;; ;(define-key nispio/gdb-window-map (kbd "w") 'sr-speedbar-select-window)

  ;; Display ^L as a horizontal line
  (use-package page-break-lines
    :delight
    :config (global-page-break-lines-mode))
  
  ;; (use-package nispio/dev-utils
  ;;   :ensure nil
  ;;   :bind-keymap ("H-g" . nispio/gdb-window-map)
  ;;   :bind (:map my-map
  ;;          ("H-j" . nispio/semantic-ia-fast-jump)))

  ;(require 'nispio/xmidas)

  ) ;; end with-demoted-errors



;; Load packages that are only compatible with Emacs 24.3+
(when (or (> emacs-major-version 24)
          (and (= emacs-major-version 24) (>= emacs-minor-version 3)))
  (with-demoted-errors "INIT ERROR: %s"
    (electric-pair-mode 1) ; Enable automatic bracket closing

    ;; Use Helm for incremental completion and selection narrowing
    ;; (source: https://github.com/emacs-helm/helm)
    (use-package helm
      ;; :bind-keymap ("C-8" . helm-command-map)
      :bind (:map my-map
             ("C-h a" . helm-apropos) ;; Replaces apropos-command
             ("C-h f" . helm-apropos) ;; Replaces describe-function
             ("C-h v" . helm-apropos) ;; Replaces describe-variable
             ("C-h p" . helm-list-elisp-packages)
             ("M-s o" . helm-occur) ;; Replaces occur
             ("M-s r" . helm-register)
             ("C-h A" . nispio/helm-customize-group)
             ("M-s b" . nispio/helm-moccur-buffers))
      :config
      (require 'helm-config)
      (require 'nispio/helm-config)
      (require 'nispio/helm-extra)
      (nispio/setup-helm-occur-from-isearch)
      (nispio/setup-helm-apropos)
      (define-key helm-command-map (kbd "C-SPC") 'helm-resume)
      (define-key helm-map (kbd "M-1") 'nispio/helm-full-frame)
      (message "use-package loaded %s" "helm"))
    

    ;; Helm interface for describe bindings
    ;; (source: https://github.com/emacs-helm/helm-descbinds)
    (use-package helm-descbinds
      :after helm
      :bind ("<XF86Favorites>" . helm-descbinds)
      :config
      (helm-descbinds-mode 1)
      (define-key my-map (kbd "C-h b") nil)
      (message "use-package loaded %s" "helm-descbinds"))

    ;; Helm-Swoop is a more flexible flavor of helm-occur
    (use-package helm-swoop
      :after helm
      :bind (:map my-map
             ("M-s i" . helm-swoop)
             ("M-s I" . helm-multi-swoop)
             ("M-s B" . helm-multi-swoop-all))
      :config (message "use-package loaded %s" "helm-swoop"))

    (use-package helm-ag
      :after helm
      :commands (helm-do-ag helm-do-ag-project-root)
      :bind (:map my-map
             ("M-s A" . helm-do-ag)
             ("M-s a" . helm-do-ag-project-root))
      :init
      (autoload 'helm-silver "nispio/helm-silver")
      (autoload 'helm-silver-project-root "nispio/helm-silver")
      :config
      (require 'nispio/helm-ag-extra)
      (require 'nispio/helm-silver)
      ;; Smarter way to find project root
      (nispio/setup-helm-ag-project-root)
      ;; Ability to freeze a helm-do-ag query and continue narrowing
      (nispio/setup-helm-ag-narrow)
      (message "use-package loaded %s" "helm-ag"))

    ;; Use a more powerful alternative to ido-mode's flex matching.
    ;; SOURCE: https://github.com/lewang/flx.git
    (use-package flx-ido
      :config (flx-ido-mode 1))

    ;; Manage and navigate projects easily in Emacs
    ;; SOURCE: https://github.com/bbatsov/projectile.git
    (use-package projectile
      :delight
      :config
      (require 'nispio/projectile-extra)
      (projectile-global-mode)
      (setq projectile-enable-caching t)
      (message "use-package loaded %s" "projectile"))

    ;; Use helm for projectile
    (use-package helm-projectile
      :after (helm projectile)
      :after helm-config
      :bind-keymap ("C-7" . projectile-command-map)
      :bind (:map projectile-command-map
             ("\\" . projectile-find-other-file))
      :config
      (helm-projectile-on)
      (message "use-package loaded %s" "helm-projectile"))

    (use-package helm-gtags
      :after helm
      :hook (c-mode-common . helm-gtags-mode)
      :bind (:map my-map
             ("M-." . helm-gtags-dwim)
             ("M-," . helm-gtags-pop-stack)
             ("C-M-." . helm-gtags-select))
      :config (message "use-package loaded %s" "helm-gtags"))

    ;; Emacs frontend to GNU Global source code tagging system.
    ;; SOURCE: https://github.com/leoliu/ggtags
    ;(use-package ggtags :ensure t)

    (use-package yasnippet :config (yas-global-mode 1))
    
    ;; Set up auto-complete
    ;; (source: https://github.com/auto-complete/auto-complete)
    (use-package auto-complete :ensure t :diminish "")
    (use-package ac-c-headers :ensure t)
    (require 'auto-complete-config)
    (global-auto-complete-mode t)
    (setq ac-auto-start 3)              ; start after 3 characters were typed
    (setq ac-auto-show-menu 0.8)        ; Wait 0.8 seconds to show menu

    (setq-default ac-sources '(ac-source-abbrev
                               ac-source-dictionary
                               ac-source-words-in-same-mode-buffers))

    (defun ac-common-setup ()
      (add-to-list 'ac-sources 'ac-source-filename))
    (add-hook 'auto-complete-mode-hook 'ac-common-setup)

    (defun ac-cc-mode-setup ()
      (setq ac-sources
            (append '(ac-source-c-headers
                      ac-source-semantic
                      ac-source-yasnippet
                      ac-source-gtags)
                    ac-sources)))
    (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)

    (defun ac-emacs-lisp-mode-setup ()
      (setq ac-sources (append '(ac-source-features
                                 ac-source-functions
                                 ac-source-yasnippet
                                 ac-source-variables
                                 ac-source-symbols)
                               ac-sources)))
    (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)

    (add-to-list 'ac-modes 'matlab-mode)  ; Allow auto-complete with matlab-mode

    (use-package ac-helm :ensure t)
    (define-key my-map (kbd "C-.") 'ac-complete-with-helm)
    (define-key ac-complete-mode-map (kbd "C-.") 'ac-complete-with-helm)

    ;; Jump to anywhere in the buffer with a few keystrokes
    (use-package avy :ensure t)
    (define-key my-map (kbd "C-;") 'avy-goto-word-or-subword-1)

    ;; Zap up to any character with a few keystrokes
    (use-package avy-zap :ensure t)
    (define-key my-map (kbd "M-z") 'avy-zap-up-to-char)

    ;; Zap up to any character with a few keystrokes
    (use-package ace-link :ensure t)
    (ace-link-setup-default)

    ;; Undo history is preserved as a tree, rather than linearly
    (use-package undo-tree :ensure t :diminish "")
    (global-undo-tree-mode)

    ;; Use ido completion with extended commands (M-x)
    (use-package smex :ensure t)
    (define-key my-map (kbd "M-x") 'smex)

    ;; Smarter highlighting of shell variables within quotes
    (use-package sh-extra-font-lock :ensure t)
    (add-hook 'sh-mode-hook 'sh-extra-font-lock-activate)

    ;; ;; Multiple frames as tabs
    ;; (use-package elscreen :ensure t)
    ;; (customize-set-value 'elscreen-display-screen-number nil)
    ;; (customize-set-value 'elscreen-display-tab nil)
    ;; (customize-set-value 'elscreen-tab-display-control nil)
    ;; (customize-set-value 'elscreen-tab-display-kill-screen nil)
    ;; (elscreen-start)
    ;; (define-key my-map (kbd "C-z z") 'suspend-frame)
    ;; (define-key my-map (kbd "C-z f") 'elscreen-toggle-display-tab)
    ;; (define-key my-map (kbd "C-z F") 'elscreen-toggle-display-tab)
    ;; (define-key my-map (kbd "C-z C-z") 'elscreen-toggle)

    ;; Commands for working with regexps with visual feedback
    (use-package visual-regexp :ensure t)
    (use-package visual-regexp-steroids :ensure t)
    (define-key my-map (kbd "M-5") 'vr/replace)
    (define-key my-map (kbd "M-%") 'vr/query-replace)

    ;; View large files one piece at a time
    (use-package vlf-setup :ensure vlf)

    ;; Automatic unobtrusive whitespace cleanup on save
    (use-package ws-butler :ensure t :diminish "")
    (ws-butler-global-mode)

    ;; Use ido for completing-read
    (use-package ido-completing-read+ :ensure t)
    (ido-ubiquitous-mode)

    (use-package which-key :ensure t :diminish "")
    (which-key-mode 1)

    (use-package zygospore :ensure t)
    (define-key my-map (kbd "M-1") 'zygospore-toggle-delete-other-windows)

    ) ;; end with-demote-errors
  ) ;; end emacs 24.3+ customizations



;; Set up org mode.  This is postponed to make sure that we are working with the
;; latest version of org instead of the version included with emacs
(require 'nispio/org-config)
(define-key my-map (kbd "C-c a") 'org-agenda)
(define-key my-map (kbd "C-c l") 'org-store-link)
(define-key my-map (kbd "C-'") 'org-capture)
(define-key my-map (kbd "C-c C-x C-i") 'org-clock-in)
(define-key my-map (kbd "C-c C-x <C-i>") 'org-clock-in)
(define-key my-map (kbd "C-c C-x C-o") 'org-clock-out)
(define-key my-map (kbd "C-c C-x C-x") 'org-clock-in-last)
(define-key my-map (kbd "C-c C-x C-e") 'org-clock-modify-effort-estimate)
(define-key my-map (kbd "C-c C-x <C-m>") 'org-clock-menu)

;; ;; isearch automatically wraps upon failure
;; ;; (source: http://stackoverflow.com/q/285660/1590790)
;; (defadvice isearch-search (after isearch-no-fail activate)
;;   (unless isearch-success
;;     (ad-disable-advice 'isearch-search 'after 'isearch-no-fail)
;;     (ad-activate 'isearch-search)
;;     (isearch-repeat (if isearch-forward 'forward))
;;     (ad-enable-advice 'isearch-search 'after 'isearch-no-fail)
;;     (ad-activate 'isearch-search)))

;; Set the default faces for highlighting with hi-lock
(setq hi-lock-face-defaults (list "hi-yellow" "hi-pink" "hi-green" "hi-blue"))

;; Function to turn a delimiter-separated value file into an org table
(autoload 'nispio/edit-dsv-as-orgtbl "nispio/org-table-utils")
(global-set-key (kbd "C-c |") 'nispio/edit-dsv-as-orgtbl)

;; Add some personal org-mode tweaks centered around editing org tables
(autoload 'nispio/org-mode-setup "nispio/org-table-utils")
(add-hook 'org-mode-hook 'nispio/org-mode-setup)

;; Load custom function clocktable-by-tag when needed
(autoload 'org-dblock-write:clocktable-by-tag "nispio/org-clocktable-extra")

;; Make hide-show mode available, turn it on it a buffer with C-c @
(defun nispio/hs-toggle-hiding ()
  (interactive)
  (if (bound-and-true-p hs-minor-mode)
      (call-interactively #'hs-toggle-hiding)
    (if (hs-minor-mode) (message "%s" "Hs minor mode enabled in current buffer"))))

(autoload 'hs-minor-mode "hideshow")
(global-set-key (kbd "C-c @") 'hs-minor-mode)
(defvar nispio/hs-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-2") 'nispio/hs-toggle-hiding)
    (define-key map (kbd "C-c") 'hs-toggle-hiding)
    (define-key map (kbd "C-h") 'hs-hide-block)
    (define-key map (kbd "C-l") 'hs-hide-level)
    (define-key map (kbd "C-s") 'hs-show-block)
    (define-key map (kbd "C-M-h") 'hs-hide-all)
    (define-key map (kbd "C-M-s") 'hs-show-all)
    map))
(define-key my-map (kbd "C-2") nispio/hs-mode-map)

;; Add [] to the list of collapsible entries in js mode (for JSON files)
(eval-after-load "hideshow"
  (progn
    (add-to-list 'hs-special-modes-alist
                 '(js-mode "\\({\\|\\[\\)" "\\(}\\|\\]\\)" "/[*/]" nil))
    nil))

;; Use hideshow mode in javascript mode by default
(add-hook 'js-mode-hook 'hs-minor-mode)

;; Key bindings for calc
(require 'calc)
(define-key calc-mode-map (kbd "<backtab>") 'calc-roll-up)
(define-key my-map (kbd "H-*") 'nispio/calc-grab-number)

;; Key bindings for re-builder
(require 're-builder)
(define-key reb-mode-map (kbd "C-c %") 'nispio/reb-query-replace)

;; Do not attempt to open .doc files as Word Documents
(add-to-list 'auto-mode-alist '("\\.doc\\'" . text-mode))

;; Workaround for an issue with isearch
(define-key isearch-mode-map (kbd "<return>") 'isearch-exit)

;; Enable disabled commands
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)

(setq no-server-start-p (member "--no-server" command-line-args))
(setq command-line-args (delete "--no-server" command-line-args))

(when (display-graphic-p)
  (unless no-server-start-p
    ;; Start (or restart) the server
    (require 'server)
    (server-force-delete)
    (server-start)))



(setq no-custom-file-p (member "--no-custom-file" command-line-args))
(setq command-line-args (delete "--no-custom-file" command-line-args))
(unless no-custom-file-p (load custom-file))

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
