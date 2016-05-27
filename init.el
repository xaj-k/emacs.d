;;;; .emacs

;; Configure UI features
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode 0))  ; Disable scroll bars
(when (fboundp 'fringe-mode) (fringe-mode '(nil . 0))) ; Left fringes only
(when (fboundp 'tool-bar-mode) (tool-bar-mode 0))      ; Disable toolbar

;; White text on black background
(set-background-color "black")
(set-foreground-color "white smoke")

(message "Welcome to Emacs!\nThis session brought to you by:\n%s"
         (mapconcat 'identity command-line-args " "))

(message "Loading init file...")

;; My "must-have" key bindings get set before anything can go wrong.
(global-set-key (kbd "<C-tab>") 'next-multiframe-window)
(global-set-key (kbd "<C-iso-lefttab>") 'previous-multiframe-window)
(global-set-key (kbd "M-1") 'delete-other-windows)

;; Set load paths for lisp files
(setq site-lisp (concat user-emacs-directory "site-lisp/"))
(add-to-list 'load-path site-lisp)

;; Settings modified via the Customize interface get their own file.  We set
;; this right up front in case any of the other init functions use the customize
;; interface.
(if (display-graphic-p)
    (setq custom-file (concat user-emacs-directory "settings.el"))
  (setq custom-file (concat user-emacs-directory "settings-tty.el")))

(require 'nispio/misc-utils)
(setq load-path
      (append
       ;; Add all folders from site-lisp (except explicitly rejected dirs).
       ;; The "nispio" dir is not included because its packages are provided as
       ;; (provide 'nispio/package-name)
       (nispio/directory-subdirs site-lisp '(".hg" ".git" "nispio"))
       ;; Put the current load-path at the end
       load-path))

;; Make custom themes available
(customize-set-value 'custom-theme-directory (concat site-lisp "themes/"))

;; Load my own minor mode for personal keybindings
(require 'nispio/my-mode)
(enable-my-global-mode)
(global-set-key (kbd "<pause>") 'enable-my-global-mode)
(global-set-key (kbd "<C-m>") (kmacro meta-return "<M-return>"))

(require 'nispio/key-utils)
(nispio/unbind-digit-arguments)
(define-key my-map (kbd "C-h C-k") 'nispio/insert-key-description)
(define-key my-map (kbd "C-h k") 'nispio/locate-key-binding)
(global-set-key (kbd "C-h C-M-k") 'nispio/unbind-local-key)
(global-set-key (kbd "C-h k") 'nispio/locate-key-binding)

;; Add greek characters to "C-x 8" map
(require 'nispio/unicode)

;; Add convenience function for printing a buffer to PS/PDF
(require 'nispio/print)
(define-key my-map (kbd "<S-print>") 'nispio/ps-print-buffer)

;; This is a hack because my M-s keybinding disappear in some modes
(define-key my-map (kbd "M-s") (lookup-key global-map (kbd "M-s")))

(require 'nispio/rect-utils)
(define-key my-map (kbd "C-x r Y") 'nispio/yank-rectangle-from-kill-ring)
(define-key my-map (kbd "C-x r D") 'delete-whitespace-rectangle)

(define-key my-map (kbd "C-c c") 'comment-region)
(define-key my-map (kbd "C-c u") 'uncomment-region)
(define-key my-map (kbd "<f5>") 'nispio/revert-buffer)
(define-key my-map (kbd "C-x <f5>") 'nispio/remove-properties)
(define-key my-map (kbd "C-x <f6>") 'add-file-local-variable)

(define-key my-map (kbd "C-H-\\") 'nispio/switch-to-scratch-and-back)
(define-key my-map (kbd "M-s N") 'nispio/dired-find-exts)
(define-key my-map (kbd "M-s O") 'multi-occur-in-matching-buffers)
(define-key my-map (kbd "M-s .") 'isearch-forward-symbol-at-point)
(define-key my-map (kbd "C-x DEL") 'nispio/strip-1)
(define-key my-map (kbd "<S-prior>") 'nispio/scroll-down-lines)
(define-key my-map (kbd "<S-next>") 'nispio/scroll-up-lines)
(define-key my-map (kbd "C-x K") 'nispio/delete-this-file)

(define-key my-map (kbd "C-c C-n") 'nispio/end-of-column)
(define-key my-map (kbd "C-c C-p") 'nispio/beginning-of-column)
(define-key my-map (kbd "M-s s") 'nispio/sort-this-column)

;; Basic editor configuration
(setq-default truncate-lines t)        ; Truncate lines by default
(setq inhibit-startup-screen t)        ; Disable splash screen
(setq visible-bell t)                  ; Disable system beep
(setq transient-mark-mode t)           ; Enable visual feedback on selections
(setq x-stretch-cursor t)              ; Cursor as wide as the glyph under it
(setq scroll-step 1)                   ; Only scroll by one line at top/bottom
(setq require-final-newline t)         ; Always end a file with a newline
(setq frame-title-format "emacs - %b") ; Set frame title to "emacs - <buffer name>"
(setq-default fill-column 80)          ; Set the default fill-column to 80
(setq enable-recursive-minibuffers t)  ; Allow recursive minibuffers
(minibuffer-depth-indicate-mode 1)     ; Show the current minibuffer depth if depth > 1

;; Set tab width to 4 and put tab stops every 4 characters
(setq-default tab-width 4)
(setq tab-stop-list (number-sequence 4 100 4))

(defun elisp-set-tab-width ()
  (setq tab-width 8))
(add-hook 'emacs-lisp-mode-hook 'elisp-set-tab-width)

;; Workaround for an issue with isearch
(define-key isearch-mode-map (kbd "<return>") 'isearch-exit)



;; Load init files as appropriate, turning errors into messages
(with-demoted-errors "INIT ERROR: %s"
  ;; Simple minor modes
  (show-paren-mode 1)                ; Show matching parenthesis
  (global-font-lock-mode 1)          ; Enable syntax highlighting
  (column-number-mode t)             ; Show column number on mode line
  (ido-mode 1)

  (setq package-archives '())
  (add-to-list 'package-archives `("local-misc" . ,(concat user-emacs-directory "local-elpa-misc/")))
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
  ;; (setq package-archives `(("local-elpa" . ,(concat user-emacs-directory "local-elpa/"))))
  (require 'nispio/package-config)

  ;; Display line numbers in all programming buffers
  (use-package linum :ensure t)
  ;(global-linum-mode -1)
  (add-hook 'prog-mode-hook 'linum-mode)
  (setq linum-format "%3d")

  (unless (fboundp 'forward-symbol)
    (require 'thingatpt))

  (defun backward-symbol () (interactive) (forward-symbol -1))

  ;; (defun my-prog-mode-keys ()
  ;;   (local-set-key (kbd "M-f") 'forward-symbol)
  ;;   (local-set-key (kbd "M-b") 'backward-symbol)
  ;;   (local-set-key (kbd "M-F") 'forward-word)
  ;;   (local-set-key (kbd "M-B") 'backward-word))
  ;; (add-hook 'prog-mode-hook 'my-prog-mode-keys)

  ;; Install updated org-mode from ELPA
  (use-package org-plus-contrib :ensure t)

  ;; Use unix line endings by default
  (setq default-buffer-file-coding-system 'utf-8-unix)

  (require 'dired-x)
  ;; Start search in dired buffer with "/"
  (define-key dired-mode-map (kbd "/") 'dired-isearch-filenames)
  (define-key dired-mode-map (kbd "F") 'nispio/find-marked-files)
  (define-key dired-mode-map (kbd "W") 'nispio/dired-copy-filename)

  ;; ;; Extend dired functionality
  (use-package dired+ :ensure t)

  (nispio/after 'dired+
    ;; When opening a directory in dired, reuse the current buffer
    (diredp-toggle-find-file-reuse-dir 1)
    (define-key dired-mode-map [mouse-2] 'diredp-mouse-find-file)
    (customize-set-variable 'diredp-hide-details-initially-flag nil))

  ;; Make ibuffer auto-update after changes
  ;; (source: http://emacs.stackexchange.com/a/2179/93)
  (defun nispio/ibuffer-stale-p (&optional noconfirm)
    (frame-or-buffer-changed-p 'ibuffer-auto-buffers-changed))
  (defun nispio/ibuffer-auto-revert-setup ()
    (set (make-local-variable 'buffer-stale-function)
         'nispio/ibuffer-stale-p)
    (setq-local auto-revert-verbose nil)
    (auto-revert-mode 1))
  (add-hook 'ibuffer-mode-hook 'nispio/ibuffer-auto-revert-setup)

  ;; Move current line up and down with M-up and M-down
  (use-package move-text :ensure t)
  (define-key my-map (kbd "<M-up>") 'move-text-up)
  (define-key my-map (kbd "<M-down>") 'move-text-down)

  ;; Easily re-arrange buffers within the frame
  ;; (source: http://www.emacswiki.org/emacs/download/buffer-move.el)
  (use-package buffer-move :ensure t)
  (define-key my-map (kbd "C-c <C-up>") 'buf-move-up)
  (define-key my-map (kbd "C-c <C-down>") 'buf-move-down)
  (define-key my-map (kbd "C-c <C-left>") 'buf-move-left)
  (define-key my-map (kbd "C-c <C-right>") 'buf-move-right)

  ;; Keybindings to change the window size
  (define-key my-map (kbd "C-S-<left>") 'shrink-window-horizontally)
  (define-key my-map (kbd "C-S-<right>") 'enlarge-window-horizontally)
  (define-key my-map (kbd "C-S-<up>") 'shrink-window)
  (define-key my-map (kbd "C-S-<down>") 'enlarge-window)
  (define-key my-map (kbd "M-o 6 d") 'shrink-window-horizontally)
  (define-key my-map (kbd "M-o 6 c") 'enlarge-window-horizontally)
  (define-key my-map (kbd "M-o 6 a") 'shrink-window)
  (define-key my-map (kbd "M-o 6 b") 'enlarge-window)

  ;; Make sure that the cygwin bash executable can be found (Windows Emacs)
  (when (eq system-type 'windows-nt)
    (setq explicit-shell-file-name "C:/cygwin/bin/bash.exe")
    (setq shell-file-name explicit-shell-file-name)
    (add-to-list 'exec-path "C:/cygwin/bin"))

  ;; Add an easy way to produce dummy text
  ;; (source: http://www.emacswiki.org/emacs/download/lorem-ipsum.el)
  (use-package lorem-ipsum :ensure t)
  (define-key my-map (kbd "C-c C-l")  'Lorem-ipsum-insert-paragraphs)

  ;; Add support for isearch functionality with multiple cursors
  ;; (source: https://github.com/zk-phi/phi-search)
  (use-package phi-search :ensure t)
  (customize-set-value 'phi-search-case-sensitive 'guess)
  (define-key my-map (kbd "C-s") 'phi-search)
  (define-key my-map (kbd "C-r") 'phi-search-backward)

  ;; Complete phi-search with the match selected
  (defun phi-search-complete-with-selection ()
  (interactive)
  (let ((query (buffer-string)))
    (phi-search-complete)
    (mc/execute-command-for-all-cursors
     (lambda ()
       (interactive)
       (when (looking-back query)
         (push-mark (match-beginning 0) t t)
         (goto-char (match-end 0))
         (activate-mark))))))
  (define-key phi-search-default-map (kbd "<S-return>") 'phi-search-complete-with-selection)

  ;; Add support for editing with multiple cursors
  ;; (source: https://github.com/magnars/multiple-cursors.el)
  (use-package multiple-cursors :ensure t)

  ;; Modify the behavoir of mc/mark-lines to set a temporary goal column and
  ;; allow skipping of lines during mc/mark-more-like-this-extended.
  (require 'nispio/mc-extra)
  (nispio/mc-setup-mark-lines)

  (nispio/after 'multiple-cursors
    (define-key mc/keymap (kbd "C-c C-v") 'mc/vertical-align-with-space)
    (define-key mc/keymap (kbd "M-x") 'execute-extended-command)
    (define-key mc/keymap (kbd "C-!") 'nispio/mc-insert-numbers-1)
    (define-key mc/keymap (kbd "C-1") 'mc/insert-numbers))

  (define-key my-map (kbd "C->") 'mc/mark-next-like-this)
  (define-key my-map (kbd "C-<") 'mc/mark-previous-like-this)
  (define-key my-map (kbd "C-c C-<") 'mc/mark-all-like-this)
  (define-key my-map (kbd "C-c C->") 'mc/mark-more-like-this-extended)
  (define-key my-map (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (define-key my-map (kbd "C-S-c C-<") 'mc/mark-all-in-region)
  (define-key my-map (kbd "H-C-SPC") 'nispio/fake-cursor-at-point)
  (define-key my-map (kbd "<H-return>") 'multiple-cursors-mode)
  (define-key my-map (kbd "<H-C-return>") 'nispio/mc-many-cursors)
  (define-key my-map (kbd "<C-down-mouse-1>") 'mc/toggle-cursor-on-click)
  (define-key my-map (kbd "<C-mouse-1>") 'ignore)
  (define-key my-map (kbd "M-s M-s") 'mc--mark-symbol-at-point)
  (define-key my-map (kbd "C-c S-C-n") 'nispio/mark-this-column)

  ;; TODO: find a better way to set these bindings in special cases
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
  (global-set-key (kbd "C-c C->") 'mc/mark-more-like-this-extended)
  (global-set-key (kbd "H-C-SPC") 'nispio/fake-cursor-at-point)
  (global-set-key (kbd "<H-return>") 'multiple-cursors-mode)
  
  ;; Add extended interoperability between phi-search and multiple cursors
  ;; (source: https://github.com/knu/phi-search-mc.el)
  (use-package phi-search-mc :ensure t)
  (phi-search-mc/setup-keys)
  (phi-search-from-isearch-mc/setup-keys)

  ;; Use phi-rectangle for rectangular selections
  (use-package phi-rectangle :ensure t)
  (define-key my-map (kbd "C-c C-SPC") 'phi-rectangle-set-mark-command)
  (define-key my-map (kbd "C-w") 'phi-rectangle-kill-region)
  (define-key my-map (kbd "M-w") 'phi-rectangle-kill-ring-save)
  (define-key my-map (kbd "C-y") 'phi-rectangle-yank)

  ;; Add support for editing matlab files
  ;; (source: http://matlab-emacs.cvs.sourceforge.net/viewvc/matlab-emacs/matlab-emacs/?view=tar)
  (use-package "matlab-load" :ensure matlab-mode)
  (require 'nispio/matlab-debug)
  (setq matlab-comment-column 50)
  (add-hook 'matlab-mode-hook 'linum-mode)
  ;; Use CEDET tools for matlab-mode
  (when (>= emacs-major-version 24)
    (matlab-cedet-setup))

  ;; Enable column markers at column 81 to warn of long lines
  ;; (source: http://www.emacswiki.org/emacs/download/column-marker.el)
  (use-package column-marker :ensure t)
  (defvar-local nispio/column-marker-column 91
    "The column that should be highlighted as a long-line warning.")
  (defun nispio/column-marker (&optional col)
    "Place column-marker-1 at the column specified by `nispio/column-marker-column'"
    (interactive)
    (column-marker-1 (or col nispio/column-marker-column)))
  (add-hook 'prog-mode-hook 'nispio/column-marker)

  (use-package tex-site :ensure auctex)
  (nispio/after 'tex-site
    (setq
     TeX-auto-save t
     TeX-parse-self t
     TeX-source-correlate-method (quote synctex)
     TeX-source-correlate-mode t
     TeX-source-correlate-start-server t
     reftex-plug-into-AUCTeX t)
    ;; (setq
    ;;  TeX-view-program-list (quote (("Sumatra PDF" "/usr/local/bin/sumatra -reuse-instance %o")))
    ;;  TeX-view-program-selection (quote ((output-pdf "Sumatra PDF"))))
    (setq-default TeX-master nil)
    (add-hook 'LaTeX-mode-hook 'visual-line-mode)
    (add-hook 'LaTeX-mode-hook 'flyspell-mode)
    (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
    (add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)
    (add-hook 'LaTeX-mode-hook 'turn-on-reftex))

  ;; ;; SrSpeedbar allows a speedbar that is "docked" in the current frame
  ;; (use-package sr-speedbar :ensure t)
  ;; (define-key my-map (kbd "C-c M-SPC") 'sr-speedbar-toggle)
  ;; ;(define-key nispio/gdb-window-map (kbd "w") 'sr-speedbar-select-window)

  ;; Display ^L as a horizontal line
  (use-package page-break-lines :ensure t :diminish "")
  (global-page-break-lines-mode)
  
  (require 'nispio/dev-utils)
  (define-key nispio/gdb-window-map (kbd "w") 'sr-speedbar-select-window)
  (define-key my-map (kbd "H-g") nispio/gdb-window-map)

  (require 'nispio/xmidas)

  ) ;; end with-demoted-errors



;; Load packages that are only compatible with Emacs 24.3+
(when (and (= emacs-major-version 24) (>= emacs-minor-version 3))
  (with-demoted-errors "INIT ERROR: %s"
    (electric-pair-mode 1) ; Enable automatic bracket closing

    ;; Use Helm for incremental completion and selection narrowing
    ;; (source: https://github.com/emacs-helm/helm)
    (use-package helm :ensure t)
    (require 'nispio/helm-config)
    (nispio/setup-helm-occur-from-isearch)

    (require 'nispio/helm-extra)
    (nispio/setup-helm-apropos)
    (define-key my-map (kbd "C-h A") 'nispio/helm-customize-group)

    ;; Helm interface for describe bindings
    ;; (source: https://github.com/emacs-helm/helm-descbinds)
    (use-package helm-descbinds :ensure t)
    (helm-descbinds-mode 1)
    (define-key my-map (kbd "C-h b") nil)
    (global-set-key (kbd "<XF86Favorites>") 'helm-descbinds)

    ;; Helm-Swoop is a more flexible flavor of helm-occur
    (use-package helm-swoop :ensure t)
    (define-key my-map (kbd "M-s i") 'helm-swoop)
    (define-key my-map (kbd "M-s I") 'helm-multi-swoop)
    (define-key my-map (kbd "M-s B") 'helm-multi-swoop-all)

    (use-package helm-ag :ensure t)
    (require 'nispio/helm-ag-extra)
    ;; Smarter way to find project root
    (nispio/setup-helm-ag-project-root)
    ;; Ability to freeze a helm-do-ag query and continue narrowing
    (nispio/setup-helm-ag-narrow)

    (require 'nispio/helm-silver)
    (define-key my-map (kbd "M-s A") 'helm-silver)
    (define-key my-map (kbd "M-s a") 'helm-silver-project-root)

    (define-key my-map (kbd "C-8") helm-command-map)
    (define-key helm-command-map (kbd "C-SPC") 'helm-resume)
    (define-key helm-map (kbd "M-1") 'nispio/helm-full-frame)
    (define-key my-map (kbd "M-s n") 'find-name-dired)
    (define-key my-map (kbd "C-h a") 'helm-apropos) ;; Replaces apropos-command
    (define-key my-map (kbd "C-h f") 'helm-apropos) ;; Replaces describe-function
    (define-key my-map (kbd "C-h p") 'helm-list-elisp-packages)
    (define-key my-map (kbd "M-s b") 'nispio/helm-moccur-buffers)
    (define-key my-map (kbd "M-s o") 'helm-occur) ;; Replaces occur
    (define-key my-map (kbd "M-s O") 'helm-multi-occur)
    (define-key my-map (kbd "M-s r") 'helm-register)

    ;; Use a more powerful alternative to ido-mode's flex matching.
    ;; SOURCE: https://github.com/lewang/flx.git
    (use-package flx-ido :ensure t)
    (flx-ido-mode 1)

    ;; Manage and navigate projects easily in Emacs
    ;; SOURCE: https://github.com/bbatsov/projectile.git
    (use-package projectile :ensure t :diminish "")
    (projectile-global-mode)
    (setq projectile-enable-caching t)

    (defvar project-root-regexps ()
      "List of regexps to match against when projectile is searching
for project root directories.")

    ;; Add the ability to use projects that are not
    (eval-after-load 'projectile
      (progn 
        ;; (source: https://github.com/bbatsov/projectile/issues/364#issuecomment-61296248)
        (defun projectile-root-child-of (dir &optional list)
          (projectile-locate-dominating-file
           dir
           (lambda (dir)
             (--first
              (if (and
                   (s-equals? (file-remote-p it) (file-remote-p dir))
                   (string-match-p (expand-file-name it) (expand-file-name dir)))
                  dir)
              (or list project-root-regexps (list))))))
        (nconc projectile-project-root-files-functions '(projectile-root-child-of))
        nil))

    ;; Use helm for projectile
    (use-package helm-projectile :ensure t)
    (eval-after-load "helm-config"
      (progn
        (require 'helm-projectile)
        (helm-projectile-on)
        (define-key my-map (kbd "C-7") projectile-command-map)
        (define-key projectile-command-map (kbd "\\") 'projectile-find-other-file)
        nil))

    (use-package helm-gtags :ensure t)
    (add-hook 'c-mode-common-hook 'helm-gtags-mode)
    (define-key my-map (kbd "M-.") 'helm-gtags-dwim)
    
    ;; Emacs frontend to GNU Global source code tagging system.
    ;; SOURCE: https://github.com/leoliu/ggtags
    (use-package ggtags :ensure t)

    ;; (use-package yasnippet :ensure t)
    ;; (yas-global-mode 1)

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

    ;; Multiple frames as tabs
    (use-package elscreen :ensure t)
    (customize-set-value 'elscreen-display-screen-number nil)
    (customize-set-value 'elscreen-display-tab nil)
    (customize-set-value 'elscreen-tab-display-control nil)
    (customize-set-value 'elscreen-tab-display-kill-screen nil)
    (elscreen-start)
    (define-key my-map (kbd "C-z z") 'suspend-frame)
    (define-key my-map (kbd "C-z f") 'elscreen-toggle-display-tab)
    (define-key my-map (kbd "C-z F") 'elscreen-toggle-display-tab)
    (define-key my-map (kbd "C-z C-z") 'elscreen-toggle)

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
    (use-package ido-ubiquitous :ensure t)
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

;; Make hide-show mode available, turn it on it a buffer with C-c @
(autoload 'hs-minor-mode "hideshow")
(global-set-key (kbd "C-c @") 'hs-minor-mode)
(defvar nispio/hs-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-2") 'hs-toggle-hiding)
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
