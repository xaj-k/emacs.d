(tooltip-mode 1)			; Enable tooltips

;; Fromat the appearance of the mode line
(setq-default mode-line-format
 '("%e"
   mode-line-front-space
   mode-line-mule-info
   mode-line-client
   mode-line-modified
   mode-line-remote
   mode-line-frame-identification
   mode-line-buffer-identification
   " %3l :%3c  "
   mode-line-modes
   " "
   (vc-mode vc-mode)
   (global-mode-string global-mode-string)
   mode-line-end-spaces))

(load-theme 'nispio-dark t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(aw-dispatch-always t)
 '(aw-keys (quote (106 102 104 103 107 100 108 115 97 59)))
 '(browse-url-generic-program "firefox")
 '(custom-safe-themes
   (quote
    ("efe9aa1a078bf00a43342a1fc8b16505d444f8068285f5f83c6275cadcc44b7d" default)))
 '(dired-listing-switches "-Alh --group-directories-first")
 '(dired-omit-files "^\\.\\|^\\.?#\\|.*~")
 '(diredp-hide-details-initially-flag nil t)
 '(eshell-scroll-to-bottom-on-output (quote all))
 '(fci-rule-color "#14151E" t)
 '(focus-follows-mouse t)
 '(helm-ag-base-command "ag --nocolor --nogroup")
 '(helm-ag-fuzzy-match t)
 '(helm-el-package-initial-filter (quote all))
 '(indent-tabs-mode nil)
 '(mc/cycle-looping-behaviour (quote continue))
 '(mc/edit-lines-empty-lines (quote ignore))
 '(mc/mode-line
   (quote
    (" mc:" :eval
     (format
      #("%d" 0 2
        (face mc/cursor-face))
      (mc/num-cursors)))))
 '(mouse-autoselect-window t)
 '(org-agenda-clockreport-parameter-plist (quote (:link t :maxlevel 2 :fileskip0 t)))
 '(org-agenda-dim-blocked-tasks t)
 '(org-agenda-files "~/org/agenda.ini")
 '(org-agenda-ndays 10 t)
 '(org-agenda-restore-windows-after-quit t t)
 '(org-agenda-show-future-repeats (quote next))
 '(org-agenda-skip-deadline-if-done nil t)
 '(org-agenda-skip-scheduled-if-done nil t)
 '(org-agenda-span 10)
 '(org-agenda-start-on-weekday nil t)
 '(org-agenda-time-grid
   (quote
    ((daily today require-timed)
     (800 1000 1200 1400 1600 1800)
     "......" "----------------")))
 '(org-agenda-todo-ignore-scheduled t t)
 '(org-agenda-todo-ignore-with-date t)
 '(org-blank-before-new-entry (quote ((heading . t) (plain-list-item))))
 '(org-capture-templates
   (quote
    (("t" "Todo" entry
      (file+headline "tasks.org" "Unfiled Tasks")
      "* TODO %i%?
  - State \"TODO\"       from \"\"           %U")
     ("T" "Todo (from file)" entry
      (file+headline "tasks.org" "Unfiled Tasks")
      (function nispio/linked-todo)
      :immediate-finish t)
     ("d" "Distraction" entry
      (file+headline "distractions.org" "Distractions")
      "* %?
  - Added: %U")
     ("D" "Distraction (as TODO)" entry
      (file+headline "distractions.org" "Tasks")
      "* TODO %?
  - Added: %U")
     ("n" "Notes" entry
      (file+headline "notes.org" "Notes")
      "* %i%?
  - Added: %U")
     ("N" "Notes (from file)" entry
      (file+headline "notes.org" "Notes")
      (function nispio/linked-note))
     ("p" "Pomodoros" entry
      (file+datetree "pomodoros.org")
      "* TODO %i%?
  - State \"TODO\"       from \"\"           %U" :jump-to-captured t)
     ("j" "Journal" entry
      (file+datetree "journal.org")
      "* %?")
     ("J" "Journal (free writing)" entry
      (file+datetree "freejourn.org")
      "* %?"))) t)
 '(org-completion-use-ido t t)
 '(org-directory "~/org")
 '(org-duration-format (quote (("h") (special . 2))))
 '(org-enforce-todo-checkbox-dependencies t)
 '(org-enforce-todo-dependencies t)
 '(org-export-with-LaTeX-fragments t t)
 '(org-fast-tag-selection-single-key (quote expert))
 '(org-goto-auto-isearch nil)
 '(org-hide-leading-stars t)
 '(org-link-search-must-match-exact-headline nil)
 '(org-log-repeat nil)
 '(org-outline-path-complete-in-steps nil)
 '(org-refile-targets (quote ((org-agenda-files :maxlevel . 2))))
 '(org-return-follows-link t)
 '(org-special-ctrl-a/e t)
 '(org-startup-folded t)
 '(org-tag-alist
   (quote
    (("CODE" . 99)
     ("PLAN" . 112)
     ("EXPERIMENT" . 101)
     ("LEARN" . 108)
     ("DOCUMENT" . 100)
     ("TEACH" . 116))))
 '(org-tags-column -77)
 '(org-todo-keywords
   (quote
    ((sequence "TODO(t!)" "STARTED(s!)" "WAIT(w@/!)" "ASK(a)" "|" "ANSWERED(A@)" "CANCELLED(x@)" "DONE(d)" "COMPLETE(c!)"))))
 '(package-selected-packages
   (quote
    (yasnippet ein-mumamo ein delight diminish treemacs-icons-dired treemacs-magit swiper auto-yasnippet function-args popwin fill-column-indicator volatile-highlights clang-format ob-async ob-ipython ob-mongo org-clock-convenience org-clock-csv org-clock-split all-the-icons all-the-icons-dired anzu embrace expand-region highlight-indentation neotree powerline rainbow-delimiters cask cask-mode evil evil-leader magit-popup markdown-mode markdown-mode+ smartparens ac-clang ac-etags ac-html ac-html-angular ac-html-bootstrap ac-html-csswatcher ac-ispell ac-js2 ac-math ac-octave ac-php ac-php-core transient lv magit company-flx company-jedi company-math company-c-headers company-bibtex company-auctex company-anaconda company-irony company-irony-c-headers google-c-style flycheck-pos-tip flycheck-irony flycheck-clangcheck flycheck-clang-tidy flycheck-clang-analyzer flymake-google-cpplint flymake-cppcheck flycheck-pycheckers flycheck ox-pandoc ox-rst treemacs-projectile ido-completing-read-plus ido-completing-read+ treemacs ace-jump-buffer ace-mc ace-window gnugo zygospore ws-butler which-key vlf visual-regexp-steroids use-package undo-tree smex sh-extra-font-lock phi-search-mc phi-rectangle page-break-lines org-plus-contrib move-text matlab-mode lorem-ipsum ido-ubiquitous helm-swoop helm-projectile helm-gtags helm-descbinds helm-ag ggtags flx-ido elscreen dired+ column-marker buffer-move avy-zap auctex ace-link ac-helm ac-c-headers)))
 '(phi-search-case-sensitive (quote guess) t)
 '(ps-bottom-margin 40)
 '(ps-font-size 9.5)
 '(ps-header-lines 1)
 '(ps-header-offset 20)
 '(ps-left-margin 40)
 '(ps-right-margin 20)
 '(ps-top-margin 20)
 '(safe-local-variable-values
   (quote
    ((TeX-master . "report")
     (TeX-master . t)
     (xmidas-macro-mode . t)
     (eval add-hook
           (quote after-save-hook)
           (lambda nil
             (setq kill-ring nil))))))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#d54e53")
     (40 . "goldenrod")
     (60 . "#e7c547")
     (80 . "DarkOliveGreen3")
     (100 . "#70c0b1")
     (120 . "DeepSkyBlue1")
     (140 . "#c397d8")
     (160 . "#d54e53")
     (180 . "goldenrod")
     (200 . "#e7c547")
     (220 . "DarkOliveGreen3")
     (240 . "#70c0b1")
     (260 . "DeepSkyBlue1")
     (280 . "#c397d8")
     (300 . "#d54e53")
     (320 . "goldenrod")
     (340 . "#e7c547")
     (360 . "DarkOliveGreen3"))))
 '(vc-annotate-very-old-color nil)
 '(vr/engine (quote python)))
(put 'dired-find-alternate-file 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(elscreen-tab-background-face ((t (:background "Gray20"))))
 '(elscreen-tab-current-screen-face ((t (:background "gray50" :foreground "white"))))
 '(elscreen-tab-other-screen-face ((t (:background "Gray30" :foreground "Gray50"))))
 '(hl-line ((t (:background "dim gray" :foreground "Old Lace")))))
