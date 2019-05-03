;; --------------------------------------------
;; ----gutted init.el file forked from nispio------
;; --------------------------------------------
;; My "must-have" key bindings get set before anything can go wrong.
(global-set-key (kbd "<C-tab>") 'next-multiframe-window)
(global-set-key (kbd "<C-iso-lefttab>") 'previous-multiframe-window)
(global-set-key (kbd "M-1") 'delete-other-windows)

;; Basic editor configuration
(setq-default truncate-lines t)        ; Truncate lines by default
(setq inhibit-startup-screen t)        ; Disable splash screen
(setq visible-bell t)                  ; Disable system beep
(setq transient-mark-mode t)           ; Enable visual feedback on selections
(setq x-stretch-cursor t)              ; Cursor as wide as the glyph under it
(setq scroll-step 1)                   ; Only scroll by one line at top/bottom ;; das es good!
(setq require-final-newline t)         ; Always end a file with a newline
(setq frame-title-format "emacs - %b") ; Set frame title to "emacs - <buffer name>"
(setq-default fill-column 80)          ; Set the default fill-column to 80
(setq enable-recursive-minibuffers t)  ; Allow recursive minibuffers
(minibuffer-depth-indicate-mode 1)     ; Show the current minibuffer depth if depth > 1

;; --------------------------------------------
;; ----my basic setup from scratch------------
;; --------------------------------------------
;(set-default 'cursor-type 'hbar) ; change cursor to simply underscore bar
(menu-bar-mode -1) ; disable the menu bar
(tool-bar-mode -1) ; disable the tool bar
(scroll-bar-mode -1) ; disable the scroll bar
(column-number-mode) ; add the column number to info bar
(show-paren-mode) ; highlight parentheses pairs
(electric-pair-mode) ; auto-add closing parentheses when adding opening parentheses
(global-hl-line-mode t) ; highlight entire line cursor is currently on
(ido-mode t) ; smart completion (especially useful when opening files)
(winner-mode t) ; revert back to previous window layouts using 'C-c left' and 'C-c right'
(windmove-default-keybindings) ; intuitively move between windows using Shift + arrow keys
(load-theme 'manoj-dark t) ; dark theme, need to find a different sometime
