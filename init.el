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
