(defun nispio/buffer-file-name ()
  "Display the name of the file backing the current buffer"
  (interactive)
  (message (or buffer-file-name "no file"))
  buffer-file-name)

;; Pager mode for viewing piped data
(define-derived-mode pager-mode
  view-mode "Emacs Pager" "Major mode for paging"
  (setq buffer-read-only nil)
  (nispio/ansi-colorize)
  (set-buffer-modified-p nil)
  (setq buffer-read-only t))

;; Function to set font colors based on ansi color escape sequences
(defun nispio/ansi-colorize (&optional start end)
  "Replace in the region ansi-color specifications"
  (interactive "r")
  (save-excursion
	(if (use-region-p)
		(ansi-color-apply-on-region start end)
	  (ansi-color-apply-on-region (point-min) (point-max)))))

;; Automatically open emacspipe file in pager-mode
(defvar emacspipe-regexp "emacspipe[.][A-Za-z0-9]\\{10\\}"
  "Regexp describing the name of temporary files used for paging")
(add-to-list 'auto-mode-alist (cons emacspipe-regexp 'pager-mode))

;; Helper macro to replace `eval-after-load'
(defmacro nispio/after (mode &rest body)
  "`eval-after-load' MODE evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,mode
     '(progn ,@body nil)))

;; Custom function to toggle fullscreen by maximizing or restoring the current frame.
(defvar nispio/fullscreen-p t "Check if fullscreen is on or off")

(defun nispio/restore-frame ()
  (if (fboundp 'w32-send-sys-command) (w32-send-sys-command 61728)
    (set-frame-parameter nil 'width 82)
	(set-frame-parameter nil 'fullscreen 'fullheight)))
(defun nispio/maximize-frame ()
  (if (fboundp 'w32-send-sys-command) (w32-send-sys-command 61488)
    (set-frame-parameter nil 'fullscreen 'fullboth)))
(defun nispio/toggle-fullscreen (&optional DEL)
  "Toggle \"fullscreen\" by maximizing or restoring the current frame.
  If optional argument DEL is non-nil, delete the current frame."
  (interactive "P")
  (if DEL (delete-frame)
	(setq nispio/fullscreen-p (not nispio/fullscreen-p))
	(if nispio/fullscreen-p
		(nispio/restore-frame)
	  (nispio/maximize-frame))))

(defun nispio/show-prefix-arg (&optional arg)
  "Show the raw prefix arg"
  (interactive "P")
  (message "%s" arg))

(defun nispio/trim-string (arg)
  "Simple function for trimming the whitespace from the ends of
 a string. Also removes any string properties such as font faces."
  (let ((str (substring-no-properties arg)))
	(when (string-match "^[ \t]+" str)
	  (setq str (replace-match "" nil nil str)))
	(when (string-match "[ \t]+$" str)
	  (setq str (replace-match "" nil nil str)))
	str))

(defun nispio/directory-subdirs (directory &optional reject)
  "Returns a list of all subdirectories contained in DIRECTORY.
Optional argument REJECT can specify a list of subdirectory names
to ignore."
  (let* ((dir (file-name-as-directory directory))
		 (ls (directory-files (file-name-as-directory dir)))
		 (files (mapcar (lambda (el) (concat dir el)) ls))
		 (reject (append reject '("." "..")))
		 (prunes (mapcar (lambda (el) (concat dir el)) reject)))
	(prune-directory-list files nil prunes)))


(defun nispio/set-window-size (window length &optional horizontal ignore)
  "Resize WINDOW vertically to length lines.
WINDOW can be an arbitrary window and defaults to the selected
one.  An attempt to resize the root window of a frame will raise
an error though.

For more information, refer to the doc string of
`window-resize'.
"
  (interactive (list nil (prefix-numeric-value current-prefix-arg) nil nil))
  (setq window (window-normalize-window window))
  (let* ((cur-height (window-body-height window))
		(cur-width (window-body-width window))
		(delta (if horizontal
				   (- length cur-width)
				 (- length cur-height))))
	(window-resize window delta horizontal ignore)))


;; This function is used in nispio/run-debugger
(defun nispio/delete-window-maybe (&optional window)
  "Delete WINDOW.
WINDOW must be a valid window and defaults to the selected one.
Return nil.

If WINDOW is the root window of its frame, then make sure the
window is not dedicated to its buffer.

For more information see `delete-window'.
"
  (setq window (window-normalize-window window))
  (if (frame-root-window-p window)
	  (set-window-dedicated-p window nil)
	(delete-window window)))

(defun nispio/menu-item-property (menu item property value)
  "Set VALUE of named PROPERTY in menu item ITEM from MENU"
  (let* ((map (assoc item (cdr menu)))
		 (n (position property map)))
	(if (numberp n)
		 (setf (elt map (+ 1 n)) value)
	   (nconc map (list property value)))
	map))

;; (source: https://github.com/magnars/.emacs.d/blob/master/defuns/lisp-defuns.el)
(defun nispio/eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
	  (prin1 (eval (read (current-kill 0)))
			 (current-buffer))
	(error (message "Invalid expression")
		   (insert (current-kill 0)))))

(defun nispio/calc-grab-number (top bot)
  "Parse the region as a single number and push it on the Calculator stack."
  (interactive "r")
  (require 'calc-ext)
  (calc-do-grab-region top bot '(4)))


;; (source: http://emacs.stackexchange.com/a/81/93)
(defun nispio/switch-to-scratch-and-back ()
  "Toggle between *scratch-MODE* buffer and the current buffer.
If a scratch buffer does not exist, create it with the major mode set to that
of the buffer from where this function is called."
  (interactive)
  (if (string-match "*scratch" (format "%s" (current-buffer)))
      (switch-to-buffer (other-buffer))
    (let ((mode-str (format "%s" major-mode)))
      (let ((scratch-buffer-name (get-buffer-create (concat "*scratch-" mode-str "*"))))
        (switch-to-buffer scratch-buffer-name)
        ; (source: http://stackoverflow.com/q/7539615)
        (funcall (intern mode-str))))))

;; (source: http://www.emacswiki.org/emacs/ReBuilder)
(defun nispio/reb-query-replace (to-string)
  "Replace current RE from point with `query-replace-regexp'."
  (interactive
   (progn (barf-if-buffer-read-only)
		  (list (query-replace-read-to
				 (reb-target-binding reb-regexp)
				 "Query replace"
				 t ))))
  (with-current-buffer reb-target-buffer
	(query-replace-regexp (reb-target-binding reb-regexp) to-string)))



(defvar find-exts "py m el"
  "Last arguments given to `find' by \\[nispio/dired-find-exts].")

(defvar find-exts-history nil)

(defun nispio/dired-find-exts (dir exts)
    "Search DIR recursively for files matching with extensions matching EXTS,
and run Dired on those files.
EXTS is a shell wildcard (not an Emacs regexp) and need not be quoted.
The default command run (after changing into DIR) is

    find . (-path */.git -prune -o (-name *.ext1 -o -name *.ext2)) -a -type f

This function is a wrapper around \\[find-dired]."
  (interactive (list (read-directory-name "Run find in directory: " nil "" t)
					 (read-string "Find extensions (no . or *): " find-exts
								  '(find-exts-history . 1))))
  (let* ((dir (file-name-as-directory (expand-file-name dir)))
		 (ext-list (split-string exts))
		 (ext-glob (lambda (x) (concat "\\*." x)))
		 (find-args (mapconcat ext-glob ext-list " -o -name "))
		 (find-args (concat
					 "\\( -path \\*/.git -prune -o \\( -name "
					 find-args
					 " \\) \\) -a -type f")))
	;; Store this time's input for next time
	(setq find-exts exts)
	;; Call `find-dired' to do the rest of the work
	(find-dired dir find-args)))

;; Open all marked files at once, but only show one
(defun nispio/find-marked-files ()
  (interactive)
  (let ((file (dired-get-filename nil t))
		buf)
	(dired-do-find-marked-files 0)
	(quit-window)
	(setq buf (and file (get-file-buffer file)))
    (and buf (show-buffer nil buf))))

;; Copy to the clipboard the full path to the marked file
(defun nispio/dired-copy-filename ()
  "Copy full path of marked file onto the clipboard."
  (interactive)
  (let ((filename (dired-copy-filename-as-kill 0)))
    (x-own-selection-internal 'PRIMARY filename)))


;; SOURCE: http://emacs.stackexchange.com/a/16854/93
(defun nispio/set-comment-char (char)
  "Sets comment char for current buffer."
  (interactive "sComment char: ")
  (setq comment-start char)
  (font-lock-add-keywords nil `((,(concat comment-start ".+") . font-lock-comment-face)))
  )

(defun nispio/strip-1 (beg end)
  "Strip the first and last character from the active region.
This is intended to be the inverse operation of electric-pair
insertion."
  (interactive "r")
  (when (use-region-p)
    ;; TODO: It would be nice to ignore leading/trailing whitespace
    (let ((deactivate-mark nil))
      (delete-region (1- end) end)
      (delete-region beg (1+ beg)))))


(defun nispio/revert-buffer (&optional preserve-modes)
  "A wrapper around `revert-buffer' which only prompts for
confirmation if the buffer has been modified.  Also strips text
properties from the buffer before reverting.

Optional third argument PRESERVE-MODES non-nil means don't alter
the files modes.  Normally we reinitialize them using
`normal-mode'.  Setting PRESERVE-MODES to be non-nil will also
prevent removing of text properties."
  (interactive "P")
  (unless preserve-modes (nispio/remove-properties))
  (revert-buffer t (not (buffer-modified-p)) preserve-modes))


(defun nispio/remove-properties (&optional start end)
  "Removes all text properties from the region.  If no region is
selected, removes all text properties from the buffer."
  (interactive (list
                (and (use-region-p) (region-beginning))
                (and (use-region-p) ( region-end))))
  (save-restriction
    (widen)
    (let ((start (or start (point-min)))
          (end (or end (point-max)))
          (modified (buffer-modified-p)))
      (set-text-properties start end nil)
      (set-buffer-modified-p modified))))


(defvar nispio/scroll-lines-amount 5
  "The amount to be scrolled by `nispio/scroll-[up|down]-lines'.")

(defun nispio/scroll-down-lines (&optional arg)
  "Scroll text of selected window down ARG lines.  If ARG is omitted or
 nil, scroll down by the value of `nispio/scroll-lines-amount'."
  (interactive "P")
  (setq arg (and arg (prefix-numeric-value arg)))
  (scroll-down-line (or arg nispio/scroll-lines-amount)))

(defun nispio/scroll-up-lines (&optional arg)
  "Scroll text of selected window up ARG lines.  If ARG is omitted or
 nil, scroll up by the value of `nispio/scroll-lines-amount'."
  (interactive "P")
  (setq arg (and arg (prefix-numeric-value arg)))
  (scroll-up-line (or arg nispio/scroll-lines-amount)))


(defun nispio/delete-this-file ()
  (interactive)
  (if (and buffer-file-name (file-exists-p buffer-file-name))
      (if (yes-or-no-p (format "Permanently delete file '%s'? " buffer-file-name))
          (progn
            (delete-file buffer-file-name)
            (kill-buffer))
        (if (y-or-n-p (format "Kill buffer '%s'? " (buffer-name (current-buffer))))
            (kill-buffer)))
    (kill-buffer)))

(defun nispio/align-regexp ()
  "Wrapper around align-regexp which works around issues that
can occur when newlines are included in the whitespace syntax
class. [bug #23339]"
  (interactive)
  (setq this-command 'align-regexp)
  (if (eq ?\s (char-syntax ?\n))
      (let ((table (copy-syntax-table (syntax-table))))
        (modify-syntax-entry ?\n ">" table)
        (with-syntax-table table
          (call-interactively 'align-regexp)))
    (call-interactively 'align-regexp)))


(defun nispio/cons-to-list (cons)
  "Modifies CONS to turn it into a list."
  (when (consp cons)
    (setcdr (last cons) (cons (cdr (last cons)) nil)))
  (and (listp cons) cons))

(defun nispio/list-to-cons (list)
  "Modifies LIST to turn it into a nested cons."
  (when (< 1 (safe-length list))
    (setcdr (last list 2) (car (last list))))
  (and (consp list) list))

(defsubst nispio/true-listp (list)
  "Checks whether a list is actually nil-terminated"
  (and (listp list) (null (cdr-safe (last list)))))

(defsubst nispio/true-consp (cons)
  "Checks for a nested cons cell whose last element does not link
to nil."
  (and (< 1 (safe-length cons)) (cdr-safe (last list))))

(defun nispio/rotate-list (list N)
  (let* ((consp (nispio/true-consp list))
         (x (if consp (nispio/cons-to-list list) list))
         (size (length x))
         (n (mod N size)))
    (setq x (append (last x n) (butlast x n)))
    (if consp (nispio/list-to-cons x) x)))




(provide 'nispio/misc-utils)
