;; Unbind a local key binding
(defun nispio/unbind-local-key (key)
  "Remove the local binding for a given key sequence"
  (interactive "kPress key: ")
  (let ((name (key-description key))
    (binding (local-key-binding key)))
    (if (not binding)
    (message "Key is not bound locally: %s" name)
      (local-set-key key nil)
      (message "Unbinding key: %s (was '%s)" name binding))))

;; Look for special keybindings associated with overlays or text properties
;; (source: http://emacs.stackexchange.com/a/654/93)
(defun nispio/key-binding-at-point (key)
  "Find key bindings associated with text properties or overlays at point"
  (mapcar (lambda (keymap) (lookup-key keymap key))
          (cl-remove-if-not
           #'keymapp
           (append
            (mapcar (lambda (overlay)
                      (overlay-get overlay 'keymap))
                    (overlays-at (point)))
            (get-text-property (point) 'keymap)
            (get-text-property (point) 'local-map)))))

;; Get raw text printed by a help function
(defun help-text (help-fun &rest args)
  (let ((help-xref-following t))
    (with-temp-buffer
      (help-mode)
      (apply help-fun args)
      (buffer-substring-no-properties (point-min) (point-max)))))

;; This is lifted directly from help.el
(defun describe-key--interactive-args ()
  (let ((enable-disabled-menus-and-buttons t)
        (cursor-in-echo-area t)
        saved-yank-menu)
    (unwind-protect
        (let (key)
          ;; If yank-menu is empty, populate it temporarily, so that
          ;; "Select and Paste" menu can generate a complete event.
          (when (null (cdr yank-menu))
            (setq saved-yank-menu (copy-sequence yank-menu))
            (menu-bar-update-yank-menu "(any string)" nil))
          (setq key (read-key-sequence "Describe key (or click or menu item): "))
          (list
           key
           (prefix-numeric-value current-prefix-arg)
           ;; If KEY is a down-event, read and include the
           ;; corresponding up-event.  Note that there are also
           ;; down-events on scroll bars and mode lines: the actual
           ;; event then is in the second element of the vector.
           (and (vectorp key)
                (let ((last-idx (1- (length key))))
                  (and (eventp (aref key last-idx))
                       (memq 'down (event-modifiers (aref key last-idx)))))
                (or (and (eventp (aref key 0))
                         (memq 'down (event-modifiers (aref key 0)))
                         ;; However, for the C-down-mouse-2 popup
                         ;; menu, there is no subsequent up-event.  In
                         ;; this case, the up-event is the next
                         ;; element in the supplied vector.
                         (= (length key) 1))
                    (and (> (length key) 1)
                         (eventp (aref key 1))
                         (memq 'down (event-modifiers (aref key 1)))))
                (read-event))))
      ;; Put yank-menu back as it was, if we changed it.
      (when saved-yank-menu
        (setq yank-menu (copy-sequence saved-yank-menu))
        (fset 'yank-menu (cons 'keymap yank-menu))))))

;; Show a list of all key bindings for a given key sequence
;; (derived from: http://emacs.stackexchange.com/a/654/93)
(defun nispio/locate-key-binding (&optional key untranslated up-event)
  "Determine in which keymap KEY is defined."
  (interactive (describe-key--interactive-args))
  (if (key-binding key t)
      (let* ((help-text (help-text 'describe-key key untranslated up-event))
             (desc (key-description key))
             (function (key-binding key t))
             (at-point-binding (nispio/key-binding-at-point key))
             (minor-mode-binding (minor-mode-key-binding key))
             (local-binding (local-key-binding key))
             (global-binding (global-key-binding key)))
        (with-help-window (help-buffer)
          (when at-point-binding
            (princ (format "At Point: %S\n" at-point-binding)))
          (when minor-mode-binding
            (princ (format "Minor-mode: %s\n"
                           (mapconcat (lambda (x) (format "%s: %s" (car x) (cdr x)))
                                      minor-mode-binding "\n            "))))
          (when (and local-binding (not (numberp local-binding)))
            (princ (format "Local: %s\n" local-binding)))
          (when global-binding
            (princ (format "Global: %s\n" global-binding)))
          (terpri)
          (princ help-text))
        function)
    (message "%s is undefined" (key-description key))))

(defun nispio/insert-key-description (key &optional arg)
"Capture a keybinding directly from the keyboard and insert its string
representation at point. With optional ARG, display the key description in the
minibuffer instead of inserting it at point."
  (interactive "k\nP")
  (let ((desc (key-description key)))
	(if arg (message desc) (insert desc))))

(defun nispio/describe-keymap (keymap)
  "List the binding in KEYMAP in a human-readable format"
  (interactive
   (list (intern (completing-read "Keymap: " obarray
     (lambda (m) (and (boundp m) (keymapp (symbol-value m)))) t nil nil))))
  (unless (and (symbolp keymap) (boundp keymap) (keymapp (symbol-value keymap)))
    (error "`%S' is not a keymapp" keymap))
  (let ((name (symbol-name keymap)))
	(with-help-window (help-buffer)
	  (save-excursion
		(read-only-mode -1)
		(princ (format "Key bindings in keymap `%s':\n\n" name))
		(princ (substitute-command-keys (concat "\\{" name "}")))
		))))

(defun nispio/simulate-key-event (event &optional N)
  "Simulate an arbitrary keypress event.

This function sets the `unread-command-events' variable in order to simulate a
series of key events given by EVENT. Can also For negative N, simulate the
specified key EVENT directly.  For positive N, removes the last N elements from
the list of key events in `this-command-keys' and then appends EVENT.  For N nil,
treat as N=1."
  (let ((prefix (listify-key-sequence (this-command-keys)))
		 (key (listify-key-sequence event))
		 (n (prefix-numeric-value N)))
	 (if (< n 0)
		 (setq prefix key)
	   (nbutlast prefix n)
	   (nconc prefix key))
	 (setq unread-command-events prefix)))

(defun nispio/unbind-digit-arguments ()
  "Unbind modified digit keys from the global map"
  (let ((prefix-list '("C-M-" "M-" "C-"))
		(digit-list (number-sequence 0 9))
		digit-string key)
	(dolist (digit digit-list)
	  (setq digit-string (format "%d" digit))
	  (mapc
	   (lambda (prefix-string)
		 (setq key (kbd (concat prefix-string digit-string)))
		 (when (eq (global-key-binding key) 'digit-argument)
		   (global-unset-key key)))
	   prefix-list))))

(defun nispio/rebind-digit-arguments ()
  "Unbind modified digit keys from the global map"
  (let ((prefix-list '("C-M-" "M-" "C-"))
		(digit-list (number-sequence 0 9))
		digit-string key)
	(dolist (digit digit-list)
	  (setq digit-string (format "%d" digit))
	  (mapc
	   (lambda (prefix-string)
		 (setq key (kbd (concat prefix-string digit-string)))
		 (unless (global-key-binding key)
		   (global-set-key key 'digit-argument)))
           prefix-list))))



(defmacro kmacro (name keys &optional doc)
  "Define a named keyboard macro.
NAME should be an unquoted symbol name to which the function
definition will be bound.  KEYS should be a string constant in
the format used for saving keyboard macros. DOC is an optional
docstring to be associated with the kbd macro. Returns NAME"
  (prog1
      `(defun ,name (&optional arg)
         ,doc
         (interactive "p")
         (kmacro-exec-ring-item (list ,(kbd keys) 0 "%d") arg))
    (put name 'kmacro t)))
(font-lock-add-keywords 'emacs-lisp-mode '(("(\\(kmacro\\)\\_>" 1 'font-lock-keyword-face)))


(provide 'nispio/key-utils)
