(require 'easy-mmode)
(require 'nispio/key-utils)

(defvar my-map (make-sparse-keymap)
  "Keymap for my personal key bindings")

(define-minor-mode my-mode
  "A minor mode which provides my personal key bindings."
  nil " MY" my-map)

(defun my-mode-maybe ()
  "Enable minor my-mode in the current buffer, unless in minibuffer"
  (if (not (minibufferp (current-buffer)))
      (my-mode 1)))

(define-global-minor-mode my-global-mode my-mode my-mode-maybe)

(defun enable-my-global-mode ()
  "Command to enable my-global-mode"
  (interactive)
  (my-global-mode 1)
  (setq-default my-mode 1)
  (message "my-global-mode enabled"))

(defun disable-my-global-mode ()
  "Command to disable my-global-mode"
  (interactive)
  (my-global-mode -1)
  (setq-default my-mode nil)
  (message "my-global-mode disabled"))

(defun nispio/fake-M-RET ()
  "Simulating pressing M-RET"
  (interactive)
  (let ((command (key-binding [M-return])))
    (setq last-command-event [M-return])
    (setq this-command command)
    (call-interactively command)))

(defun nispio/press (key)
  (interactive)
  (let* ((local (local-key-binding key))
         (global (global-key-binding key))
         (command (or local global this-command)))
    (unless (eq command this-command)
      (setq last-command-event key)
      (setq this-command command)
      (call-interactively command))))

(defun nispio/meta-up ()
  (interactive)
  (cond
   ((eq major-mode 'org-mode)
    (call-interactively #'org-metaup))
   (t (nispio/press (kbd "<M-up>")))))

(defun nispio/meta-down ()
  (interactive)
  (cond
   ((eq major-mode 'org-mode)
    (call-interactively #'org-metadown))
   (t (nispio/press (kbd "<M-down>")))))

(defun nispio/meta-return ()
  (interactive)
  (cond
   ((eq major-mode 'org-mode)
    (call-interactively #'org-meta-return))
   (t (nispio/press [M-RET]))))

;; ;; If not in a TTY, Unbind C-m, C-i, and C-[ so we can use them elsewhere
;; (if (and nil (not (display-graphic-p)))
;;     (setq tty-keys t)
;;   (define-key input-decode-map [?\C-m] [C-m])
;;   (define-key input-decode-map [?\C-i] [C-i])
;;   (define-key input-decode-map [?\C-\[] [C-\[])
;;   (define-key function-key-map [C-m] [?\C-m])
;;   (define-key function-key-map [C-i] [?\C-i])
;;   (define-key function-key-map [C-\[] [?\C-\[])
;;   (setq tty-keys nil))

(defun nispio/init-terminal-local-map (&optional frame)
  (with-selected-frame frame
    (when (display-graphic-p)
      (define-key input-decode-map [?\C-m] [C-m])
      (define-key input-decode-map [?\C-i] [C-i])
      (define-key input-decode-map [?\C-\[] [C-\[])
      (define-key function-key-map [C-m] [?\C-m])
      (define-key function-key-map [C-i] [?\C-i])
      (define-key function-key-map [C-\[] [?\C-\[])
      (frame-terminal))))

(nispio/init-terminal-local-map (window-frame))
(add-hook 'after-make-frame-functions 'nispio/init-terminal-local-map)

(defun nispio/apply-hyper-modifier (&optional prmpt)
  "\\<function-key-map>Add the Hyper modifier to the following event.
For example, type \\[event-apply-hyper-modifier] & to enter Hyper-&."
  (vector (event-apply-modifier (read-event (or prmpt "H-")) 'hyper 24 "H-")))

;; Turn C-] into a sticky "hyper" modifier
(define-key function-key-map [?\C-\]] 'nispio/apply-hyper-modifier)
(define-key global-map [?\C-\]] nil)

;; Simple macro to define a key binding
(defmacro nispio/bind-key (key-name command &optional keymap)
  (let ((namevar (make-symbol "name"))
        (keyvar (make-symbol "key"))
        (bindingvar (make-symbol "binding")))
    `(let* ((,namevar ,key-name)
            (,keyvar (if (vectorp ,namevar) ,namevar (read-kbd-macro ,namevar)))
            (,bindingvar (lookup-key (or ,keymap global-map) ,keyvar)))
       (define-key (or ,keymap global-map) ,keyvar ,command))))

(defmacro nispio/bind-keys (map-name bindings)
  (declare (indent 1))
  `(mapc (lambda (x)
           (nispio/bind-key (car x) (cdr x) (or ,map-name global-map)))
         ,bindings))

(provide 'nispio/my-mode)
