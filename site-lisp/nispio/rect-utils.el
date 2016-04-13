
;; (source: http://ergoemacs.org/emacs/emacs_copy_rectangle_text_to_clipboard.html)
(defun nispio/copy-rectangle-to-kill-ring (start end)
  "Saves a rectangle to the normal kill ring. Not suitable for yank-rectangle."
  (interactive "r")
  (let ((lines (extract-rectangle start end)))
    (with-temp-buffer
      (while lines ;; insert-rectangle, but without the unneeded stuff
        ;; (most importantly no push-mark)
        (insert-for-yank (car lines))
        (insert "\n")
        (setq lines (cdr lines)))
      (kill-ring-save (point-min) (point-max)))))

;; (adapted from: http://emacs.stackexchange.com/a/3174/93)
(defun nispio/push-killed-rectangle-to-kill-ring (&optional arg)
  "Saves a rectangle to the normal kill ring. Not suitable for yank-rectangle."
  (interactive "*P")
  (let ((lines killed-rectangle))
    (with-temp-buffer
      (while lines
        (insert-for-yank (car lines))
        (insert "\n")
        (setq lines (cdr lines)))
      (kill-ring-save (point-min) (point-max)))
	(setq phi-rectangle--last-killed-is-rectangle nil)))

(defun nispio/yank-rectangle-from-kill-ring (&optional arg)
  "Copy the current killed rectangle to the kill ring and"
  (interactive "*P")
  (nispio/push-killed-rectangle-to-kill-ring)
  (yank arg))

(defun nispio/fake-cursor-at-point ()
  "Create a fake cursor at point.  Multiple-cursors mode must be
activated before the fake cursor becomes active."
  (interactive)
  (mc/create-fake-cursor-at-point))

(defun nispio/mc-many-cursors (&optional arg)
  "Insert a number of fake cursors equal to length of the last
  killed rectangle.  If ARG is non-nil, insert N-1 fake cursors.
  In both cases, `multiple-cursors-mode' is activated."
  (interactive "*P")
  (let ((N (1- (or (and arg (prefix-numeric-value arg))
                   (and killed-rectangle (length killed-rectangle))
                   1))))
    (when (and (not arg) killed-rectangle)
      (setq phi-rectangle--last-killed-is-rectangle t))
    (insert (make-string N 32))
    (backward-char N)
    (dotimes (n N)
      (mc/create-fake-cursor-at-point)
      (forward-char))
    (mc/maybe-multiple-cursors-mode)))

(defun nispio/mc-insert-numbers-1 (&optional arg)
  "Insert increasing numbers for each cursor, starting at 1 or ARG."
  (interactive "*P")
  (setq this-command 'mc/insert-numbers)
  (setq mc--this-command 'mc/insert-numbers)
  (setq current-prefix-arg (or arg 1))
  (call-interactively 'mc/insert-numbers))

;; (defun nispio/region-to-phi-rectangle (start end &optional fill)
;;   (interactive "r\nP")
;;   (unless (fboundp 'phi-rectangle-set-mark-command)
;; 	(error "This command require the package `phi-rectangle'"))
;;   (exchange-point-and-mark)
;;   (phi-rectangle-set-mark-command)
;;   (exchange-point-and-mark))

(provide 'nispio/rect-utils)
