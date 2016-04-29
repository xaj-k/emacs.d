
(require 'mc-mark-more)

(defvar nispio/mc-goal-column t
  "Goal column for adding fake cursors in multiple-cursors mode.

An integer value specifies the desired goal column.
A value of 'current means use the current column of point.
A value of t means use the current goal-column if it is
  available, otherwise use the current column of point.")

(defun nispio/mc--goal-column ()
  "Returns the goal column that should be used by `mc/mark-lines'.

The logic in this function establishes the rules of precedence
for determining goal-column based on the symbol value of
`nispio/mc-goal-column'."
  (cond
   ((eq nispio/mc-goal-column 'current) (current-column))
   ((integerp nispio/mc-goal-column) nispio/mc-goal-column)
   (nispio/mc-goal-column (or goal-column (current-column)))
   (goal-column)))

;;;###autoload
(defun nispio/ad-around--mc-mark-lines (old-fun &rest args)
  "Optionally set a goal column when adding new cursors by line.
Also adds the ability to skip lines when using
`mc/mark-more-like-this-extended', so that the behavior more
closely mimics the behavior when a word is selected."
  (let ((goal-column (nispio/mc--goal-column)))
    (if (> (car args) 0)
        (apply old-fun args)
      (mc/save-excursion
       (let ((furthest-cursor (cl-ecase (cadr args)
                                (forwards  (mc/furthest-cursor-after-point))
                                (backwards (mc/furthest-cursor-before-point)))))
         (when (overlayp furthest-cursor)
           (goto-char (overlay-get furthest-cursor 'point))
           (mc/remove-fake-cursor furthest-cursor)))
       (cl-ecase (cadr args)
         (forwards (next-logical-line 1 nil))
         (backwards (previous-logical-line 1 nil)))
       (mc/create-fake-cursor-at-point)))))

;;;###autoload
(defun nispio/mc-setup-mark-lines ()
  "Advise `mc/mark-lines' with `nispio/ad-around--mc-mark-lines'"
  (advice-add 'mc/mark-lines :around #'nispio/ad-around--mc-mark-lines))

(provide 'nispio/mc-extra)
