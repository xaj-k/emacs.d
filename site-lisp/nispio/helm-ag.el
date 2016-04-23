
(require 'helm-ag)

(defun nispio/helm-ag--narrow (_arg)
  (let* ((result (with-current-buffer helm-buffer
                   (goto-char (point-min))
                   (forward-line 1)
                   (buffer-substring-no-properties (point) (point-max))))
         (buf "*helm ag narrow*")
         (parent (file-name-directory (directory-file-name default-directory)))
         (initial-input helm-input))
    (when (buffer-live-p (get-buffer buf))
      (kill-buffer buf))
    (with-current-buffer (get-buffer-create buf)
      (erase-buffer)
      (save-excursion (insert result)))
    (helm :sources '(nispio/helm-source-ag-narrow) :buffer "*helm-ag*"
          :keymap helm-ag-map)))

(defun nispio/helm-ag-narrow--init ()
  (let ((buf-coding buffer-file-coding-system))
    (helm-attrset 'recenter t)
    (with-current-buffer (helm-candidate-buffer 'global)
      (let* ((default-directory (or helm-ag--default-directory
                                    default-directory))
             (coding-system-for-read buf-coding)
             (coding-system-for-write buf-coding))
        (helm-ag--save-current-context)))))


(defvar nispio/helm-do-ag--actions
  (let* ((action-alist (assoc 'action helm-ag-source))
         (new-action (cons "Save for narrow" 'nispio/helm-ag--narrow)))
    (append (copy-alist (cdr action-alist)) (list new-action))))

(defvar nispio/helm-source-ag-narrow
  (let* ((new-source (copy-alist helm-ag-source))
         (init-alist (assoc 'action new-source)))
    (setf (cdr init-alist) 'nispio/helm-ag-narrow--init)
    new-source))

(let ((action-alist (assoc 'action helm-source-do-ag)))
  (setf (cdr action-alist) nispio/helm-do-ag--actions))

(provide 'nispio/helm-ag)
