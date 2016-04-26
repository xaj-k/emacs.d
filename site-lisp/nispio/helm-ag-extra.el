
(require 'helm-ag)

(defvar nispio/helm-ag-narrow--buffer "*helm candidates:Helm Ag Narrow*")

(defun nispio/helm-ag--narrow (_arg)
  (let* ((result (with-current-buffer helm-buffer
                   (goto-char (point-min))
                   (forward-line 1)
                   (buffer-substring-no-properties (point) (point-max))))
         (buf nispio/helm-ag-narrow--buffer)
         (parent (file-name-directory (directory-file-name default-directory)))
         (initial-input helm-input))
    (when (buffer-live-p (get-buffer buf))
      (kill-buffer buf))
    (with-current-buffer (get-buffer-create buf)
      (erase-buffer)
      (save-excursion (insert result))
      (setq nispio/helm-ag-narrow--buffer (current-buffer)))
    (helm :sources '(nispio/helm-source-ag-narrow) :buffer "*helm-ag-narrow*"
          :keymap helm-ag-map)))

(defun nispio/helm-ag-narrow--init ()
  (helm-attrset 'recenter t)
  (with-current-buffer (helm-candidate-buffer nispio/helm-ag-narrow--buffer)
    (helm-ag--save-current-context)))

(defvar nispio/helm-do-ag--actions
  (let* ((action-alist (assoc 'action helm-ag-source))
         (new-action (cons "Save and narrow" 'nispio/helm-ag--narrow)))
    (append (copy-alist (cdr action-alist)) (list new-action))))

(defvar nispio/helm-source-ag-narrow
  (helm-build-in-buffer-source "Helm Ag Narrow"
    :init 'nispio/helm-ag-narrow--init
    :real-to-display 'helm-ag--candidate-transformer
    :persistent-action 'helm-ag--persistent-action
    :fuzzy-match helm-ag-fuzzy-match
    :action helm-ag--actions
    :candidate-number-limit 9999
    :follow (and helm-follow-mode-persistent 1)))

;;;###autoload
(defun nispio/ad-around-helm-do-ag (orig-fun &rest args)
  (let ((action-alist-ref (assoc 'action helm-source-do-ag)))
    (cl-letf (((cdr action-alist-ref) nispio/helm-do-ag--actions))
      (apply orig-fun args))))

;;;###autoload
(defun nispio/setup-helm-ag-narrow ()
  (advice-add 'helm-do-ag :around #'nispio/ad-around-helm-do-ag))

(provide 'nispio/helm-ag-extra)
