
(require 'helm-mode)

(defun nispio/helm-comp-read (prompt collection
         &optional predicate require-match initial-input hist def inherit-input-method)
  (let ((completion-ignore-case t))
    (helm-comp-read prompt collection
                    :test predicate
                    :must-match require-match
                    :initial-input initial-input
                    :history hist
                    :default def)))

(defun nispio/helm-customize-group (&optional group other-window)
  (interactive)
  (cl-letf ((completing-read-function 'nispio/helm-comp-read))
    (if (called-interactively-p)
        (progn
          (setq this-command 'customize-group)
          (call-interactively this-command))
      (customize-group group other-window))))

(defun nispio/helm-customize-option (symbol)
  (interactive (list nil))
  (cl-letf ((completing-read-function 'nispio/helm-comp-read))
    (if (called-interactively-p)
        (progn
          (setq this-command 'customize-option)
          (call-interactively this-command))
      (customize-option symbol))))

;; (defun nispio/with-helm-completion (orig-fun &rest args)
;;   (interactive (list (helm-comm))))

(provide 'nispio/helm-customize-extra)
