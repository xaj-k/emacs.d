
(require 'helm-command)

(defun nispio/helm--run-command (command-name)
  (let ((prefix-arg (or helm-current-prefix-arg prefix-arg))
        (sym-com (and (stringp command-name) (intern-soft command-name))))
    (when sym-com
      ;; Avoid having `this-command' set to *exit-minibuffer.
      (setq this-command sym-com)
      (setq real-this-command sym-com) ;; Handle C-x z (repeat)
      (command-execute sym-com 'record))))

(defun nispio/helm-def-source--emacs-commands (&optional default)
  (autoload 'helm-M-x-transformer-1 "helm-command")
  (helm-build-in-buffer-source "Commands"
    :init `(lambda ()
             (helm-apropos-init 'commandp ,default))
    :fuzzy-match helm-apropos-fuzzy-match
    :filtered-candidate-transformer (and (null helm-apropos-fuzzy-match)
                                         'helm-apropos-default-sort-fn)
    ;; Show keybindings for emacs commands in helm-apropos
    ;; (source: http://emacs.stackexchange.com/a/15053/93)
    :candidate-transformer 'helm-M-x-transformer-1
    :nomark t
    :persistent-action (lambda (candidate)
                         (helm-elisp--persistent-help
                          candidate 'helm-describe-function))
    :persistent-help "Describe command (use \\[nispio/helm--run-command] to run)"
    :action '(("Describe function" . helm-describe-function)
              ("Find function" . helm-find-function)
              ("Info lookup" . helm-info-lookup-symbol)
              ("Run command" . nispio/helm--run-command))))

;;;###autoload
(defun nispio/ad-around-helm-apropos (orig-fun &rest args)
  (cl-letf (((car helm-apropos-function-list) 'nispio/helm-def-source--emacs-commands))
    (apply orig-fun args)))

;;;###autoload
(defun nispio/setup-helm-apropos ()
  "Make the commands section of `helm-apropos' behave more like `helm-M-x'"
  (advice-add 'helm-apropos :around #'nispio/ad-around-helm-apropos))



(require 'helm-mode)

;;;###autoload
(defun nispio/helm-comp-read (prompt collection
         &optional predicate require-match initial-input hist def inherit-input-method)
  (let ((completion-ignore-case t))
    (helm-comp-read prompt collection
                    :test predicate
                    :must-match require-match
                    :initial-input initial-input
                    :history hist
                    :default def)))

;;;###autoload
(defun nispio/helm-customize-group (&optional group other-window)
  (interactive)
  (cl-letf ((completing-read-function #'nispio/helm-comp-read))
    (if (called-interactively-p)
        (progn
          (setq this-command 'customize-group)
          (call-interactively this-command))
      (customize-group group other-window))))

;;;###autoload
(defun nispio/helm-customize-option (symbol)
  (interactive (list nil))
  (cl-letf ((completing-read-function #'nispio/helm-comp-read))
    (if (called-interactively-p)
        (progn
          (setq this-command 'customize-option)
          (call-interactively this-command))
      (customize-option symbol))))



(provide 'nispio/helm-extra)
