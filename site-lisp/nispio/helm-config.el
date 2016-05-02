(require 'helm-config)
(require 'helm-regexp)



(defvar nispio/helm-follow-sources ()
  "List of sources for which helm-follow-mode should be enabled")

;; Use helm-follow-mode for the following sources:
(add-to-list 'nispio/helm-follow-sources 'helm-source-occur)
(add-to-list 'nispio/helm-follow-sources 'helm-source-moccur)

(defun nispio/helm-set-follow ()
  "Enable helm-follow-mode for the sources specified in the list
variable `nispio/helm-follow-sources'. This function is meant to
be run during `helm-initialize' and should be added to the hook
`helm-before-initialize-hook'."
  (mapc (lambda (source)
		  (when (memq source nispio/helm-follow-sources)
			(helm-attrset 'follow 1 (symbol-value source))))
		helm-sources))

;; Add hook to enable helm-follow mode for specified helm 
(add-hook 'helm-before-initialize-hook 'nispio/helm-set-follow)



;; Do helm-multi-occur in all buffers backed by files
;; (source: http://stackoverflow.com/q/14726601)
(defun nispio/helm-moccur-buffers ()
  "helm-multi-occur in all buffers backed by files."
  (interactive)
  (helm-multi-occur
   (delq nil
	 (mapcar (lambda (b)
		   (when (buffer-file-name b) (buffer-name b)))
		 (buffer-list)))))



;; Expand the Helm window to take up the entire frame
;; (source: http://emacs.stackexchange.com/a/650/93)
(defun nispio/helm-full-frame ()
  (interactive)
  (with-selected-window (helm-window)
    (delete-other-windows)))



(defun nispio/setup-helm-occur-from-isearch ()
  ;; Helm occur from isearch bindings
  (let ((map isearch-mode-map))
    (define-key map [remap isearch-occur] 'helm-occur-from-isearch)
    (define-key map (kbd "M-s O") 'helm-multi-occur-from-isearch)
    (define-key map (kbd "M-s i") 'helm-swoop-from-isearch)
    (define-key map (kbd "M-s B") 'helm-multi-swoop-all-from-isearch)))

(provide 'nispio/helm-config)
