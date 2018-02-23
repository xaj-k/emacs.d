
(require 'helm-ag)

(defcustom helm-silver-case-sensitive nil
  "Make `helm-silver' queries case-sensitive (the default is smart-case)"
  :type 'boolean)

(defun helm-silver--candidate-process (&rest args)
  (cl-letf (((symbol-function 'helm-ag--construct-do-ag-command)
             'helm-silver--construct-command))
    (apply 'helm-ag--do-ag-candidate-process args)))

(defun helm-silver--construct-command (pattern)
  (let* ((opt-query (helm-silver--parse-file-and-query pattern))
         (file (car opt-query))
         (query (cdr opt-query)))
    (when helm-ag-use-emacs-lisp-regexp
      (setq file (helm-ag--elisp-regexp-to-pcre file)
            query (helm-ag--elisp-regexp-to-pcre query)))
    (if (not (string= query ""))
        (append (car helm-do-ag--commands)
                (list (if helm-silver-case-sensitive "-s" "-S"))
                (list "-G" (helm-ag--join-patterns file))
                (list "--" (helm-ag--join-patterns query))
                (cdr helm-do-ag--commands))
      (unless (string= file "")
        (append (car helm-do-ag--commands)
                (list (if helm-silver-case-sensitive "-s" "-S"))
                (list "-g" (helm-ag--join-patterns file))
                (cdr helm-do-ag--commands))))))

(defvar helm-silver--line nil "Line to skip to after persistent action")

(defun helm-silver--parse-file-and-query (input)
  (with-temp-buffer
    (insert input)
    (let (end file line query buf)
      (goto-char (point-min))
      (when (re-search-forward "@" nil t)
        (setq end (match-end 0))
        (replace-match " ")
        (goto-char (point-min))
        (while (re-search-forward "\\(?:^\\|\\s-+\\)\\(\\S-+\\)\\(?:\\s-+\\|$\\)" end t)
          (push (match-string-no-properties 1) file)
          (when end
            (cl-decf end (- (match-end 0) (match-beginning 0))))
          (replace-match "")))
      (while (re-search-forward "\\(?:^\\|\\s-+\\)\\(\\S-+\\)\\(?:\\s-+\\|$\\)" nil t)
        (push (match-string-no-properties 1) query)
        (replace-match ""))
      (setq helm-silver--line line)
      (cons (mapconcat 'identity (reverse (or file '(""))) " ")
            (mapconcat 'identity (reverse (or query '(""))) " ")))))

(defun helm-silver--find-file-action (candidate find-func this-file &optional persistent)
  (when (memq 'pt helm-ag--command-features)
    ;; 'pt' always show filename if matched file is only one.
    (setq this-file nil))
  (let* ((file-line (or (helm-grep-split-line candidate)
                        (list candidate (or helm-silver--line "1"))))
         (filename (or this-file (cl-first file-line)))
         (line (if this-file
                   (cl-first (split-string candidate ":"))
                 (cl-second file-line)))
         (default-directory (or helm-ag--default-directory
                                helm-ag--last-default-directory
                                default-directory)))
    (unless persistent
      (setq helm-ag--last-default-directory default-directory))
    (funcall find-func filename)
    (goto-char (point-min))
    (when line
      (forward-line (1- (string-to-number line))))))

(defun helm-silver--persistent-action (candidate)
  (let ((find-func (if helm-ag-use-temp-buffer
                       #'helm-ag--open-file-with-temp-buffer
                     #'find-file)))
    (helm-silver--find-file-action candidate find-func (helm-ag--search-this-file-p) t)
    (helm-highlight-current-line)))

(defun helm-silver--action-find-file (candidate)
  (helm-silver--find-file-action candidate 'find-file (helm-ag--search-this-file-p)))

(defun helm-silver--action-find-file-other-window (candidate)
  (helm-silver--find-file-action candidate 'find-file-other-window (helm-ag--search-this-file-p)))

(defvar helm-silver--actions
  (helm-make-actions
   "Open file"              #'helm-silver--action-find-file
   "Open file other window" #'helm-silver--action-find-file-other-window
   "Save results in buffer" #'helm-ag--action-save-buffer))

(defvar helm-source-silver
  (helm-build-async-source "Helm Silver"
    :init 'helm-ag--do-ag-set-command
    :candidates-process 'helm-silver--candidate-process
    :persistent-action  'helm-silver--persistent-action
    :action helm-silver--actions
    :nohighlight t
    :requires-pattern 3
    :candidate-number-limit 9999
    :follow (and helm-follow-mode-persistent 1)))

(defun helm-silver--helm ()
  (message "%s" helm-do-ag--commands)
  (let ((search-dir (if (not (helm-ag--windows-p))
                        helm-ag--default-directory
                      (if (helm-do-ag--target-one-directory-p helm-ag--default-target)
                          (car helm-ag--default-target)
                        helm-ag--default-directory))))
    (helm-attrset 'name (helm-ag--helm-header search-dir)
                  helm-source-silver)
    (helm :sources '(helm-source-silver) :buffer "*helm-silver*"
          :input (helm-ag--insert-thing-at-point helm-ag-insert-at-point)
          :keymap helm-do-ag-map)))

(defun helm-silver-magic-forward-char (arg)
  "Move forward in user input or modify file pattern."
  (interactive "P")
  (cond
   ((or arg (not (eobp)))
    (forward-char (min (prefix-numeric-value arg)
                       (- (point-max) (point)))))
   ((search-backward "@" nil t))
   (t
    (beginning-of-line)
    (insert "@")
    (backward-char))))
(define-key helm-ag-map (kbd "C-f") #'helm-silver-magic-forward-char)
(define-key helm-ag-mode-map (kbd "C-f") #'helm-silver-magic-forward-char)
(define-key helm-do-ag-map (kbd "C-f") #'helm-silver-magic-forward-char)


;;;###autoload
(defun helm-silver (&rest args)
  (interactive)
  (cl-letf (((symbol-function 'helm-do-ag--helm) 'helm-silver--helm)
            (helm-ag-base-command (concat helm-ag-base-command (if args " -a" ""))))
    (apply 'helm-do-ag args)))

;;;###autoload
(defun helm-silver-project-root ()
  (interactive)
  (let ((rootdir (helm-ag--project-root)))
    (unless rootdir
      (error "Could not find the project root. Create a git, hg, or svn repository there first. "))
    (helm-silver rootdir)))

(provide 'nispio/helm-silver)
