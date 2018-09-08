
(require 'org)

(defun scriptures-follow-link (&optional path)
  "Follow a scripture link."
  (org-open-file path))

(defun scriptures-complete-link (&optional arg)
  "Create a scripture link using completion."
  (let ((file (read-file-name "scriptures: "))
	(pwd (file-name-as-directory (expand-file-name ".")))
	(pwd1 (file-name-as-directory (abbreviate-file-name
				       (expand-file-name ".")))))
    (cond ((equal arg '(16))
	   (concat "scriptures:"
		   (abbreviate-file-name (expand-file-name file))))
	  ((string-match
	    (concat "^" (regexp-quote pwd1) "\\(.+\\)") file)
	   (concat "scriptures:" (match-string 1 file)))
	  ((string-match
	    (concat "^" (regexp-quote pwd) "\\(.+\\)")
	    (expand-file-name file))
	   (concat "scriptures:"
		   (match-string 1 (expand-file-name file))))
	  (t (concat "scriptures:" file)))))

(add-to-list 'org-link-parameters '("scriptures" :complete scriptures-complete-link))
(add-to-list 'org-link-parameters '("scriptures" :follow scriptures-follow-link))

(provide 'nispio/org-scriptures)
