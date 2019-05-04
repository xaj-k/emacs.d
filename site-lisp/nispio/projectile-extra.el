(require 'dash)
(require 'projectile)

;; Add the ability to use projects that do not have a normal marker (.git,
;; .projectile, etc) at the root of the directory tree
(defvar project-root-regexps ()
  "List of regexps to match against when projectile is searching for project
root directories.")

;; (source: https://github.com/bbatsov/projectile/issues/364#issuecomment-61296248)
(defun projectile-root-child-of (dir &optional list)
  (projectile-locate-dominating-file
   dir
   (lambda (dir)
     (--first
      (if (and
	   (s-equals? (file-remote-p it) (file-remote-p dir))
	   (string-match-p (expand-file-name it) (expand-file-name dir)))
	  dir)
      (or list project-root-regexps (list))))))
(nconc projectile-project-root-files-functions '(projectile-root-child-of))

(provide 'nispio/projectile-extra)
