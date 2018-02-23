;; Use online archives for downloading emacs packages

(when (>= emacs-major-version 24)
  (require 'package)
  (setq package-enable-at-startup nil)
  (package-initialize nil)

  ;; Simplify loading of packages from the network with use-package.el
  ;; (source: https://github.com/jwiegley/use-package)
  (unless (package-installed-p 'use-package)
	(message "%s" "Refreshing package database...")
	(package-refresh-contents)
	(package-install 'use-package))
  (require 'use-package)

  ;; Allow asynchronous processing in emacs
  ;; (source: https://github.com/jwiegley/emacs-async)
  (use-package async :ensure t))

(defmacro nispio/require (name &rest ignored)
  "Simple macro which requires a package by name."
  `(let* ((name (quote ,name))
	  (package (if (stringp name) (intern name) name)))
     (require package)))

;; If use-package is not loaded, use require instead
(unless (fboundp 'use-package)
  (defalias 'use-package 'nispio/require))

;; (source: http://emacs.stackexchange.com/a/22174/93)
(defun package-update (package &optional version)
  "Update a package from the package archives.
If VERSION is nil, update the package to the most recent version
available.  Otherwise, VERSION should be a version string, or a
list of the type returned by `version-to-list'. The package will
be updated only if the currently installed version is less than
the version specified, even if a newer version is available."
  (unless package--initialized
    (package-initialize t))
  (unless package-archive-contents
    (package-refresh-contents))
  (let* ((current (cadr (assoc package package-alist)))
         (current-version (if current (package-desc-version current) '(-1)))
         (pkg-desc (cadr (assoc package package-archive-contents)))
         (pkg-version (and pkg-desc (package-desc-version pkg-desc)))
         (target-version (or (and (stringp version) (version-to-list version))
                             version
                             pkg-version)))
    (when (version-list-< current-version target-version)
      (if (null pkg-desc)
        (error "Package `%s' not found in package archives" package))
      (if (version-list-< pkg-version target-version)
        (error "A suitable version of `%s' is not available" package))
      (package-install pkg-desc)
      (if current
          (package-delete current)))
    nil))



(defun nispio/create-custom-archive (whitelist-file &optional repository-list)
  (let* ((dir (file-name-directory whitelist-file))
		 (dest-file (concat dir "archive-contents"))
		 (whitelist (nispio/read-contents whitelist-file))
		 my-archive name added full-name readme-name)
	(setq repository-list (or repository-list
                              '("http://orgmode.org/elpa/"
                                "http://melpa.org/packages/"
                                "http://elpa.gnu.org/packages/"
                                "file:///home/jph/.emacs.d/local-elpa-misc/")))
	(dolist (url repository-list)
	  (dolist (package (cdr (nispio/get-archive-contents url)))
		(let* ((name (car package))
			   (full-name (nispio/package-full-name package))
			   (readme-name (concat (symbol-name name) "-readme.txt")))
		(when (and (memq name whitelist) (not (memq name added)))
		  (setq my-archive (cons package my-archive))
		  (setq added (cons name added))
		  (message "Downloading %s" full-name)
		  (url-copy-file (concat url full-name) (concat dir full-name) t)
		  (url-copy-file (concat url readme-name) (concat dir readme-name) t)))))
	(nispio/write-archive-to-file my-archive dest-file)))

(defun nispio/package-full-name (package)
  (let* ((name (symbol-name (car package)))
		 (version (package--ac-desc-version (cdr package)))
		 (version-string (package-version-join version))
		 (kind (package--ac-desc-kind (cdr package)))
		 (suffix (if (eq kind 'tar) "tar" "el")))
	(concat name "-" version-string "." suffix)))

(defun nispio/get-archive-contents (repository-url)
  (let* ((file-url (concat repository-url "archive-contents"))
		 (file (or (url-file-local-copy file-url) file-url)))
	(nispio/read-contents file)))

(defun nispio/read-contents (filename)
  (if (file-exists-p filename)
	(with-temp-buffer
	  (insert-file-contents-literally filename)
	  (let ((contents (read (current-buffer))))
		contents))
	(error "Cannot find file %s" filename)))

(defun nispio/write-archive-to-file (archive file)
  (when archive
	(with-temp-file file
		(insert (prin1-to-string (cons '1 archive)))
	  file)))




(provide 'nispio/package-config)
