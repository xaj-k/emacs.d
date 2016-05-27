
(require 'ps-print)

(custom-set-variables
 '(ps-font-size     9.5)
 '(ps-header-lines    1)
 '(ps-left-margin    40)
 '(ps-right-margin   20)
 '(ps-top-margin     20)
 '(ps-bottom-margin  40)
 '(ps-header-offset  20))

(defvar pdf-viewers '("okular" "kpdf" "evince" "sumatra")
  "List of PDF viewers to search for")

(defvar nispio/ps-print-faces
  '((default "black" "white")
    (font-lock-builtin-face "NavyBlue")
    (font-lock-comment-delimiter-face "DarkOrange4")
    (font-lock-comment-face "chartreuse4" nil italic)
    (font-lock-constant-face "SaddleBrown")
    (font-lock-doc-face "DarkOrange4")
    (font-lock-function-name-face "RoyalBlue4")
    (font-lock-keyword-face "NavyBlue")
    (font-lock-negation-char-face nil nil bold)
    (font-lock-preprocessor-face "NavyBlue")
    (font-lock-regexp-grouping-backslash nil nil bold)
    (font-lock-regexp-grouping-construct nil nil bold)
    (font-lock-string-face "DarkRed")
    (font-lock-type-face "NavyBlue")
    (font-lock-variable-name-face "RoyalBlue4")
    (font-lock-warning-face "DeepPink3")))

(defun nispio/ps-print-faces ()
  (let (face-map)
    (dolist (face-ext nispio/ps-print-faces face-map)
      (ps-extend-face face-ext 'merge 'face-map))))

(defun nispio/locate-pdf-viewer ()
  (cl-loop for viewer in pdf-viewers
           when (executable-find viewer)
           return it))

(defun nispio/ps-to-pdf-maybe (filename)
  (let* ((ps2pdf (executable-find "ps2pdf"))
         (filename (expand-file-name filename))
         (pdf-file (concat (file-name-sans-extension filename) ".pdf")))
    (if (and ps2pdf
             (eq 0 (call-process ps2pdf nil nil nil filename pdf-file))
             (file-exists-p pdf-file))
        (delete-file filename)
      (setq pdf-file filename))
    pdf-file))

;;;###autoload
(defun nispio/ps-print-buffer (&optional filename)
  (interactive (ps-print-preprint current-prefix-arg))
  (let ((ps-print-face-extension-alist (nispio/ps-print-faces))
        (pdf-viewer (nispio/locate-pdf-viewer))
        (filename (expand-file-name (or filename "~/emacs_print.ps"))))
    (ps-print-buffer-with-faces filename)
    (setq filename (nispio/ps-to-pdf-maybe filename))
    (start-process "PDF-Viewer" nil pdf-viewer filename)))

(provide 'nispio/print)
