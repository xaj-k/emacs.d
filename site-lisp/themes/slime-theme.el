;;; slime-theme.el --- an Emacs 24 theme based on Slime (tmTheme)
;;
;;; Author: Jason Milkins
;;; Version: 20141116
;; Package-Version: 20141115.2302
;;
;;; Url: https://github.com/emacsfodder/tmtheme-to-deftheme
;;; Package-Requires: ((emacs "24.0"))
;;
;;; Commentary:
;;  This theme was automatically generated by tmtheme-to-deftheme (tm2deftheme),
;;  from Slime (tmTheme)
;;
;;; Code:

(deftheme slime
  "slime-theme - Created by tmtheme-to-deftheme - 2014-11-16 10:26:41 +0800")

(custom-theme-set-variables
 'slime
)

(custom-theme-set-faces
 'slime
 ;; basic theming.

 '(default ((t (:foreground "#FFFFFF" :background "#292D30" ))))
 '(region  ((t (:background "#161A1F"))))
 '(cursor  ((t (:background "#F8F8F0"))))

 ;; Temporary defaults
 '(linum                               ((t (:foreground "#545759"  :background "#3e4245" ))))
 '(fringe                              ((t (                       :background "#3e4245" ))))

 '(minibuffer-prompt                   ((t (:foreground "#1278A8"  :background nil       :weight bold                                  ))))
 '(escape-glyph                        ((t (:foreground "orange"   :background nil                                                     ))))
 '(highlight                           ((t (:foreground "orange"   :background nil                                                     ))))
 '(shadow                              ((t (:foreground "#777777"  :background nil                                                     ))))

 '(trailing-whitespace                 ((t (:foreground "#FFFFFF"  :background "#C74000"                                               ))))
 '(link                                ((t (:foreground "#00b7f0"  :background nil       :underline t                                  ))))
 '(link-visited                        ((t (:foreground "#4488cc"                        :underline t :inherit (link)                  ))))
 '(button                              ((t (:foreground "#FFFFFF"  :background "#444444" :underline t :inherit (link)                  ))))
 '(next-error                          ((t (                                             :inherit (region)                             ))))
 '(query-replace                       ((t (                                             :inherit (isearch)                            ))))
 '(header-line                         ((t (:foreground "#EEEEEE"  :background "#444444" :box nil :inherit (mode-line)                 ))))

 '(mode-line-highlight                 ((t (                                             :box nil                                      ))))
 '(mode-line-emphasis                  ((t (                                             :weight bold                                  ))))
 '(mode-line-buffer-id                 ((t (                                             :box nil :weight bold                         ))))

 '(mode-line-inactive                  ((t (:foreground "#cccccc"  :background "#3e4245" :box nil :weight light :inherit (mode-line)   ))))
 '(mode-line                           ((t (:foreground "#ffffff"  :background "#3e4245" :box nil ))))

 '(isearch                             ((t (:foreground "#99ccee"  :background "#444444"                                               ))))
 '(isearch-fail                        ((t (                       :background "#ffaaaa"                                               ))))
 '(lazy-highlight                      ((t (                       :background "#77bbdd"                                               ))))
 '(match                               ((t (                       :background "#3388cc"                                               ))))

 '(tooltip                             ((t (:foreground "black"    :background "LightYellow" :inherit (variable-pitch)                 ))))

 '(js3-function-param-face             ((t (:foreground "#BFC3A9"                                                                      ))))
 '(js3-external-variable-face          ((t (:foreground "#F0B090"  :bold t                                                             ))))

 '(secondary-selection                 ((t (                       :background "#342858"                                               ))))
 '(cua-rectangle                       ((t (:foreground "#E0E4CC"  :background "#342858" ))))

 ;; Magit hightlight
 '(magit-item-highlight                ((t (:foreground "white" :background "#1278A8" :inherit nil ))))

 ;; flyspell-mode
 '(flyspell-incorrect                  ((t (:underline "#AA0000" :background nil :inherit nil ))))
 '(flyspell-duplicate                  ((t (:underline "#009945" :background nil :inherit nil ))))

 ;; flymake-mode
 '(flymake-errline                     ((t (:underline "#AA0000" :background nil :inherit nil ))))
 '(flymake-warnline                    ((t (:underline "#009945" :background nil :inherit nil ))))

 ;;git-gutter
 '(git-gutter:added                    ((t (:foreground "#609f60" :bold t))))
 '(git-gutter:modified                 ((t (:foreground "#3388cc" :bold t))))
 '(git-gutter:deleted                  ((t (:foreground "#cc3333" :bold t))))

 '(diff-added                          ((t (:background "#305030"))))
 '(diff-removed                        ((t (:background "#903010"))))
 '(diff-file-header                    ((t (:background "#362145"))))
 '(diff-context                        ((t (:foreground "#E0E4CC"))))
 '(diff-changed                        ((t (:foreground "#3388cc"))))
 '(diff-hunk-header                    ((t (:background "#242130"))))


 '(font-lock-comment-face ((t (:foreground "#4F5A63"  ))))
 '(font-lock-string-face ((t (:foreground "#FAFFDB"  ))))
 '(font-lock-builtin-face ((t (:foreground "#C7AF3F"  ))))
 '(font-lock-variable-name-face ((t (:foreground "#8AB8A2"  ))))
 '(font-lock-keyword-face ((t (:foreground "#9FB3C2"  ))))
 '(font-lock-type-face ((t (:foreground "#C7AF3F"  :underline t))))
 '(font-lock-function-name-face ((t (:foreground "#C7AF3F"  ))))
 '(js3-function-param-face ((t (:foreground "#C7AF3F"  :italic t))))
 '(js2-function-param ((t (:foreground "#C7AF3F"  :italic t))))
 '(font-lock-warning-face ((t (:foreground "#F8F8F0" :background "#8FBE00" ))))
 '(diff-removed ((t (:foreground "#00A8C6"  ))))
 '(diff-added ((t (:foreground "#A6E22E"  ))))
 '(diff-changed ((t (:foreground "#E6DB74"  ))))
 '(font-lock-comment-delimiter-face ((t (:foreground "#4F5A63"  ))))

;; Rainbow delimiters
 '(rainbow-delimiters-depth-1-face ((t (:foreground "#00768b"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "#018299"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "#018ea7"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "#019ab4"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "#01a6c2"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "#01b2d0"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "#01bdde"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "#01c9ec"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "#01d5fa"))))
 '(rainbow-delimiters-unmatched-face ((t (:foreground "#FF0000"))))
) ;; End face definitions

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'slime)

;;; slime-theme.el ends here
