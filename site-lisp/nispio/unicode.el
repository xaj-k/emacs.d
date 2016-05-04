
(require 'iso-transl)

(defvar greek-small
  (let ((map (make-sparse-keymap)))
    (mapc (lambda (x) (define-key map (kbd (car x)) (cdr x)))
          '(("a" . "α")  ;; alpha
            ("b" . "β")  ;; beta
            ("g" . "γ")  ;; gamma
            ("d" . "δ")  ;; delta
            ("e" . "ε")  ;; epsilon
            ("z" . "ζ")  ;; zeta
            ("h" . "η")  ;; eta
            ("q" . "θ")  ;; theta
            ("i" . "ι")  ;; iota
            ("k" . "κ")  ;; kappa
            ("l" . "λ")  ;; lambda
            ("m" . "μ")  ;; mu
            ("n" . "ν")  ;; nu
            ("v" . "ξ")  ;; xi
            ("o" . "ο")  ;; omicron
            ("p" . "π")  ;; pi
            ("r" . "ρ")  ;; rho
            ("s" . "σ")  ;; sigma
            ("t" . "τ")  ;; tau
            ("u" . "υ")  ;; upsilon
            ("f" . "φ")  ;; phi
            ("x" . "χ")  ;; chi
            ("y" . "ψ")  ;; psi
            ("w" . "ω")) ;; omega
          ) map)
  "Keymap of lower-case greek letters.")
(fset 'greek-small greek-small)
(define-key 'iso-transl-ctl-x-8-map "g" 'greek-small)

(defvar greek-large
  (let ((map (make-sparse-keymap)))
    (mapc (lambda (x) (define-key map (kbd (car x)) (cdr x)))
          '(("a" . "Α")  ;; Alpha
            ("b" . "Β")  ;; Beta
            ("g" . "Γ")  ;; Gamma
            ("d" . "Δ")  ;; Delta
            ("e" . "Ε")  ;; Epsilon
            ("z" . "Ζ")  ;; Zeta
            ("h" . "Η")  ;; Eta
            ("q" . "Θ")  ;; Theta
            ("i" . "Ι")  ;; Iota
            ("k" . "Κ")  ;; Kappa
            ("l" . "Λ")  ;; Lambda
            ("m" . "Μ")  ;; Mu
            ("n" . "Ν")  ;; Nu
            ("v" . "Ξ")  ;; Xi
            ("o" . "Ο")  ;; Omicron
            ("p" . "Π")  ;; Pi
            ("r" . "Ρ")  ;; Rho
            ("s" . "Σ")  ;; Sigma
            ("t" . "Τ")  ;; Tau
            ("u" . "Υ")  ;; Upsilon
            ("f" . "Φ")  ;; Phi
            ("x" . "Χ")  ;; Chi
            ("y" . "Ψ")  ;; Psi
            ("w" . "Ω")) ;; Omega
          ) map)
  "Keymap of upper-case greek letters.")
(fset 'greek-large greek-large)
(define-key 'iso-transl-ctl-x-8-map "G" 'greek-large)

(provide 'nispio/unicode)
