;;; keybindings.el -*- lexical-binding: t; -*-

(provide 'user-keybindings)

;; Buffers and windows
(map! :leader
      :desc "Toggle RO"
      "br"
      'read-only-mode)

(map! :leader
      :desc "Hide window"
      "bk"
      '+workspace/close-window-or-workspace)

(map! :leader
      :desc "Kill buffer"
      "bq"
      'kill-buffer)

(map! :leader
      :desc "Switch to buffer"
      "bb"
      'consult-buffer)

(map! :leader
      :desc "Switch to workspace buffer"
      "."
      '+vertico/switch-workspace-buffer)

(map! :leader
      :desc "Switch to project"
      ","
      'counsel-projectile-switch-project)

(map! :leader
     :desc "M-x"
      "SPC"
      'counsel-M-x)

;; Smartparens
(defhydra hydra-smartparens ()
  "smartparens"
  ("f"   sp-forward-sexp "forward")
  ("b"   sp-backward-sexp "backward")
  ("F"   sp-forward-parallel-sexp "forward sibling")
  ("B"   sp-backward-parallel-sexp "backward sibling")
  ("u"   sp-down-sexp "down")
  ("d"   sp-up-sexp "up")
  ("n"   sp-next-sexp "next")
  ("p"   sp-previous-sexp "previous")
  ("-"   sp-narrow-sexp "narrow")
  ("a"   sp-beginning-of-sexp "beginning")
  ("e"   sp-end-of-sexp "end")
  ("y"   sp-copy-sexp "forward copy")
  ("Y"   sp-backward-copy-sexp "backward copy")
  ("d"   sp-kill-sexp "forward kill")
  ("D"   sp-backward-kill-sexp "backward kill")
  ("]"   sp-forward-barf-sexp "forward barf")
  ("["   sp-backward-barf-sexp "backward barf")
  ("}"   sp-slurp-hybrid-sexp "forward slurp")
  ("{"   sp-backward-slurp-sexp "backward slurp")
  ("w"   sp-forward-symbol "next symbol")
  ("b"   sp-backward-symbol "previous symbol")
  ("W"   sp-skip-forward-to-symbol "skip to next symbol")
  ("B"   sp-skip-backward-to-symbol "skip to previous symbol")
  ("_"   sp-unwrap-sexp "Unwrap")
  ("@"   sp-splice-sexp "Splice")
  (">"   sp-absorb-sexp "Absorb")
  ("<"   sp-emit-sexp "Emit")
  ("TAB" sp-forward-whitespace "Forward whitespace")
  ("DEL" sp-forward-whitespace "Backward whitespace")
  ("q"   nil "exit"))

(define-key smartparens-mode-map (kbd "M--") #'hydra-smartparens/body)
