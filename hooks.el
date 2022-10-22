;;; user-hooks.el -*- lexical-binding: t; -*-

(provide 'user-hooks)

(add-hook 'org-mode-hook (lambda nil
                           (setq fill-column 60)
                           (auto-fill-mode)))

(remove-hook 'org-mode-hook 'flycheck-mode)
(add-hook 'term-mode-hook 'evil-emacs-state)
(add-hook 'vterm-mode-hook 'evil-emacs-state)
