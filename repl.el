;;; test.*el -*- lexical-binding: t; -*-
(provide 'repl)

(defvar repl-commands '((ruby-mode . ("/usr/bin/irb" "--inf-ruby-mode"))
                        (racket-mode . "/usr/bin/racket")
                        (perl-mode . "/usr/bin/perl")
                        (raku-mode . "/usr/bin/rakudo")
                        (python-mode . "/usr/bin/python")
                        (lua-mode . "/usr/bin/lua")
                        (sh-mode . "/bin/bash")))

(defvar repl-running '())

(defun repl::get (spec &optional mode cmd)
  (let* ((spec (cond
                ((keywordp spec)
                 spec)
                ((not spec)
                 :buffer-name)
                (t t)))
         (mode (if (or (not mode)
                       (eq (intern "") mode))
                   major-mode
                 mode))
         (cmd (if (or (not cmd)
                      (string-empty-p cmd))
                  (cdr (assoc mode repl-commands))
                cmd))
         (name (format "*%s-repl*" mode))
         (buffer (get-buffer name))
         (proc (get-buffer-process name)))
    (cond
     ((eq :buffer-name spec) name)
     ((eq :buffer spec) buffer)
     ((eq :process spec) proc)
     ((eq :cmd spec) cmd)
     ((eq :mode spec) mode)
     (t `((:buffer-name ,name)
          (:buffer ,buffer)
          (:process ,proc)
          (:mode ,mode)
          (:cmd ,cmd))))))

(defun repl::live? (&optional mode)
  (process-live-p (repl::get :process mode)))

(defun repl::start-process (&optional mode cmd)
  (let* ((mode (or mode major-mode))
         (cmd (ensure-list (repl::get :cmd mode)))
         (args (cdr cmd))
         (cmd (car cmd))
         (name (buffer-name (get-buffer-create (repl::get :buffer-name mode)))))
    (unless cmd
      (error "No command defined for %s" mode))
    (when (or (not (get-process name))
              (not (process-live-p (get-process name))))
      (apply #'make-comint-in-buffer `(,name ,name ,cmd nil ,@args))
      (unless (get-process name)
        (kill-buffer name)
        (error "Could not start REPL for %s with command %s" mode cmd)))
    (add-to-list 'repl-running name)
    (get-process name)))

(defun repl::kill-process (&optional mode)
  (let ((proc (repl::get :process mode))
        (buffer (repl::get :buffer mode)))
    (when proc
      (kill-process proc)
      (kill-buffer buffer)
      (setq repl-running (remove mode repl-running))
      t)))

(defun repl::assert-live-process (&optional mode)
  (unless (repl::live? mode)
      (error "No process running for %s" (or major-mode))))

(defun repl::send (s &optional mode)
  (repl::assert-live-process mode)
  (let* ((s (s-chomp (cond
                      ((keywordp s) (cond
                                     ((eq s :region)
                                      (buffer-substring-no-properties (mark) (point)))
                                     ((eq s :till-point)
                                      (buffer-substring-no-properties (point-min) (point)))
                                     ((eq s :line)
                                      (thing-at-point 'line t))
                                     ((eq s :buffer)
                                      (thing-at-point 'buffer t))
                                     (t
                                      (thing-at-point 'line t))))
                      ((symbolp s) (thing-at-point s t))
                      (t s)))))
    (process-send-string (repl::get :process mode) (concat s "\n"))))

(defun repl::hide (&optional mode)
  (repl::assert-live-process mode)
  (let ((win (get-buffer-window (repl::get :buffer mode) 'visible)))
    (when win
      (delete-window win))))

(defun repl::show (&optional split mode)
  (repl::assert-live-process mode)
  (let* ((buffer (repl::get :buffer mode))
         (win (get-buffer-window buffer 'visible)))
    (when win
      (delete-window win))
    (cond ((eq split :s) (progn
                           (split-window-vertically)
                           (windmove-down)
                           (switch-to-buffer buffer)))
          ((eq split :v) (progn
                           (split-window-horizontally)
                           (windmove-right)
                           (switch-to-buffer buffer))))))

(defun repl::show-split (&optional mode)
  (repl::show :s mode))

(defun repl::show-vsplit (&optional mode)
  (repl::show :v mode))

(defun repl::--parse-mode (&optional mode command)
  (let* ((mode (if (or (not mode)
                       (eql mode (intern "")))
                   major-mode
                 mode))
         (command (if (string-empty-p command)
                      nil
                    command)))
    `(,mode ,command)))



(defun repl::ivy::get-mode-from-name (bufname)
  (intern (replace-regexp-in-string
           "[*]\\([a-z-]+\\)-repl[*]"
           "\\1"
           bufname)))

(defun repl::ivy::split (bufname)
  (with-ivy-window
    (repl::show :s (repl::ivy::get-mode-from-name bufname))))

(defun repl::ivy::vsplit (bufname)
  (with-ivy-window
    (repl::show :v (repl::ivy::get-mode-from-name bufname))))

(defun repl::ivy::kill (bufname)
  (with-ivy-window
    (repl::kill-process (repl::ivy::get-mode-from-name bufname))))

(defun repl::ivy::send-string (bufname)
  (with-ivy-window
    (let* ((mode (repl::ivy::get-mode-from-name bufname))
           (prompt (format "(%s) > " mode))
           (userint (read-from-minibuffer prompt)))
      (when (> (length userint) 0)
        (repl::send (concat userint "\n") mode)))))


(defun repl::ivy::start (mode)
  (repl::start-process (intern mode)))

(defun repl::ivy::edit (mode &optional run)
  (let* ((mode (intern mode))
         (command (nthcdr 1 (assoc mode repl-commands)))
         (prompt (format "%s -> ? > " command))
         (new (read-from-minibuffer prompt)))
    (if (> (length new) 0)
        (setcdr (assoc mode repl-commands) new)
      (message "No command provided fou %s.\nUsing default command: %s" mode command))
    (if run
        (repl::start-process mode))))

(defun repl::ivy::edit-and-run (mode)
  (repl::ivy::edit mode t))

(defun repl::ivy::start-and-split (mode)
  (with-ivy-window
    (repl::ivy::start mode)
    (repl::ivy::split (format "*%s-repl*" mode))))

(defun repl::ivy::start-and-vsplit (mode)
  (with-ivy-window
    (repl::ivy::start mode)
    (repl::ivy::vsplit (format "*%s-repl*" mode))))

(defun repl::ivy/start nil
  (interactive)
  (ivy-read (format "Start REPL > " major-mode)
            (mapcar #'car repl-commands)
            :initial-input (symbol-name major-mode)
            :require-match t
            :action '(1
                      ("o" repl::ivy::start "Start for current buffer")
                      ("e" repl::ivy::edit  "Edit command")
                      ("E" repl::ivy::edit-and-run "Edit command and start")
                      ("s" repl::ivy::start-and-split "Start and split")
                      ("v" repl::ivy::start-and-vsplit "Start and vsplit"))))

(defun repl::ivy/running nil
  (interactive)
  (ivy-read "Running REPLs > "
            (cl-remove-if-not #'get-process repl-running)
            :require-match t
            :action '(1
                      ("o" repl::ivy::split "Split")
                      ("s" repl::ivy::split "Split")
                      ("v" repl::ivy::vsplit "Vsplit")
                      ("k" repl::ivy::kill "Kill")
                      ("i" repl::ivy::send-string "Send string"))))

(defun repl/send-sexp nil
  (interactive)
  (repl::send 'sexp))

(defun repl/send-defun nil
  (interactive)
  (repl::send 'defun))

(defun repl/start nil
  (interactive)
  (repl::start-process))

(defun repl/kill nil
  (interactive)
  (repl::kill-process))

(defun repl/hide nil
  (interactive)
  (repl::hide))

(defun repl/split nil
  (interactive)
  (repl::show :s))

(defun repl/vsplit nil
  (interactive)
  (repl::show :v))

(defun repl/send-region nil
  (interactive)
  (repl::send :region))

(defun repl/send-line nil
  (interactive)
  (repl::send :line))

(defun repl/send-buffer nil
  (interactive)
  (repl::send :buffer))

(defun repl/send-till-point nil
  (interactive)
  (repl::send :till-point))

(defun repl/send-eof nil
  (interactive)
  (repl::send 'eof))

(defun repl/send-string nil
  (interactive)
  (if (repl::live?)
      (let* ((prompt (format "(%s) > " major-mode))
             (s (read-from-minibuffer prompt)))
        (when (> (length s) 0)
          (repl::send s)))))

(defun repl/shell-start nil
  (interactive)
  (repl::start-process 'sh-mode))

(defun repl/shell-kill nil
  (interactive)
  (repl::kill-process 'sh-mode))

(defun repl/shell-hide nil
  (interactive)
  (repl::hide 'sh-mode))

(defun repl/shell-split nil
  (interactive)
  (repl::show :s 'sh-mode))

(defun repl/shell-vsplit nil
  (interactive)
  (repl::show :v 'sh-mode))

(defun repl/shell-send-region nil
  (interactive)
  (repl::send :region 'sh-mode))

(defun repl/shell-send-line nil
  (interactive)
  (repl::send :line 'sh-mode))

(defun repl/shell-send-buffer nil
  (interactive)
  (repl::send :buffer 'sh-mode))

(defun repl/shell-send-till-point nil
  (interactive)
  (repl::send :till-point 'sh-mode))

(defun repl/shell-send-eof nil
  (interactive)
  (repl::send 'eof 'sh-mode))

(defun repl/shell-send-string nil
  (interactive)
  (if (repl::live?)
      (let* ((prompt (format "(%s) > " major-mode))
             (s (read-from-minibuffer prompt)))
        (when (> (length s) 0)
          (repl::send s 'sh-mode)))))

(defun repl/shell-send-sexp nil
  (interactive)
  (repl::send 'sexp 'sh-mode))

(defun repl/shell-send-sexp nil
  (interactive)
  (repl::send 'defun 'sh-mode))

(defun repl/turn-on-evil-keybindings nil
  (interactive)
  (map! :mode repl-shell-mode-map :leader :n "r&" #'repl/shell-start)
  (map! :mode repl-shell-mode-map :leader :n "rQ" #'repl/shell-kill)
  (map! :mode repl-shell-mode-map :leader :n "rS" #'repl/shell-split)
  (map! :mode repl-shell-mode-map :leader :n "rV" #'repl/shell-vsplit)
  (map! :mode repl-shell-mode-map :leader :n "rK" #'repl/shell-hide)
  (map! :mode repl-shell-mode-map :leader :n "rL" #'repl/shell-send-line)
  (map! :mode repl-shell-mode-map :leader :v "rR" #'repl/shell-send-region)
  (map! :mode repl-shell-mode-map :leader :n "rB" #'repl/shell-send-buffer)
  (map! :mode repl-shell-mode-map :leader :n "r>" #'repl/shell-send-till-point)
  (map! :mode repl-shell-mode-map :leader :n "rI" #'repl/shell-send-string)
  (map! :mode repl-shell-mode-map :leader :n "rC" #'repl/shell-send-eof)
  (map! :mode repl-shell-mode-map :leader :n "rE" #'repl/shell-send-sexp)
  (map! :mode repl-shell-mode-map :leader :n "rD" #'repl/shell-send-defun)
  (map! :mode repl-mode-map :leader :n "r!" #'repl/start)
  (map! :mode repl-mode-map :leader :n "rq" #'repl/kill)
  (map! :mode repl-mode-map :leader :n "rs" #'repl/split)
  (map! :mode repl-mode-map :leader :n "rv" #'repl/vsplit)
  (map! :mode repl-mode-map :leader :n "rk" #'repl/hide)
  (map! :mode repl-mode-map :leader :n "rl" #'repl/send-line)
  (map! :mode repl-mode-map :leader :v "rr" #'repl/send-region)
  (map! :mode repl-mode-map :leader :n "rb" #'repl/send-buffer)
  (map! :mode repl-mode-map :leader :n "r." #'repl/send-till-point)
  (map! :mode repl-mode-map :leader :n "ri" #'repl/send-string)
  (map! :mode repl-mode-map :leader :n "rc" #'repl/send-eof)
  (map! :mode repl-mode-map :leader :n "rd" #'repl/send-defun)
  (map! :mode repl-mode-map :leader :n "re" #'repl/send-sexp)
  (map! :mode repl-mode-map :leader :n "r," #'repl::ivy/running)
  (map! :mode repl-mode-map :leader :n "r`" #'repl::ivy/start))

(defun repl/turn-off-evil-keybindings nil
  (interactive)
  (map! :mode repl-shell-mode-map :leader :n "r&" nil)
  (map! :mode repl-shell-mode-map :leader :n "rQ" nil)
  (map! :mode repl-shell-mode-map :leader :n "rS" nil)
  (map! :mode repl-shell-mode-map :leader :n "rV" nil)
  (map! :mode repl-shell-mode-map :leader :n "rK" nil)
  (map! :mode repl-shell-mode-map :leader :n "rL" nil)
  (map! :mode repl-shell-mode-map :leader :v "rR" nil)
  (map! :mode repl-shell-mode-map :leader :n "rB" nil)
  (map! :mode repl-shell-mode-map :leader :n "r>" nil)
  (map! :mode repl-shell-mode-map :leader :n "rI" nil)
  (map! :mode repl-shell-mode-map :leader :n "rC" nil)
  (map! :mode repl-shell-mode-map :leader :n "rE" nil)
  (map! :mode repl-shell-mode-map :leader :n "rD" nil)
  (map! :mode repl-mode-map :leader :n "r!" nil)
  (map! :mode repl-mode-map :leader :n "rq" nil)
  (map! :mode repl-mode-map :leader :n "rs" nil)
  (map! :mode repl-mode-map :leader :n "rv" nil)
  (map! :mode repl-mode-map :leader :n "rk" nil)
  (map! :mode repl-mode-map :leader :n "rl" nil)
  (map! :mode repl-mode-map :leader :v "rr" nil)
  (map! :mode repl-mode-map :leader :n "rb" nil)
  (map! :mode repl-mode-map :leader :n "r." nil)
  (map! :mode repl-mode-map :leader :n "ri" nil)
  (map! :mode repl-mode-map :leader :n "rc" nil)
  (map! :mode repl-mode-map :leader :n "rd" nil)
  (map! :mode repl-mode-map :leader :n "re" nil)
  (map! :mode repl-mode-map :leader :n "r," nil)
  (map! :mode repl-mode-map :leader :n "r`" nil))

(defvar repl-shell-mode-map (let ((map (make-sparse-keymap)))
                              (define-key map (kbd "Q") #'repl/shell-kill)
                              (define-key map (kbd "K") #'repl/shell-hide)
                              (define-key map (kbd "L") #'repl/shell-send-line)
                              (define-key map (kbd "R") #'repl/shell-send-region)
                              (define-key map (kbd "B") #'repl/shell-send-buffer)
                              (define-key map (kbd "D") #'repl/shell-send-defun)
                              (define-key map (kbd ">") #'repl/shell-send-till-point)
                              (define-key map (kbd "S") #'repl/shell-split)
                              (define-key map (kbd "V") #'repl/shell-vsplit)
                              map))

(defvar repl-mode-map (let ((map (make-sparse-keymap)))
                        (define-key map (kbd "q") #'repl/kill)
                        (define-key map (kbd "k") #'repl/hide)
                        (define-key map (kbd "l") #'repl/send-line)
                        (define-key map (kbd "r") #'repl/send-region)
                        (define-key map (kbd "b") #'repl/send-buffer)
                        (define-key map (kbd "d") #'repl/send-defun)
                        (define-key map (kbd ".") #'repl/send-till-point)
                        (define-key map (kbd "s") #'repl/split)
                        (define-key map (kbd "v") #'repl/vsplit)
                        map))

(define-minor-mode repl-mode
  "Basic REPL extension for emacs"
  :lighter " repl"
  :keymap repl-mode-map
  (if (and repl-mode (repl::get :cmd))
      (repl/start)
    (progn (setq repl-mode nil)
           (message "No command defined for major %s" major-mode))))

(define-minor-mode repl-shell-mode
  "Basic REPL extension for emacs"
  :lighter " repl-shell"
  :keymap repl-shell-mode-map
  (if repl-shell-mode
      (repl/shell-start)
    (setq repl-shell-mode nil)))

; LISP Syntax
