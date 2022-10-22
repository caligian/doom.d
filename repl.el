;;; test.*el -*- lexical-binding: t; -*-
(provide 'repl)
(require 'general)

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

(map! :leader :n "r&" #'repl/shell-start)
(map! :leader :n "rQ" #'repl/shell-kill)
(map! :leader :n "rS" #'repl/shell-split)
(map! :leader :n "rV" #'repl/shell-vsplit)
(map! :leader :n "rK" #'repl/shell-hide)
(map! :leader :n "rL" #'repl/shell-send-line)
(map! :leader :v "rR" #'repl/shell-send-region)
(map! :leader :n "rB" #'repl/shell-send-buffer)
(map! :leader :n "r>" #'repl/shell-send-till-point)
(map! :leader :n "rI" #'repl/shell-send-string)
(map! :leader :n "rC" #'repl/shell-send-eof)
(map! :leader :n "r!" #'repl/start)
(map! :leader :n "rq" #'repl/kill)
(map! :leader :n "rs" #'repl/split)
(map! :leader :n "rv" #'repl/vsplit)
(map! :leader :n "rk" #'repl/hide)
(map! :leader :n "rl" #'repl/send-line)
(map! :leader :v "rr" #'repl/send-region)
(map! :leader :n "rb" #'repl/send-buffer)
(map! :leader :n "r." #'repl/send-till-point)
(map! :leader :n "ri" #'repl/send-string)
(map! :leader :n "rc" #'repl/send-eof)
(map! :leader :n "r," #'repl::ivy/running)
(map! :leader :n "r`" #'repl::ivy/start)
