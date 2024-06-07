;;; x4-funs.el --- My custom functions.	-*- lexical-binding: t -*-

(require 'cl-lib)
(require 'dash)
(require 's)

;;; clear function for eshell
(defun eshell/clear ()
  "Clear the eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

;; TODO: check if this can be replaced by a crux package
;; TODO: why there are two functions? and one has commented-out diff fun name?
(defun prelude-swap-windows ()
  ;;(defun rotate-windows ()
  "Rotate your windows"
  (interactive)
  (cond
   ((not (> (count-windows) 1))
    (message "You can't rotate a single window!"))
   (t
    (let ((num-windows (count-windows))
          (i (count-windows)))
      (while  (> i 0)
        (let* ((w1 (elt (window-list) i))
               (w2 (elt (window-list) (- (% i num-windows) 1)))
               (b1 (window-buffer w1))
               (b2 (window-buffer w2))
               (s1 (window-start w1))
               (s2 (window-start w2)))
          (set-window-buffer w1 b2)
          (set-window-buffer w2 b1)
          (set-window-start w1 s2)
          (set-window-start w2 s1)
          (setq i (1- i))))))))

(defun prelude-swap-windows ()
  "Rotate your windows"
  (interactive)
  (cond
   ((not (> (count-windows) 1))
    (message "You can't rotate a single window!"))
   (t
    (let ((i 1)
          (num-windows (count-windows)))
      (while  (< i num-windows)
        (let* ((w1 (elt (window-list) i))
               (w2 (elt (window-list) (+ (% i num-windows) 1)))
               (b1 (window-buffer w1))
               (b2 (window-buffer w2))
               (s1 (window-start w1))
               (s2 (window-start w2)))
          (set-window-buffer w1 b2)
          (set-window-buffer w2 b1)
          (set-window-start w1 s2)
          (set-window-start w2 s1)
          (setq i (1+ i))))))))

(defun x4/yank-primary-at-point ()
  "Yank from primary selection at point"
  (interactive "^")
  (let ((primary
         (cond
          ((eq system-type 'windows-nt)
           ;; MS-Windows emulates PRIMARY in x-get-selection, but not
           ;; in x-get-selection-value (the latter only accesses the
           ;; clipboard).  So try PRIMARY first, in case they selected
           ;; something with the mouse in the current Emacs session.
           (or (x-get-selection 'PRIMARY)
               (x-get-selection-value)))
          ((fboundp 'x-get-selection-value) ; MS-DOS and X.
           ;; On X, x-get-selection-value supports more formats and
           ;; encodings, so use it in preference to x-get-selection.
           (or (x-get-selection-value)
               (x-get-selection 'PRIMARY)))
          ;; FIXME: What about xterm-mouse-mode etc.?
          (t
           (x-get-selection 'PRIMARY)))))
    (unless primary
      (error "No selection is available"))
    (push-mark (point))
    (insert primary)))

(defun x4/comment-line ()
  "Comments line."
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))


(defun x4/delete-this-file-and-buffer ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(defun x4/js-eval-region ()
  "Evaluate region in JS."
  (interactive)
  (js-eval (buffer-substring (mark) (point)))
  )

(defun x4/close-and-kill-next-pane ()
  "Close the other pane and kill the buffer in it also."
  (interactive)
  (other-window 1)
  (if (kill-buffer)
      (delete-window)
    (other-window -1)
    ))

(defun x4/close-and-kill-this-pane ()
  "Close this pane and kill the buffer in it also."
  (interactive)
  (if (kill-buffer)
      (delete-window)
    nil
    ))

(defun markdown-toc/--to-link (title)
  "Given a TITLE, return the markdown link associated."
  (format "[%s](#%s)" title
          (->>
           title
           ;;downcase
           (s-replace " " "_")
           ;;(replace-regexp-in-string "[^a-z0-9 -]" "-")
           (replace-regexp-in-string "_+" "_")
           )))

(defun markdown-toc/--compute-full-toc (toc)
  "Given the TOC's content, compute the full toc with comments and title."
  (format "%s\n\n%s\n\n%s\n\n%s\n"
          *markdown-toc/header-toc-start*
          *markdown-toc/header-toc-title*
          toc
          *markdown-toc/header-toc-end*))

(defun x4/yank-replace-line ()
  "Yanks killed content replacing entire line"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((beg (point))) (forward-line 1) (delete-region beg (point)))
    (yank)
    )
  )

(defun x4/switch-to-last-window ()
  "Switch to previously selected window.
  Repeated invocations toggle between the two most recently selected windows."
  (interactive)
  ;;(print (get-mru-window nil nil t))
  (if-let (last-window (get-mru-window nil nil t))
      (select-window last-window)))



(defun x4/sugest-elixir-module-name ()
  (let* ((lib-path (concat (project-root (project-current)) "lib/"))
         (relative-path (file-relative-name buffer-file-name lib-path)))
    (alchemist-utils-path-to-module-name relative-path)
    )
  )

(defun x4/project-named-p (name)
  "Check if the current project is named NAME."
  (string= (project-name (project-current)) name))

(provide 'x4-funs)
