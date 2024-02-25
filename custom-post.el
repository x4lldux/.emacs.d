;; custom-post.el --- Custom user configuration.	-*- lexical-binding: t -*-

;;; Commentary:
;; My fancy config 😎

;;; Code:

(defvar x4/personal-path "~/.emacs.d/personal/")
(defvar x4/personal-vendor-path (expand-file-name "vendor" x4/personal-path))
(defvar x4/personal-confidential-path (expand-file-name "confidential" x4/personal-path))
(defvar x4/var-path "~/.emacs.d/var/")

(add-to-list 'load-path x4/personal-path)
(add-to-list 'load-path x4/personal-vendor-path)
(add-to-list 'load-path x4/personal-confidential-path)

(require 'x4-funs)

;; TODO:
;; - make saving multiple-cursors allowed/disallowed funs work
;; - general open/find file/buffer in prot, recent files in smart order
;; - use adjust ordering in ripgrep, to be similar as in general find
;; - figure out completion how to trigger it for regular words (dictionary, last
;;   used words, symbols/keywword from buffer) (what is Dabbrev)
;; - go-to definition and get back doen't work reliably
;; - move other temp/var files to var directory
;; - ~~replace "RET" with "a" in dired (use dired-find-alternate-file as default)~~
;;   investigate if solution can be moved from init.el to here
;; - increase posframe height for ivy/counsel/swipper
;; - experiment with ivy-posframe transparency/alpha
;;   `(setq ivy-posframe-parameters '((alpha . 85)))`
;; - ~~yasnippet for elixir~~ probably works, check!
;; - toggleing between top and bottom position of an active posframe

;;; default editor congigs
(use-package emacs
  :config
  (toggle-frame-maximized)

  (when sys/macp
    (setq
     ns-command-modifier 'super
     ns-option-modifier 'meta
     ns-right-option-modifier 'none)

    ;; auto raise window on MacOS
    (progn
      (defun ns-raise-emacs ()
        "Raise Emacs."
        (ns-do-applescript "tell application \"Emacs\" to activate"))
      (defun ns-raise-emacs-with-frame (frame)
        "Raise Emacs and select the provided frame."
        (with-selected-frame frame
          (when (display-graphic-p)
            (ns-raise-emacs))))

      (add-hook 'after-make-frame-functions 'ns-raise-emacs-with-frame)

      (when (display-graphic-p)
        (ns-raise-emacs))))

  ;; Put backup files neatly away
  (let ((backup-dir (expand-file-name "backups" x4/var-path))
        (auto-saves-dir (expand-file-name "auto-saves" x4/var-path)))
    (dolist (dir (list backup-dir auto-saves-dir))
      (when (not (file-directory-p dir))
        (make-directory dir t)))
    (setq backup-directory-alist `(("." . ,backup-dir))
          auto-save-file-name-transforms `((".*" ,auto-saves-dir t))
          auto-save-list-file-prefix (concat auto-saves-dir ".saves-")
          tramp-backup-directory-alist `((".*" . ,backup-dir))
          tramp-auto-save-directory auto-saves-dir))
  (setq
   delete-by-moving-to-trash t         ; Deleting files go to OS's trash folder
   make-backup-files t                 ; Forbide to make backup files
   auto-save-default t                 ; Disable auto save
   set-mark-command-repeat-pop t       ; Repeating C-SPC after popping mark pops it again
   backup-by-copying t                 ; Don't delink hardlinks
   delete-old-versions t               ; ;Clean up the backups
   version-control t                   ; Use version numbers on backups,
   kept-new-versions 5                 ; keep some new versions
   kept-old-versions 2)                ; and some old ones, too

  ;; saner defaults
  (global-auto-revert-mode 1)

  (global-aggressive-indent-mode 0)
  (aggressive-indent-mode 0)

  (when (featurep 'company-mode)
    (global-company-mode 0)
    (company-mode 0))

  (setq
   global-auto-revert-non-file-buffers t ; revert dired and other buffers

   ;; moves point to the farthest possible position. If point is already there, the command signals an error.
   scroll-error-top-bottom 't
   set-mark-command-repeat-pop 't

   sentence-end-double-space nil   ; don't assume sentences have two spaces between them
   require-final-newline t         ; ensure new line at end of file
   frame-inhibit-implied-resize t  ; don't truncate frame size to text column size
   pixel-scroll-precision-mode t

   ;; TODO: tinker with split-window-sensibly-p, to prefer vertical splitting
   ;; over horizontal. For right now, disable horizontal splitting
   split-width-threshold 120
   split-height-threshold nil

   fill-column 80
   git-commit-fill-column 72

   ;; when C-h use Embark searching
   prefix-help-command #'embark-prefix-help-command

   ;; additional tree sitter grammars
   treesit-language-source-alist
   '(
     (nim "https://github.com/alaviss/tree-sitter-nim.git")
     (zig "https://github.com/maxxnino/tree-sitter-zig.git")
     )

   ispell-dictionary "american")

  )


;;; Config

(use-package dired
  :ensure nil
  :init
  (put 'dired-find-alternate-file 'disabled nil)
  :custom
  (dired-kill-when-opening-new-dired-buffer 't)

  :bind (:map dired-mode-map
         ([remap dired-find-file ] . dired-find-alternate-file)
         )
  )

;; auto save when buffer/frame looses focus (from Prelude)
(use-package super-save
  :delight
  :defer 1
  :config
  ;; add integration with ace-window
  (add-to-list 'super-save-triggers 'ace-window)
  ;; save on find-file
  (add-to-list 'super-save-hook-triggers 'find-file-hook)
  (super-save-mode +1))

;;: use-package extensions
(use-package use-package-chords
  :ensure t
  :demand t
  :custom
  (key-chord-one-key-delay 0.11)
  (key-chord-two-keys-delay 0.11)
  (key-chord-safety-interval-forward 0.0)
  :config (key-chord-mode 1))

(use-package major-mode-hydra
  :bind ("C-M-m" . major-mode-hydra)
  :config
  (setq major-mode-hydra-title-generator
      #'(lambda (mode)
         (s-concat "\n"
                   (s-repeat 10 " ")
                   (all-the-icons-icon-for-mode mode :v-adjust 0.05)
                   " "
                   (symbol-name mode)
                   " commands")))
  )

(use-package pretty-hydra
  )

;;; Some misc keyboard shortcuts

(key-chord-define-global "hn" 'diff-hl-next-hunk)
(key-chord-define-global "hp" 'diff-hl-previous-hunk)
(key-chord-define-global "uu" 'vundo)

(key-chord-define-global "jj" 'avy-goto-char-timer)
;; (key-chord-define-global "jf" 'avy-goto-char)
(key-chord-define-global "jn" 'avy-goto-char-in-line)
;; (key-chord-define-global "jj" '(lambda ()
;;                                  (interactive)
;;                                  (avy-goto-char-2)
;;                                  (beacon-blink))
;;                                  )


;; select new window after splitting
(global-set-key "\C-x2" (lambda ()
                          (interactive)
                          (split-window-below)
                          (other-window 1)))
(global-set-key "\C-x3" (lambda ()
                          (interactive)
                          (split-window-right)
                          (other-window 1)))

(global-set-key (kbd ",") (lambda() (interactive) (insert ", ")))
(global-set-key (kbd ":")
                (lambda()
                  (interactive)
                  (if (and (derived-mode-p 'elixir-mode)
                           (string-match-p "[a-zA-Z0-9\"']" (string (preceding-char))))
                      (insert ": ")
                    (insert ":"))
                  ))

(global-set-key (kbd "C-c D") 'x4/delete-this-file-and-buffer)
(global-set-key (kbd "C-M-y") 'x4/yank-replace-line)
(global-set-key (kbd "C-M-;") 'x4/comment-line)

;; (global-set-key [home] 'x4/smarter-beginning-of-line)
;; (global-set-key "\C-a" 'x4/smarter-beginning-of-line)
(global-set-key [(shift insert)] 'x4/yank-primary-at-point)

;;; close and kill buffer
(global-set-key (kbd "C-x C-k C-k") 'x4/close-and-kill-this-pane)
(global-set-key (kbd "C-x C-M-k C-M-k") 'x4/close-and-kill-next-pane)


;; TODO: document potential use cases of this...
(global-set-key (kbd "C-c C-r") (defun x4/excursion-edit ()
                                  (interactive)
                                  (save-window-excursion
                                    (save-mark-and-excursion
                                      (recursive-edit)))))

(global-set-key (kbd "C-c .") 'lsp-ui-doc-glance)

;; TODO: doesn't work on Mac with my version of PL-Lefty
;; Map alt_gr+homerow to numbers
(define-key key-translation-map (kbd "C-ą")   (kbd "1"))
(define-key key-translation-map (kbd "C-M-ą") (kbd "M-1"))
(define-key key-translation-map (kbd "C-ś")   (kbd "2"))
(define-key key-translation-map (kbd "C-M-ś") (kbd "M-2"))
(define-key key-translation-map (kbd "C-ð")   (kbd "3"))
(define-key key-translation-map (kbd "C-M-ð") (kbd "M-3"))
(define-key key-translation-map (kbd "C-æ")   (kbd "4"))
(define-key key-translation-map (kbd "C-M-æ") (kbd "M-4"))
(define-key key-translation-map (kbd "C-ŋ")   (kbd "5"))
(define-key key-translation-map (kbd "C-M-ŋ") (kbd "M-5"))
(define-key key-translation-map (kbd "C-’")   (kbd "6"))
(define-key key-translation-map (kbd "C-M-’") (kbd "M-6"))
(define-key key-translation-map (kbd "C-ə")  (kbd "7"))
(define-key key-translation-map (kbd "C-M-ə") (kbd "M-7"))
(define-key key-translation-map (kbd "C-…")   (kbd "8"))
(define-key key-translation-map (kbd "C-M-…") (kbd "M-8"))
(define-key key-translation-map (kbd "C-ł")   (kbd "9"))
(define-key key-translation-map (kbd "C-M-ł") (kbd "M-9"))

;; special case for 0
;; needs to be double press because of i18n
(define-key key-translation-map (kbd "C-´")   (kbd "0"))
(define-key key-translation-map (kbd "C-M-´") (kbd "M-0"))
;; (define-key key-translation-map [(control dead-acute)] (kbd "0"))
;; (define-key key-translation-map [(control meta dead-acute)] (kbd "M-0"))

;; TODO: what special case is this?
;; (define-key key-translation-map (kbd "C-^")  (kbd "-"))
;; (define-key key-translation-map (kbd "C-M-^")  (kbd "M--"))

;; (define-key key-translation-map [(control dead-circumflex)] (kbd "-"))
;; (define-key key-translation-map [(control meta dead-circumflex)] (kbd "M--"))


;;; Scroll by a single line
(global-set-key (kbd "C-M-<down>")  #'(lambda () (interactive)
                                       (let ((pos (point)))
                                         (scroll-down-line)
                                         (set-window-point (selected-window) pos)
                                         )))
(global-set-key (kbd "C-M-<up>")  #'(lambda () (interactive)
                                     (let ((pos (point)))
                                       (scroll-up-line)
                                       (set-window-point (selected-window) pos)
                                       )))


;; Drag stuff (lines, words, region, etc...) around
(use-package drag-stuff
  :diminish
  :autoload drag-stuff-define-keys
  :hook (after-init . drag-stuff-global-mode)
  :config
  (add-to-list 'drag-stuff-except-modes 'org-mode)
  (drag-stuff-define-keys))

;;; Completion
;; TODO: find out what limits in completion-at-point-functions. when I type
;; "pro" in an elixir buffer with lsp, and there is an atom :process it doesnt
;; complete it
;; TODO: make symbols (atoms & maybe text) from a buffer be part of completion candidates
(use-package vertico
  :custom
  (vertico-cycle 't)
  :custom
  ((vertico-posframe-parameters '((left-fringe . 8) (right-fringe . 8) (alpha . 90))))
  )

(use-package consult
  :custom
  ((consult-preview-key 'any)))

;;; Golden ratio
(use-package golden-ratio
  :init
  (setq golden-ratio-adjust-factor .8
        golden-ratio-wide-adjust-factor .62)
  :bind ("C-c C-g" . golden-ratio)
  :config (progn
            (setq window-combination-resize 't)
            (defadvice x4/-golden-ratio-after-other-window (after other-window)
              "Use Golden Ratio to resize windows."
              (golden-ratio))
            ;;(ad-enable-advice 'other-window 'after 'x4/-golden-ratio-after-other-window)
            ;;(ad-activate 'x4/-golden-ratio-after-other-window)
            ))

;; Completion configuration
(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-quit-at-boundary 'separator)
  (corfu-preview-current nil)
  :bind (:map corfu-map
         ("M-SPC" . corfu-insert-separator))
  :init
  (global-corfu-mode 1)
  (corfu-history-mode 1)
  (corfu-indexed-mode 1)

  :config
  ;; enable corfu in minibuffer (when not handled by vertico)
  (defun x4/-corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer."
    (when (local-variable-p 'completion-at-point-functions)
      ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
      (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                  corfu-popupinfo-delay nil)
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'x4/-corfu-enable-in-minibuffer)

  ;; transfer completion to minibuffer for richer control
  (defun x4/-corfu-move-to-minibuffer ()
    (interactive)
    (pcase completion-in-region--data
      (`(,beg ,end ,table ,pred ,extras)
       (let ((completion-extra-properties extras)
             completion-cycle-threshold completion-cycling)
         (consult-completion-in-region beg end table pred)))))

  (keymap-set corfu-map "M-m" #'x4/-corfu-move-to-minibuffer)
  (add-to-list 'corfu-continue-commands #'x4/-corfu-move-to-minibuffer)
  )
;; TODO: experiment with cape
;; TODO: add company as a backend via cape-company-to-capf
;; TODO: add corfu-terminal

;;; LSP
(use-package lsp-mode
  :custom
  (lsp-completion-provider :none) ;; we use Corfu!
  :init
  (setq lsp-keymap-prefix "C-c C-l")
  (defun x4/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(flex))) ;; Configure flex
  :config
  ;; syncing is shuting down LSPs when changing workspaces/tabs
  (lsp-treemacs-sync-mode -1)

  :hook ((elixir-mode . lsp)
         (lsp-completion-mode . x4/lsp-mode-setup-completion))

  :bind
  (("M-<down-mouse-1>" . lsp-find-definition-mouse)
   ("C-c l" . lsp-mode-hydra/body))

  :pretty-hydra
  ((:title (pretty-hydra-title "LSP" 'mdicon "nf-md-rocket_launch")
    :color amaranth
    :exit t :quit-key ("q" "C-g" "RET"))
   ("Formatting"
    (("=" lsp-format-buffer "buffer"))
    ;; (("= =" lsp-format-buffer "buffer")
     ;; ("= r" lsp-format-region "region"))
    "Folders"
    (("F a" lsp-workspace-folders-add "add")
     ("F b" lsp-workspace-blocklist-remove "un-blocklist")
     ("F r" lsp-workspace-folders-remove "remove")
     )
    "Peek"
    (
     ("G g" lsp-ui-peek-find-definitions "definitions")
     ("G i" lsp-ui-peek-find-implementation "implementation")
     ("G r" lsp-find-references "reference")
     ("G s" lsp-ui-peek-find-workspace-symbol "workspace symbol")
     )
    ;; "Toggle"
    ;; (
    ;;  )
    ;; "Code actions"
    ;; (
    ;;  )
    "Goto"
    (("g a" xref-find-apropos "find symbol in workspace")
     ("g d" lsp-find-declaration "find declarations")
     ("g s" lsp-treemacs-symbols "TM show symbols") ;
     ("g e" lsp-treemacs-errors-list "TM show errors") ;
     ("g g" lsp-find-definition "find definitions") ;
     ("g h" lsp-treemacs-call-hierarchy "TM call hierarchy") ;
     ("g i" lsp-find-implementation "find implementations") ;
     ("g r" lsp-find-references "find references")
     ("g t" lsp-find-type-definition "find type definition")
     )
    "Help"
    (("h g" lsp-ui-doc-glance "glance symbol")
     ("h h" lsp-describe-thing-at-point "describe symbol")
     ("h s" lsp-signature-activate "signature help")
     )
    ;; "Refactor"
    ;; (
    ;;  )
    ;; "Workspaces"
    ;; (
    ;;  )
    ))
  )

;;; buffer-move
(use-package buffer-move
  :commands (buf-move-down buf-move-up buf-move-left buf-move-right)
  :bind (("C-S-M-<down>" . (lambda () (interactive)
                             (let ((this-win (selected-window)))
                               (buf-move-down)
                               (select-window this-win)
                               )))
         ("C-S-M-<up>" . (lambda () (interactive)
                           (let ((this-win (selected-window)))
                             (buf-move-up)
                             (select-window this-win)
                             )))
         ("C-S-M-<left>" . (lambda () (interactive)
                             (let (this-win (selected-window))
                               (let ((this-win (selected-window)))
                                 (select-window this-win)
                                 ))))
         ("C-S-M-<right>" . (lambda () (interactive)
                              (let ((this-win (selected-window)))
                                (buf-move-right)
                                (select-window this-win)
                                )))))

;;; Fill column indicator (emacs' built-in)
(use-package display-fill-column-indicator
  :defer 1
  :custom-face (fill-column-indicator ((t (:foreground "orange1"))))
  ;; :hook ((prog-mode text-mode markdown-mode) . display-fill-column-indicator)
  :config
  (global-display-fill-column-indicator-mode 1)
  )

;;; color-identifiers-mode
(use-package color-identifiers-mode
  :ensure t
  :defer 1
  :config
  ;; this are regular variables, NOT customizable variables
  (setq color-identifiers-coloring-method 'hash
        color-identifiers:min-color-saturation 0.333
        color-identifiers:color-luminance 0.7
        color-identifiers:num-colors 27)
  (add-to-list
   'color-identifiers:modes-alist
   `(elixir-mode . ("[[:space:]]*"
                    ;; "\\_<:?\\([a-z]\\(?:[a-zA-Z0-9_]\\)*\\|[A-Z]\\(?:[a-zA-Z0-9_\.]\\)*\\)"
                    "\\_<[:=]?\\([a-zA-z]\\(?:[[:alnum:]_]\\)*[!?]*\\)"
                    (nil elixir-atom-face font-lock-function-name-face
                         font-lock-variable-name-face font-lock-type-face))))
  (add-to-list
   'color-identifiers:modes-alist
   `(elixir-ts-mode . ("[[:space:]]*"
                       ;; "\\_<:?\\([a-z]\\(?:[a-zA-Z0-9_]\\)*\\|[A-Z]\\(?:[a-zA-Z0-9_\.]\\)*\\)"
                       "\\_<[:=]?\\([a-zA-z]\\(?:[[:alnum:]_]\\)*[!?]*\\)"
                       (nil elixir-atom-face font-lock-function-name-face
                            font-lock-variable-name-face font-lock-type-face))))
  ;; Works only for specific colors generated for 27 of different colors
  (defun color-identifiers:hash-identifier (identifier)
    "Return a color for IDENTIFIER based on its hash."
    (elt color-identifiers:colors (cond
                                   ((equal identifier "Logger") 0)
                                   ((equal identifier "ok") 6)
                                   ((equal identifier "err") 10)
                                   ((equal identifier "error") 10)
                                   ((equal identifier "alert") 23)
                                   ((equal identifier "critical") 23)
                                   ((equal identifier "warn") 2)
                                   ((equal identifier "warning") 2)
                                   ((equal identifier "true") 25)
                                   ((equal identifier "false") 1)
                                   ((equal identifier "nil") 21)
                                   (t (% (abs (sxhash identifier)) color-identifiers:num-colors)))
         ))
  (global-color-identifiers-mode +1))

;;; transpose-frame
(use-package transpose-frame
  :bind (
         ("C-<f11>" . rotate-frame-anticlockwise)
         ("C-<f12>" . rotate-frame-clockwise))
  )


(use-package move-text
  :ensure t
  :bind (("C-S-<up>" . move-text-up)
         ("C-S-<down>" . move-text-down)))

;;; Multiple cursors
(unload-feature 'multiple-cursors 't)   ;unload whatever was set by Centaur
(use-package multiple-cursors
  :custom
  (mc/list-file (expand-file-name "mc-lists.el" x4/var-path))
  :config
  (delete-selection-mode 1)
  ;; Centaur uses smart-region, which doesn't work with MC - disable it
  (smart-region-off)

  :bind
  (("C-c m" . multiple-cursors-hydra/body))) ; defined by centaur

;; Move to the beginning/end of line or code
(use-package mwim
  :bind (([remap move-beginning-of-line] . mwim-beginning)
         ([remap move-end-of-line] . mwim-end)))

;;; curx from prelude (use-package crux
(use-package crux
  :demand t
  :config
  (crux-with-region-or-line kill-region)
  :bind (("M-o" . crux-smart-open-line)
         ("S-<return>" . crux-smart-open-line)
         ("C-S-<return>" . crux-smart-open-line-above)
         ("C-c d" . crux-duplicate-current-line-or-region)
         ("C-c M-d" . crux-duplicate-and-comment-current-line-or-region)
         ("C-^" . crux-top-join-line)
         ("C-<backspace>" . crux-kill-line-backwards)
         ("C-S-<backspace>" . crux-kill-whole-line) ; [remap kill-whol-line]
         ("C-c M-r" . crux-rename-buffer-and-file)
         ))


;;; Kill & Mark things easily
;; M-w w: save word at point
;; M-w s: save sexp at point
;; M-w l: save list at point (enclosing sexp)
;; M-w d: save defun at point
;; M-w D: save current defun name
;; M-w f: save file at point
;; M-w b: save buffer-file-name or default-directory. - changes the kill to the directory name, + to full name and 0 to basename.
;; TODO: investigage other extra markings

(use-package easy-kill
  :bind (([remap kill-ring-save] . easy-kill)
         ([remap mark-sexp] . easy-mark)))

(use-package easy-kill-extras
  :defer 1
  :after easy-kill
  :config
  (require 'extra-things)

  ;; add `quoted-pair-content' - copies inside of a string
  (define-quoted-string-thing quoted-string nil)
  (add-to-list 'easy-kill-alist '(?q  quoted-pair-content "\n") t)
  (add-to-list 'easy-kill-alist '(?d  dupa-pair-content "\n") t)

  ;; example settings
  (add-to-list 'easy-kill-alist '(?W  WORD " ") t)
  (add-to-list 'easy-kill-alist '(?\' squoted-string "") t)
  (add-to-list 'easy-kill-alist '(?\" dquoted-string "") t)
  (add-to-list 'easy-kill-alist '(?\` bquoted-string "") t)
  (add-to-list 'easy-kill-alist '(?q  quoted-string "") t)
  (add-to-list 'easy-kill-alist '(?Q  quoted-string-universal "") t)
  (add-to-list 'easy-kill-alist '(?\) parentheses-pair-content "\n") t)
  (add-to-list 'easy-kill-alist '(?\( parentheses-pair "\n") t)
  (add-to-list 'easy-kill-alist '(?\] brackets-pair-content "\n") t)
  (add-to-list 'easy-kill-alist '(?\[ brackets-pair "\n") t)
  (add-to-list 'easy-kill-alist '(?}  curlies-pair-content "\n") t)
  (add-to-list 'easy-kill-alist '(?{  curlies-pair "\n") t)
  (add-to-list 'easy-kill-alist '(?>  angles-pair-content "\n") t)
  (add-to-list 'easy-kill-alist '(?<  angles-pair "\n") t)
  )


;;; magit
(use-package magit
  :ensure t
  :custom
  (magit-diff-refine-hunk 'all)
  (magit-log-arguments  '("--graph" "--color" "--decorate" "-n256"))
  (magit-log-section-arguments '("--graph" "--color" "--decorate" "-n256"))
  (magit-section-initial-visibility-alist '((stashes . hide) (todos . show) (unpushed . show)))
  (magit-status-margin '(t age magit-log-margin-width nil 18))

  :config
  ;; highlight conventional-commits as keywords
  (defun magit-log-propertize-keywords (_rev msg)
    (let ((boundary 0))
      (when (string-match "^\\(?:squash\\|fixup\\)! " msg boundary)
        (setq boundary (match-end 0))
        (magit--put-face (match-beginning 0) (1- boundary)
                         'magit-keyword-squash msg))
      (when magit-log-highlight-keywords
        (while (string-match "\\[[^[]*?]\\|^\\(?:.+\\)\\(?:([^()]+)\\)?: " msg boundary)
          (setq boundary (match-end 0))
          (magit--put-face (match-beginning 0) boundary
                           'magit-keyword msg))))
    msg)

  (defun x4/-extract-notion-id-from-branch (&optional branch-name)
    "Returns Notion ID extracted from branch named BRANCH-NAME (or
     current branch in non passed)."
    (let ((branch-name (or branch-name (magit-get-current-branch)))
          (TICKET-PATTERN ".*\\b[Hh][Ss][-_ ]?\\([[:digit:]]+\\)\\b.*"))
      (when (string-match-p TICKET-PATTERN branch-name)
            (s-upcase (replace-regexp-in-string TICKET-PATTERN "HS-\\1: " branch-name))
        )))

  (defun x4/insert-notion-id-from-current-branch ()
    "Inserts Notion ID extracted form curent branch name."
    (interactive)
    (when-let (id (x4/-extract-notion-id-from-branch))
      (insert id)))

  (defun x4/-first-commit-in-branch-p ()
    "Returns 't if this is the first commit in the branch."
    (with-temp-buffer
      (magit--git-insert nil "reflog" "--format=%h" (magit-get-current-branch))
      (delete-duplicate-lines (point-min) (point-max))
      (eq (count-words (point-min) (point-max)) 1) ; only branch created commit should be present
      ))

  (defun x4/git-commit-insert-default-template ()
    (cond
     ((x4/-first-commit-in-branch-p)
        (x4/insert-notion-id-from-current-branch))
     ('t (insert "type"))
     ))

  :hook (git-commit-setup . x4/git-commit-insert-default-template)
  :bind (:map magit-mode-map
         ("M-1" . magit-section-show-level-1-all)
         ("M-2" . magit-section-show-level-2-all)
         ("M-3" . magit-section-show-level-3-all)
         ("M-4" . magit-section-show-level-4-all)
         )
  )


;;;; transient
(use-package transient
  :ensure t
  :custom
  (transient-levels-file (expand-file-name "transient/levels.el" x4/var-path))
  (transient-values-file (expand-file-name "transient/values.el" x4/var-path))
  (transient-history-file (expand-file-name "transient/history.el" x4/var-path))

  (transient-detect-key-conflicts 't)
  ;; (transient-display-buffer-action '(display-buffer-below-selected))
  (transient-enable-popup-navigation 't)
  (transient-highlight-mismatched-keys 't)
  (transient-mode-line-format '("%e" mode-line-front-space mode-line-buffer-identification))
  (transient-semantic-coloring 't)
  ;; :custom-face
  ;; (transient-blue ((t (:inherit transient-key :foreground "deep sky blue"))))
  ;; (transient-mismatched-key ((t (:background "dim gray" :underline nil))))
  ;; (transient-nonstandard-key ((t (:background "dim gray"))))
  ;; (transient-argument ((t (:inherit font-lock-variable-name-face))))
  ;; (transient-value ((t (:inherit font-lock-variable-name-face))))

  :config
  (transient-bind-q-to-quit))

;;;; magit-todos
(use-package magit-todos
  :commands (magit-todos-mode)
  :bind ("C-c C-t" . ivy-magit-todos)
  :after (magit)
  :hook ((magit-mode . magit-todos-mode)
         (after-save . magit-after-save-refresh-status))
  :custom
  (magit-todos-auto-group-items 'never)
  :init
  (setq magit-todos-nice (if (executable-find "nice") t nil))
  (setq magit-todos-scanner #'magit-todos--scan-with-git-grep)
  (let ((inhibit-message t))
    (magit-todos-mode 1))
  :config
  (with-eval-after-load 'magit-status
    (transient-append-suffix 'magit-status-jump '(0 0 -1)
      '("t " "Todos" magit-todos-jump-to-todos))))


;;; zoom-window
(use-package zoom-window
  :bind ("C-x C-z" . zoom-window-zoom)
  :custom
  (zoom-window-mode-line-color "DarkGreen")
  ;; (zoom-window-use-persp t)
  :hook (find-file . (lambda () (setq my-solaire-mode solaire-mode)))
  :init
  (zoom-window-setup)
  (defvar-local my-solaire-mode nil)
  (advice-add #'zoom-window-zoom :before #'turn-off-solaire-mode)
  (advice-add #'zoom-window--restore-mode-line-face :after
              (lambda ()
                (if my-solaire-mode
                    (turn-on-solaire-mode)
                  (turn-off-solaire-mode)))))


;;; beacon
(use-package beacon
  :defer 1
  :config
  (setq beacon-blink-when-point-moves-vertically 3
        beacon-blink-when-point-moves-horizontally 75
        beacon-blink-duration 0.2
        beacon-blink-delay 0.3
        beacon-blink-when-focused t
        beacon-color "red"
        beacon-size 48
        beacon-dont-blink-commands '(exchange-point-and-mark)
        beacon-dont-blink-major-modes '(t magit-status-mode magit-popup-mode inf-ruby-mode mu4e-headers-mode gnus-summary-mode gnus-group-mode dired-mode)
        )
  (beacon-mode 1)
  )

(use-package linum-relative
  :defer 1)

(use-package reljump
  :ensure nil
  :after (linum-relative)
  :commands (reljump)
  :autoload reljump-run
  :custom-face (linum ((t (:foreground "grey"))))
  :config
  (defun x4/-jump-line-beginning ()
    (interactive)
    (reljump-run
     (lambda()
       (push-mark)
       )
     (lambda()
       (beginning-of-line-text)
       (beacon-blink-automated))))
  (defun x4/-jump-line-end ()
    (interactive)
    (reljump-run
     (lambda()
       (push-mark)
       )
     (lambda()
       (end-of-line)
       (beacon-blink-automated))))
  :chords (("jl" . x4/-jump-line-beginning)
           ("je" . x4/-jump-line-end)
           ))

;;; avy - lfor quich jumping around
(use-package avy
  :config
  (defun avy--beacon-blink (&rest r)
    (beacon-blink-automated))
  (advice-add 'avy-jump :after #'avy--beacon-blink)

  (defun avy-goto-word-2 (char1 char2 &optional arg beg end symbol)
    "Jump to the currently visible CHAR1 at a word starting with CHAR1 CHAR2.
The window scope is determined by `avy-all-windows' (ARG negates it)."
    (interactive (list (read-char "char 1: " t)
                       (read-char "char 2: " t)
                       current-prefix-arg))
    (avy-with avy-goto-word-2
      (let* ((str1 (string char1))
             (str2 (string char2))
             (regex1 (cond ((string= str1 ".")
                            "\\.")
                           ((and avy-word-punc-regexp
                                 (string-match avy-word-punc-regexp str1))
                            (regexp-quote str1))
                           ((<= char1 26)
                            str1)
                           (t
                            (concat
                             (if symbol "\\_<" "\\b")
                             str1))))
             (regex2 (cond ((string= str2 ".")
                            "\\.")
                           ((and avy-word-punc-regexp
                                 (string-match avy-word-punc-regexp str2))
                            (regexp-quote str2))
                           ((<= char2 26)
                            str2)
                           (t
                            str2))))
        (avy-jump
         (concat regex1 regex2)
         :window-flip arg
         :beg beg
         :end end))))

  :bind (("C-'" . avy-goto-word-2))
  :chords (("jj" . avy-goto-char-timer)
           ;; ("jf" . avy-goto-char)
           ("jd" . avy-goto-word-2)
           ("jf" . avy-goto-char-in-line)
           ("jb" . avy-pop-mark))
  )


;;; ace-window
;; TODO: rebinding of M-<num> and M-[ doesn't belong here
(use-package ace-window
  :ensure t
  :bind (("M-]" . ace-window)
         ("M-{" . ace-window))
  :config
  ;; fix centaur's ace-window biding to M-<num>
  ;; Select widnow via `M-1'...`M-9'
  (defun x4/aw--select-window (number)
    "Slecet the specified window."
    (when (numberp number)
      (let ((found nil)
            (window-key (char-to-string (nth (1- number) aw-keys))))
        (dolist (win (aw-window-list))
          (when (and (window-live-p win)
                     (string= window-key (window-parameter win 'ace-window-path)))
            (setq found t)
            (aw-switch-to-window win)))
        (unless found
          (message "No specified window: %s" window-key)))))
  ;; M-0 works as expected when in middle of argument, works as treemacs-select when is first invoked outside argument
  (global-set-key (kbd "M-0") (lambda (arg)
                                (interactive "P")
                                (prefix-command-preserve-state)
                                (if (null arg)
                                    (treemacs-select-window)
                                  (digit-argument arg))))
  (dotimes (n 9)
    (global-set-key (kbd (format "M-%d" (1+ n))) 'digit-argument))
  (ace-window-display-mode 't)
  (dotimes (n 9)
    (bind-key (format "C-M-%d" (1+ n))
              (lambda ()
                (interactive)
                (x4/aw--select-window (1+ n)))))
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l 59)) ; 59 = ;
  ;; (aw-keys '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?0))
  :custom-face
  (aw-leading-char-face ((t (:distant-foreground "black" :height 2.5))))
  )

(global-set-key (kbd "M-[") 'x4/switch-to-last-window)

;;; plantuml-mode
(use-package plantuml-mode
  :mode "\\.plantuml\\'"
  :mode "\\.puml\\'"
  )

;;; string-inflection
(use-package string-inflection
  :bind ("C-c C-u" . string-inflection-all-cycle))


;;; ivy
;; Use C-j for immediate termination with the current value, and RET
;; for continuing completion for that directory. This is the ido
;; behaviour.
(with-eval-after-load 'ivy
  (define-key ivy-minibuffer-map (kbd "C-j") #'ivy-immediate-done)
  (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)

  (setq ivy-wrap 't)

  ;; show only current dir ./ in the list of directories in find-file & similar
  (setq ivy-extra-directories '("./"))

  (defun eh-ivy-sort-file-by-mtime (x y)
    (let* ((x (concat ivy--directory x))
           (y (concat ivy--directory y))
           (x-mtime (nth 5 (file-attributes x)))
           (y-mtime (nth 5 (file-attributes y))))
      (time-less-p y-mtime x-mtime)       ; sorts files & directories by time

      ;; sorts directories first, than files

      ;; (if (file-directory-p x)
      ;;     (if (file-directory-p y)
      ;;         (time-less-p y-mtime x-mtime)
      ;;       t)
      ;;   (if (file-directory-p y)
      ;;       nil
      ;;     (time-less-p y-mtime x-mtime)))
      )
    )

  (add-to-list 'ivy-sort-functions-alist
               '(read-file-name-internal . eh-ivy-sort-file-by-mtime))

  ;; TODO: consider if ivy-rich with custom columns for ivy-switch-buffer is still needed
  )

;; TODO: do we still want helm? maybe find something similar using ivy?
;;; Helm
;; (use-package helm
;;   :ensure t
;;   :bind ("C-M-l" . helm-mini)
;;   :config
;;   (setq helm-mini-default-sources
;;         '(helm-source-projectile-buffers-list
;;           helm-source-projectile-recentf-list
;;           helm-source-projectile-files-list
;;           helm-source-buffers-list
;;           helm-source-recentf
;;           helm-source-buffer-not-found)))

;;; emojify
(use-package emojify
  :defer 1
  :config
  ;; setup emojis in minibuffer
  (defun x4/-emojify-minibuffer-hook-fn ()
    ;; (setq emojify-minibuffer-reading-emojis-p 't)
    (emojify-mode +1))
  (global-emojify-mode 't)
  :hook (minibuffer-setup-hook . x4/-emojify-minibuffer-hook-fn)
  :custom
  (emojify-emojis-dir (expand-file-name "emojis" x4/var-path)))


;;; yasnippet
(use-package yasnippet
  :defer 1
  :init
  (setq yas-snippet-dirs
        '("~/.emacs.d/snippets"                 ;; personal snippets
          "~/.emacs.d/personal/snippets"        ;; personal snippets
          ))
  :config
  (defun x4/activate-yas-extra-git-commit-mode-hook-fn ()
    (yas-activate-extra-mode 'git-commit-mode))
  (yas-global-mode 1)
  :hook
  (git-commit-mode . x4/activate-yas-extra-git-commit-mode-hook-fn)
  )

;; TODO: move this to separate file
;; git-commit snippet's scopes
(defvar git-commit-mode-scopes-list '()
  "Contains a list of scopes to be choose from when committing.")

(defun git-commit-mode-scopes-list-safep (v)
  (and (listp v)
       (seq-every-p #'stringp v)))
(put 'git-commit-mode-scopes-list 'safe-local-variable
     'git-commit-mode-scopes-list-safep)

(defun x4/-extract-bounded-contexts-from-staged-files ()
  (let ((scopes (->>
                 ;; TODO: this doesn't work for interactinve rebase
                 (magit-staged-files)
                 ;; '("lib/solis.ex")
                 (--map
                  (-> it
                      (split-string "/")
                      (pcase
                          (`("test" . ,_) "test")
                        (`("lib" ,(rx (* any) "_web") . ,_) "web")
                        ((and `("lib" ,app ,bounded-context . ,_)
                              (guard (file-directory-p (expand-file-name
                                                               (s-join "/" (list "lib" app bounded-context))
                                                               (project-root (project-current))
                                                              ))))
                         bounded-context)
                        )
                      ))
                 (-keep #'identity)
                 (-sort #'string<)
                 (-distinct)
                 )))
    (pcase scopes
      ('nil nil)
      (_ (s-join "," scopes)))
    )
  )

(defun git-commit-mode-snippet-scopes ()
  (if (null git-commit-mode-scopes-list)
      ;; (delete-dups (mapcar 'projectile-project-name projectile-known-projects))
      '()
    git-commit-mode-scopes-list))


;;; swap-regions
(use-package swap-regions
  :bind ("C-C C-t" . swap-regions))

;;; which-key - display available keybindings
(use-package which-key
  :diminish
  :bind ("C-h M-m" . which-key-show-full-major-mode)
  :hook (after-init . which-key-mode)

  :custom
  (which-key-sort-order 'which-key-prefix-then-key-order)
  (which-key-show-remaining-keys t)
  (which-key-min-description-width 32)
  (which-key-max-description-length 128)
  (which-key-show-docstrings t))

(use-package display-line-numbers
  :ensure nil
  :hook (prog-mode yaml-mode conf-mode text-mode markdown-mode)
  :init (setq display-line-numbers-width-start t)
  :custom-face (line-number ((t (:foreground "grey33"))))
  )

;;;; Enable sbt mode for executing sbt commands
(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
  ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
  (setq sbt:program-options '("-Dsbt.supershell=false")))


;;;; Add metals backend for lsp-mode
(use-package fennel-mode)

;; (use-package polymode
;;   :ensure t
;;   :mode ("\.ex$" . poly-elixir-web-mode)
;;   :config
;;   (setq polymode-prefix-key (kbd "C-c n"))

;;   (define-hostmode poly-elixir-hostmode :mode 'elixir-mode)

;;   (define-innermode poly-heex-elixir-innermode
;;     :mode 'web-mode
;;     :head-matcher (rx "~H" (= 3 (char "\"'")) (* (any space)))
;;     :tail-matcher (rx (= 3 (char "\"'")))
;;     :head-mode 'host
;;     :tail-mode 'host)

;;   (define-polymode poly-elixir-web-mode
;;     :hostmode 'poly-elixir-hostmode
;;     :innermodes '(poly-heex-elixir-innermode)
;;     ))


(use-package paradox
  :custom-face
  (paradox-archive-face ((t (:inherit font-lock-doc-face))))
  (paradox-description-face ((t (:inherit completions-annotations))))
  :hook (emacs-startup . paradox-enable)
  :init (setq paradox-execute-asynchronously t
              paradox-github-token t
              paradox-display-star-count nil
              paradox-status-face-alist ;
              '(("built-in"   . font-lock-builtin-face)
                ("available"  . success)
                ("new"        . (success bold))
                ("held"       . font-lock-constant-face)
                ("disabled"   . font-lock-warning-face)
                ("avail-obso" . font-lock-comment-face)
                ("installed"  . font-lock-comment-face)
                ("dependency" . font-lock-comment-face)
                ("incompat"   . font-lock-comment-face)
                ("deleted"    . font-lock-comment-face)
                ("unsigned"   . font-lock-warning-face)))
  :custom
  (paradox-column-width-package 30)
  (paradox-column-width-version 16)
  :config
  (add-hook 'paradox-after-execute-functions
            (lambda (_)
              "Display `page-break-lines' in \"*Paradox Report*\" buffer."
              (when (fboundp 'page-break-lines-mode)
                (let ((buf (get-buffer "*Paradox Report*"))
                      (inhibit-read-only t))
                  (when (buffer-live-p buf)
                    (with-current-buffer buf
                      (page-break-lines-mode 1))))))
            t))

;;; Elixir

;; unload because we have two modified alchemist files in personal and they need
;; to be loaded from our personal dir, instead of elpa dir
(dolist (f '(alchemist-phoenix alchemist-goto alchemist-file alchemist-mix alchemist-hooks))
  (when (featurep f)
    (unload-feature f 't)))

(use-package alchemist
  :init
  (setq alchemist-key-command-prefix (kbd "C-c C-a"))
  :custom
  (alchemist-key-command-prefix (kbd "C-c C-a"))
  (alchemist-mix-env "dev")

  :config
  ;; (add-to-list 'popper-reference-buffers 'alchemist-mix-mode)
  (add-to-list 'popper-reference-buffers 'alchemist-test-report-mode)
  (popper--set-reference-vars)          ; update popper to recognize buffers

  (defun x4/-alchemist-set-env (env)
    (setq alchemist-mix-env env))

  :pretty-hydra
  ((:title (pretty-hydra-title "Alchemist" 'sucicon "nf-custom-elixir")
    :color teal :exit t :quit-key ("q" "C-g" "RET"))
   (
   ;; "Compile"
   ;;  (("c b" alchemist-compile-this-buffer "this buffer") ; Compile the current buffer with elixirc.
   ;;  ("c c" alchemist-compile "compile") ; Compile CMDLIST with elixirc.
   ;;  ("c f" alchemist-compile-file "file") ; Compile the given FILENAME.
   ;;  )

    "Execute"
    (("e b" alchemist-execute-this-buffer "this buffer") ; Run the current buffer through elixir.
     ("e e" alchemist-execute "execute") ; Run a elixir with CMDLIST.
     ("e f" alchemist-execute-file "file") ; Run elixir with the given FILENAME.
     )

    "Help & Info"
    (("h ." alchemist-help-search-at-point "search at point") ; Search through ‘alchemist-help’ with the expression under the cursor.
     ("h d" alchemist-info-datatype-at-point "datatype at point") ; Return information about any datatype under the cursor.
     ("h t" alchemist-info-types-at-point "types at point") ; Return information of types under the cursor.
     ("h h" alchemist-help "help") ; Load Elixir documentation for SEARCH.
     ("h i" alchemist-help-history "history") ; Load Elixir from the documentation history for SEARCH.
     ;; ("h r" alchemist-refcard "refcard") ; Generate an Alchemist refcard of all the features.
     )

    "Mix"
    (("x" alchemist-mix "mix") ; Prompt for a specific mix task to run.
     ("m c" alchemist-mix-compile "compile") ; Compile the whole elixir project. Prompt for the mix env if the prefix
     ("m d" (lambda() (interactive) (alchemist-mix-execute '("deps.compile"))) "compile deps")
     ("m g" (lambda() (interactive) (alchemist-mix-execute '("deps.get"))) "get deps")
     )

    "Test"
    (("r" alchemist-mix-rerun-last-test "rerun last test") ; Rerun the last test that was run by alchemist.
     ("t ." alchemist-mix-test-at-point "at point") ; Run the test at point.
     ("t b" alchemist-mix-test-this-buffer "this buffer") ; Run the current buffer through mix test.
     ("t f" alchemist-mix-test-file "file") ; Run ‘alchemist-mix--test-file’ with the FILENAME.
     ("t t" alchemist-mix-test "all") ; Run the whole elixir test suite.
     ("t s" alchemist-mix-test-stale "stale") ; Run stale tests (Elixir 1.3+ only "mix test stale) ; Run stale tests (Elixir 1.3+ only").
     ("t F" (lambda() (interactive) (alchemist-mix--execute-test "--failed")) "failed") ; Run stale tests (Elixir 1.3+ only "mix test stale) ; Run stale tests (Elixir 1.3+ only").
     ("T" alchemist-test-toggle-test-report-display "toggle test report display") ; Toggle between display or hidding ‘alchemist-test-report-buffer-name’ buffer.
     )

    "Env"
    (("e d" (x4/-alchemist-set-env "dev") "dev" :toggle (equal alchemist-mix-env "dev") :exit nil)
     ("e t" (x4/-alchemist-set-env "test") "test" :toggle (equal alchemist-mix-env "test") :exit nil)
     ("e p" (x4/-alchemist-set-env "prod") "prod" :toggle (equal alchemist-mix-env "prod") :exit nil))

    "IEx"
    (
     ("i i" alchemist-iex-run "run") ; Start an IEx process.
     ("i p" alchemist-iex-project-run "project run") ; Start an IEx process with mix ’iex -S mix’ in the
     ("i r" alchemist-iex-send-region "send region") ; Sends the marked region to the IEx process.
     ("i R" alchemist-iex-reload-module "reload module") ; Recompiles and reloads the current module in the IEx process.
     ("i l" alchemist-iex-send-current-line "send current line") ; Sends the current line to the IEx process.
     ("i m" alchemist-iex-send-region-and-go "send region and go") ; Sends the marked region to the inferior IEx process
     ("i b" alchemist-iex-compile-this-buffer "compile this buffer") ; Compiles the current buffer in the IEx process.
     ("i c" alchemist-iex-send-current-line-and-go "send current line and go") ; Sends the current line to the inferior IEx process
     )

    ;; "Macroexpand"
    ;; (("o !" alchemist-macroexpand-close-popup "close popup") ; Quit the macroexpand buffer window.
    ;; ("o I" alchemist-macroexpand-once-print-region "once print region") ; Macro expand the Elixir code on marked region once and insert the result.
    ;; ("o K" alchemist-macroexpand-print-current-line "print current line") ; Macro expand the Elixir code on the current line and insert the result.
    ;; ("o L" alchemist-macroexpand-once-print-current-line "once print current line") ; Macro expand the Elixir code on the current line and insert the result.
    ;; ("o R" alchemist-macroexpand-print-region "print region") ; Macro expand the Elixir code on marked region and insert the result.
    ;; ("o i" alchemist-macroexpand-once-region "once region") ; Macro expand the Elixir code on marked region once.
    ;; ("o k" alchemist-macroexpand-current-line "current line") ; Macro expand the Elixir code on the current line.
    ;; ("o l" alchemist-macroexpand-once-current-line "once current line") ; Macro expand the Elixir code on the current line.
    ;; ("o r" alchemist-macroexpand-region "region") ; Macro expand the Elixir code on marked region.
    ;; )

    "Project"
    (("p f" alchemist-project-find-lib "find lib") ;
     ("p t" alchemist-project-find-test "find test") ;
     ("p o" alchemist-project-toggle-file-and-tests-other-window "toggle file and tests other window") ; Toggle between a file and its tests in other window.
     ("p s" alchemist-project-toggle-file-and-tests "toggle file and tests") ; Toggle between a file and its tests in the current window.
     ("p T" alchemist-project-run-tests-for-current-file "run tests for current file") ; Run the tests related to the current file.
     )


    "Phoenix"
    (("n R" alchemist-phoenix-routes "routes")
     ("n r" alchemist-phoenix-router "open router" )
     ("n c" alchemist-phoenix-find-controllers "find controllers")
     ("n l" alchemist-phoenix-find-channels "find channels")
     ;; ("n m" alchemist-phoenix-find-models "find models")
     ("n s" alchemist-phoenix-find-static "find static")
     ("n t" alchemist-phoenix-find-templates "find templates")
     ("n v" alchemist-phoenix-find-views "find views")
     ("n w" alchemist-phoenix-find-web "find web")
     )

    "Eval"
    (("v !" alchemist-eval-close-popup "close popup") ; Quit the evaluation buffer window.
     ("v e" alchemist-eval-quoted-buffer "quoted buffer") ; Get the Elixir code representation of the expression in the current buffer.
     ("v h" alchemist-eval-print-quoted-current-line "print quoted current line") ; Get the Elixir code representation of the expression on the current line and insert th…
     ("v i" alchemist-eval-print-region "print region") ; Evaluate the Elixir code on marked region and insert the result.
     ("v j" alchemist-eval-quoted-current-line "quoted current line") ; Get the Elixir code representation of the expression on the current line.
     ("v k" alchemist-eval-print-current-line "print current line") ; Evaluate the Elixir code on the current line and insert the result.
     ("v l" alchemist-eval-current-line "current line") ; Evaluate the Elixir code on the current line.
     ("v o" alchemist-eval-region "region") ; Evaluate the Elixir code on marked region.
     ("v q" alchemist-eval-buffer "buffer") ; Evaluate the Elixir code in the current buffer.
     ("v r" alchemist-eval-print-quoted-buffer "print quoted buffer") ; Get the Elixir code representation of the expression in the current buffer and insert result.
     ("v u" alchemist-eval-quoted-region "quoted region") ; Get the Elixir code representation of the expression on marked region.
     ("v w" alchemist-eval-print-buffer "print buffer") ; Evaluate the Elixir code in the current buffer and insert the result.
     ("v y" alchemist-eval-print-quoted-region "print quoted region") ; Get the Elixir code representation of the expression on marked region and insert the result.
     )

    "Hex"
    (("X I" alchemist-hex-info "info") ; Display Hex package info for a certain package.
     ("X R" alchemist-hex-releases "releases") ; Display Hex package releases for a certain package.
     ("X d" alchemist-hex-all-dependencies "all dependencies") ; Display Hex package dependencies for the current Mix project.
     ("X i" alchemist-hex-info-at-point "info at point") ; Display Hex package information for the package at point.
     ("X r" alchemist-hex-releases-at-point "releases at point") ; Display Hex package releases for the package at point.
     ("X s" alchemist-hex-search "search") ; Search for Hex packages.
     )

    "Goto"
    (("M-," alchemist-goto-jump-back "jump back") ; Go back to the previous position in xref history.
     ("M-." alchemist-goto-definition-at-point "definition at point") ; Jump to the elixir expression definition at point.
     ("M-N" alchemist-goto-jump-to-next-def-symbol "jump to next def symbol") ;
     ("M-P" alchemist-goto-jump-to-previous-def-symbol "jump to previous def symbol") ;
     ("." alchemist-goto-list-symbol-definitions "list symbol definitions") ; List all symbol definitions in the current file like functions/macros/modules.
     )))

  :bind
  (("C-c a" . alchemist-hydra/body)
   ("C-c C-a" . alchemist-hydra/body))
  )

(use-package elixir-mode
  :config
  (setq
   ;; clear elixir-ts-mode from auto-mode-alist
   auto-mode-alist (rassq-delete-all 'elixir-ts-mode auto-mode-alist)

   ;; set ElixirLS version to use
   lsp-elixir-ls-version "v0.19.0"
   lsp-elixir-ls-download-url (format "https://github.com/elixir-lsp/elixir-ls/releases/download/%s/elixir-ls-%s.zip"
                                      lsp-elixir-ls-version lsp-elixir-ls-version))
  :hook ((alchemist-test-report-mode . visual-line-mode))
  :bind (:map elixir-mode-map
         ("C-c C-u" . string-inflection-elixir-style-cycle)))


;;; tabspaces.el
(use-package tab-bar
  :demand t
  :custom
  (tab-bar-show 't)
  :config
  (tab-bar-mode 1))

(use-package tabspaces
  :demand t
  :hook (after-init . tabspaces-mode)
  :custom
  (tabspaces-use-filtered-buffers-as-default t)
  (tabspaces-include-buffers '("*scratch*" "*oMessages*" "*dashboard*"))
  (tabspaces-default-tab "Default")
  ;; (progn
  ;;   (tab-bar-rename-tab "Default")
  ;;   (dolist (buf '("*scratch*" "*Messages*" "*dashboard*"))
  ;;   (when-let (buf (get-buffer buf))
  ;;     (set-frame-parameter nil
  ;;                          'buffer-list
  ;;                          (cons buf (frame-parameter nil 'buffer-list))))))
  (tabspaces-remove-to-default t)
  ;; sessions
  (tabspaces-session t)
  (tabspaces-session-auto-restore t)

  ;; :chords
  ;; (("<tab>[" . tab-bar-switch-to-recent-tab)
  ;;  ("<tab><left>" . tab-bar-switch-to-prev-tab)
  ;;  ("<tab><right>" . tab-bar-switch-to-next-tab))

  :pretty-hydra
  ((:title (pretty-hydra-title "Tabspaces" 'mdicon "nf-md-tab")
    :color amaranth :body-pre (rectangle-mark-mode) :post (deactivate-mark)
    :exit t :quit-key ("q" "C-g" "RET"))
   ("Buffers"
    (("r" (tabspaces-remove-current-buffer) "remove current")
     ("R" tabspaces-remove-selected-buffer "remove selected")
     ("C" tabspaces-clear-buffers "clear list")
     )
    "Switch"
     (("[" tab-bar-switch-to-recent-tab "to recent tab")
      ("<tab>" tab-bar-switch-to-tab "tab by name")
      ("b" tabspaces-switch-to-buffer "local buffer")
      ("g" tabspaces-switch-buffer-and-tab "global buffer")
      ("<left>" tab-bar-switch-to-prev-tab "prev tab" :exit nil)
      ("<right>" tab-bar-switch-to-next-tab "next tab" :exit nil)
      )
    "Workspace"
     (("o" tabspaces-open-or-create-project-and-workspace "open")
      ("S" tabspaces-switch-or-create-workspace "switch or open")
      ("k" tabspaces-kill-buffers-close-workspace "kill & close")
      ("d" tabspaces-close-workspace "close don't kill")
      )
    ))
  :bind (
         ;; ("C-<tab>". tab-bar-switch-to-next-tab)
         ("C-c <tab>" . tabspaces-hydra/body)
         )
  )

;; Multi VTerm
(use-package multi-vterm
  :bind (:map vterm-mode-map
         ("C-c C-`" . multi-vterm)
         ("C-c C-<right>" . multi-vterm-next)
         ("C-c C-<left>" . multi-vterm-prev)
         )
  )

;; HHN magic numbers documentation
(use-package hhn-magic-numbers-docs
  :load-path x4/personal-confidential-path
  :no-require t
  :if sys/macp
  :hook
  ((elixir-mode . x4/hhn-magic-numbers-docs-setup)
   (lsp-mode . x4/hhn-magic-numbers-docs-setup))
  )

(use-package hl-todo
  :custom-face
  (hl-todo ((t (:inherit default :height 0.9 :width condensed :weight bold :underline nil :inverse-video t))))
  :hook ((after-init . global-hl-todo-mode)
         (hl-todo-mode . (lambda ()
                           (add-hook 'flymake-diagnostic-functions
                                     #'hl-todo-flymake nil t))))
  :init (setq hl-todo-require-punctuation t
              hl-todo-highlight-punctuation ":")
  :config
  (dolist (keyword '("REVIEW"))
    (add-to-list 'hl-todo-keyword-faces `(,keyword . "#ecb8bb")))
  (advice-add #'hl-todo-insert :after
              (defun x4/-insert-punctuation-after (&rest r)
                (insert hl-todo-highlight-punctuation " ")))
  )

(print "custom-post LOADED!")

(provide 'custom-post)
;;; custom-post.el ends here
