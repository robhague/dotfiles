(message "Rob's cross-platform .emacs file")

;; Start relative to home directory
(setq default-directory "~/")
(cd "~/")
(setq inhibit-splash-screen t)

;; Turn on syntax coloring and font-lock globally
(cond ((fboundp 'global-font-lock-mode)
(global-font-lock-mode t)
(setq-default font-lock-maximum-decoration t)))

;; Sort out tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

;; And don't clutter up the place with backup files
(setq-default make-backup-files nil)

;; Keep up with external file changes (e.g., git pull)
(global-auto-revert-mode)

;; HCI for Masochists
(menu-bar-mode -1)
(tool-bar-mode -1)
(setq-default mac-command-modifier 'meta)
(setq-default mac-option-modifier 'none)
(savehist-mode)
(auto-save-mode t) ; Disable auto-save

; See http://stackoverflow.com/questions/24956521
(when (and (eq system-type 'darwin) (display-graphic-p))
  (defun contextual-menubar (&optional frame)
    "Display the menubar in FRAME (default: selected frame) if on a
graphical display, but hide it if in terminal."
    (interactive)
    (set-frame-parameter frame 'menu-bar-lines
                         (if (display-graphic-p frame) 1 0)))
  (add-hook 'after-init-hook 'contextual-menubar)
  (add-hook 'after-make-frame-functions 'contextual-menubar))


;; I've finally tired of typing "y-e-s"
(defalias 'yes-or-no-p 'y-or-n-p)

;; Show the column number
(column-number-mode t)

;; Behave well in an xterm
(xterm-mouse-mode t)
(defun tmm-menubar () nil)
(defun tmm-menubar-mouse (event) (interactive "e") nil)

;; Use visual lines and flyspell for Text mode
(defun my-textmode-hook () "Nice word-processing features"
  (visual-line-mode)
  (variable-pitch-mode)
  (setq tab-width 4)
  (flyspell-mode)
  (text-scale-increase 1))
(add-hook 'text-mode-hook 'my-textmode-hook)

;; Don't flyspell things that look like code
(defun string-match-case-sensitive (regexp string)
  "Case-sensitive string match"
  (setq old-case-fold-search case-fold-search
        case-fold-search nil
        result (string-match regexp string)
        case-fold-search old-case-fold-search)
  result)

(defun word-looks-like-code (word)
  "Returns true if the word is in CamelCase or contains_underscores_or_d1gits"
  (string-match-case-sensitive "[a-z][A-Z]\\|[^A-Za-z]" word))

(defun my-flyspell-check-word-p ()
  (not (or (get-text-property (point) 'mouse-face)
           (word-looks-like-code (thing-at-point 'word)))))

(setq flyspell-generic-check-word-predicate 'my-flyspell-check-word-p)

;; Enable flyspell in comments
(defun my-prog-mode-hook () (flyspell-prog-mode))
(add-hook 'prog-mode-hook 'my-prog-mode-hook)

;; Follow the compilation output
(setq compilation-scroll-output t)

;; Fix behaviour
(setq ispell-dictionary "en_GB")

;; Programming language hooks
(add-hook 'js-mode-hook 'flycheck-mode)
(add-hook 'web-mode-hook 'flycheck-mode)

;; A command for inserting the date in RFC822-compliant form
(defun insert-date()
  (interactive)
  (insert (shell-command-to-string "date"))
  (join-line))

;; I can never remember how to do this, so...
(defun dos-to-unix()
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix))

;; View mode hook
(defun my-view-mode-hook () "File viewing features"
  (auto-revert-mode t))
(add-hook 'view-mode-hook 'my-view-mode-hook)
(global-set-key "\C-x\C-v" 'view-file)

;; Allow C-u C-<SPC> followed by repeated C-<SPC>
(setq-default set-mark-command-repeat-pop 't)

;; Make matching parentheses visible
(show-paren-mode 't)

;; Bind the windmove functions to M-<arrow>
(global-set-key [?\C-x right] 'windmove-right)
(global-set-key [?\C-x left] 'windmove-left)
(global-set-key [?\C-x up] 'windmove-up)
(global-set-key [?\C-x down] 'windmove-down)

;; Experimental bindings for other-window
(global-set-key [C-tab] 'other-window)
(global-set-key [?\M-o] 'other-window)


;; I know how big I want my windows to be, dammit!
(setq-default even-window-heights nil)
(setq-default split-width-threshold nil)

;; Uniquify buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

;; Bind goto-line to various keys (for pre-emacs22, where M-g M-g isn't a prefix)
(global-set-key "\C-x\\" 'goto-line) ; Works everywhere
(global-set-key [?\C-#] 'goto-line)

;; Rebind Regexp query-replace, as C-M-% doesn't work through screen
(global-set-key "\M-?" 'query-replace-regexp)

;; Rebind little-used keys (chiefly C-z, to avoid accidentally backgrounding Emacs so often)
(global-unset-key "\C-z")
(global-unset-key "\C-\\")

;; Bind something to compile
(global-set-key "\C-xc" 'compile)

;; Easy kill-this-buffer
(global-set-key "\C-x\M-k" 'kill-this-buffer)

;; Confirm leaving emacs...
(setq confirm-kill-emacs (lambda (prompt)
                           (or (memq t (mapcar (function
                              (lambda (buf) (and (buffer-file-name buf)
                                                 (buffer-modified-p buf))))
                                               (buffer-list)))
                               (yes-or-no-p prompt))))

;; A save-and-kill-buffer command
(defun save-and-kill-buffer ()
  "Save the current buffer, then kill it"
  (interactive)
  (save-buffer)
  (kill-buffer (current-buffer)))

(global-set-key "\C-xw" 'save-and-kill-buffer)

;; Vi-style line joining
(defun join-next-line ()
  "Joins the next line to this one"
  (interactive)
  (next-line)
  (join-line))
(global-set-key [?\C-\\] 'join-next-line)

;; Capture attempts to enter Vi command mode, with some handy aliases
(global-set-key "\M-:" 'execute-extended-command)
(defalias 'wq 'save-and-kill-buffer)

;; Bind indent-region to something easy to remember
(global-set-key [C-M-tab] 'indent-region)

;; Undefine M-p, allowing us to use it as a prefix, then use that
;; prefix for bindings to print various things
(global-set-key "\M-p" nil)
(global-set-key "\M-pd" 'pwd)

(defun buffer-file-name-interactive () (interactive) (message buffer-file-name))
(global-set-key "\M-pf" 'buffer-file-name-interactive)

;; Use Mac-like binding for various commands (save, yank/paste, copy, undo)
;; You can have my M-x when you take it from my cold, dead hands
(global-set-key "\M-s" 'save-buffer)
(global-set-key "\M-v" 'yank) ;; was scroll-down-command
(global-set-key "\M-c" 'kill-ring-save) ;; was capitalize-word
(global-set-key "\M-z" 'undo) ;; was zap-to-char

;; Server
(add-hook 'server-visit-hook
          (lambda nil
            (local-set-key "\C-c\C-c" 'server-edit)))

(server-start)

;; ibuffer
(global-set-key "\C-x\C-b" 'ibuffer-other-window)

;; Bind magit-status to a key
(global-set-key "\C-x\C-g" 'magit-status)

;; Disable truncation in partial-width windows
(setq truncate-partial-width-windows nil)

;; Set up path to include /usr/local (e.g., homebrew)
(setq exec-path (append exec-path '("/usr/local/bin")))
(setenv "PATH" "$PATH:/usr/local/bin" 't)

;; Allow narrowing
(put 'narrow-to-region 'disabled nil)

;; Trailing whitespace is rarely, if ever useful
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Packages
(require 'package)
(add-to-list 'package-archives
  '("melpa" . "https://melpa.org/packages/") t)

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . web-mode))
(eval-after-load 'flycheck
  '(flycheck-add-mode 'javascript-eslint 'web-mode))

;; Required for themes to work in customization
(package-initialize)

;; Run local config
(setq local-config-file (expand-file-name "local.el" user-emacs-directory))
(if (file-exists-p local-config-file) (load local-config-file))

;; Save custom variables elsewhere
(setq custom-file (expand-file-name (expand-file-name "custom" user-emacs-directory)))
(if (file-exists-p custom-file) (load custom-file))
