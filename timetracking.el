;; A major mode for tracking time spent

(defun timetrack-newline ()
  (interactive)
  (end-of-buffer)
  (newline)
  (insert (format-time-string "%Y-%m-%d %H:%M") " - "))

;; Define the keymap
(defvar timetrack-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'timetrack-newline)
    map)
  "Keymap for `timetrack-mode'.")

;; Syntax highlighting
(defvar timetrack-font-lock-keywords
  '(("[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]" . font-lock-keyword-face)
    ("[0-9][0-9]:[0-9][0-9]" . font-lock-string-face)
    ("#[-[:alnum:]]+" . font-lock-constant-face))
    "Syntax highlighting for `timetrack-mode'.")

;; The mode itself
(define-derived-mode timetrack-mode
  text-mode "TimeTrack"
  "A major mode for tracking time spent"
  (use-local-map timetrack-mode-map)
  (setq-local font-lock-defaults '((timetrack-font-lock-keywords))))

(add-hook 'timetrack-mode-hook 'ugly-textmode-hook)

(add-to-list 'auto-mode-alist '("\\.timetrack\\'" . timetrack-mode))
