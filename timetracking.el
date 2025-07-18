;; A major mode for tracking time spent

(defun lastline-p () (= (line-number-at-pos (point))
                        (line-number-at-pos (point-max))))

(defun timetrack-newline ()
  (interactive)
  ;; If we're not on the last line, use the current line as a template
  (delete-trailing-whitespace (point) nil)
  (let ((default-task (if (lastline-p) ""
                        (buffer-substring (+ (line-beginning-position) 17)
                                          (line-end-position)))))

    ;; Move to the end of the file and add a new line
    (end-of-buffer)
    (newline)
    (insert (format-time-string "%Y-%m-%d %H:%M ") default-task)))

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
