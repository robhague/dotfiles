;; A major mode for tracking time spent

(defun lastline-p ()
  "True if the point is on the last line of the buffer"
  (= (line-number-at-pos (point))
     (line-number-at-pos (point-max))))

(defun trim-blank-lines ()
  "Remove all blank lines from the end of the buffer"
  (save-excursion
    (end-of-buffer)
    (re-search-backward "[^[:blank:][:cntrl:]]")
    (forward-char)
    (delete-region (point) (point-max))))

(defun timetrack-newline ()
  (interactive)
  (trim-blank-lines)
  ;; If we're not on the last line, use the current line as a template
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
