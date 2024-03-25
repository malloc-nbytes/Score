;; Score mode for Emacs
;; Stolen from https://gitlab.com/tsoding/porth/-/blob/master/editor/porth-mode.el?ref_type=heads
;; and slightly edited to fit Score.

;; Score mode
(defconst score-mode-syntax-table
  (with-syntax-table (copy-syntax-table)
    (modify-syntax-entry ?- ". 124b")
    (modify-syntax-entry ?* ". 23")
    (modify-syntax-entry ?\n "> b")
    (modify-syntax-entry ?' "\"")
    (modify-syntax-entry ?' ".")
    (syntax-table))
  "Syntax table for `score-mode'.")

;; Function taken from:
;;  https://www.omarpolo.com/post/writing-a-major-mode.html
(defun score-indent-line ()
  "Indent current line."
  (let (indent
        boi-p
        move-eol-p
        (point (point)))
    (save-excursion
      (back-to-indentation)
      (setq indent (car (syntax-ppss))
            boi-p (= point (point)))
      (when (and (eq (char-after) ?\n)
                 (not boi-p))
        (setq indent 0))
      (when boi-p
        (setq move-eol-p t))
      (when (or (eq (char-after) ?\))
                (eq (char-after) ?\}))
        (setq indent (1- indent)))
      (delete-region (line-beginning-position)
                     (point))
      (indent-to (* tab-width indent)))
    (when move-eol-p
      (move-end-of-line nil))))

(eval-and-compile
  (defconst score-keywords
    '("if" "else" "while" "let" "void" "i32"
      "str" "for" "proc" "return" "mut" "break" "macro"
      "usize" "struct" "char" "import" "ref" "end" "export"
      "def" "in" "null" "type" "module" "where")))

(defconst score-highlights
  `((,(regexp-opt score-keywords 'symbols) . font-lock-keyword-face)
    (,(rx (group "--" (zero-or-more (not (any "\n"))))
         (group-n 1 (zero-or-more (any "\n"))))
     (1 font-lock-comment-delimiter-face)
     (2 font-lock-comment-face nil t))))

;;;###autoload
(define-derived-mode score-mode prog-mode "score"
  "Major Mode for editing Score source code."
  :syntax-table score-mode-syntax-table
  (setq font-lock-defaults '(score-highlights))
  (setq-local comment-start "--")
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width 4)
  (setq-local indent-line-function #'score-indent-line)
  (setq-local standard-indent 2))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.scr\\'" . score-mode))

(provide 'score-mode)
;; End Score mode
