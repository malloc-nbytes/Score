;; qbe mode
(defconst qbe-mode-syntax-table
  (with-syntax-table (copy-syntax-table)
    (modify-syntax-entry ?- ". 124b")
    (modify-syntax-entry ?* ". 23")
    (modify-syntax-entry ?\n "> b")
    (modify-syntax-entry ?' "\"")
    (modify-syntax-entry ?' ".")
    (syntax-table))
  "Syntax table for `qbe-mode'.")

(defun qbe-indent-line ()
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
  (defconst qbe-keywords
    '("export" "function" "data" "type" "w" "l" "s" "d" "b" "h"
      "add" "and" "div" "mul" "neg" "or" "rem" "sar"
      "shl" "shr" "sub" "udiv" "urem" "xor" "alloc16"
      "alloc4" "alloc8" "blit" "loadd" "loadl" "loads"
      "loadsb" "loadsh" "loadsw" "loadub" "loaduh" "loaduw"
      "loadw" "storeb" "stored" "storeh" "storel" "stores"
      "storew" "ceqd" "ceql" "ceqs" "ceqw" "cged" "cges"
      "cgtd" "cgts" "cled" "cles" "cltd" "clts" "cned"
      "cnel" "cnes" "cnew" "cod" "cos" "csgel" "csgew"
      "csgtl" "csgtw" "cslel" "cslew" "csltl" "csltw"
      "cugel" "cugew" "cugtl" "cugtw" "culel" "culew"
      "cultl" "cultw" "cuod" "cuos" "dtosi" "dtoui"
      "exts" "extsb" "extsh" "extsw" "extub" "extuh" "extuw"
      "sltof" "ultof" "stosi" "stoui" "swtof" "uwtof" "truncd"
      "cast" "copy" "call" "vastart" "vaarg" "phi" "hlt"
      "jmp" "jnz" "ret" )))

(defconst qbe-highlights
  `((,(regexp-opt qbe-keywords 'symbols) . font-lock-keyword-face)
    (,(rx (group "#" (zero-or-more (not (any "\n"))))
         (group-n 1 (zero-or-more (any "\n"))))
     (1 font-lock-comment-delimiter-face)
     (2 font-lock-comment-face nil t))))

;;;###autoload
(define-derived-mode qbe-mode prog-mode "qbe"
  "Major Mode for editing Qbe source code."
  :syntax-table qbe-mode-syntax-table
  (setq font-lock-defaults '(qbe-highlights))
  (setq-local comment-start "#")
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width 4)
  (setq-local indent-line-function #'qbe-indent-line)
  (setq-local standard-indent 2))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ssa\\'" . qbe-mode))

(provide 'qbe-mode)
;; End qbe mode
