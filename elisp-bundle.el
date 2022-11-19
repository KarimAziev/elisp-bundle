;;; elisp-bundle.el --- Auto-declarations, requires or definitions -*- lexical-binding: t -*-

;; Copyright (C) 2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/elisp-bundle
;; Keywords: lisp
;; Version: 0.2.0
;; Package-Requires: ((emacs "27.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;; Commentary:

;; This file configures operations with bundle

;; Commands

;; M-x `elisp-bundle-include-undefined'
;;    Insert defintions of unused functions or variables in current buffer.
;;    Other available strategies - insert declare-functions form or require.

;;; Code:

(defun elisp-bundle-move-with (fn &optional n)
  "Move by calling FN N times.
Return new position if changed, nil otherwise."
  (unless n (setq n 1))
  (when-let ((str-start (nth 8 (syntax-ppss (point)))))
    (goto-char str-start))
  (let ((init-pos (point))
        (pos)
        (count (if (> n 0) n (- n))))
    (while
        (and (not (= count 0))
             (when-let ((end (ignore-errors
                               (funcall fn (if
                                               (> n 0) 1
                                             -1))
                               (point))))
               (unless (or (= end
                              (or pos init-pos))
                           (nth 4 (syntax-ppss (point)))
                           (and (looking-at ";")
                                (nth 4 (syntax-ppss (1+ (point))))))
                 (setq pos end))))
      (setq count
            (1- count)))
    (if (= count 0)
        pos
      (goto-char init-pos)
      nil)))

(defun elisp-bundle-re-search-backward-inner (regexp &optional bound count)
  "This function is helper for `elisp-bundle-re-search-backward'.
Search backward from point for regular expression REGEXP.
The optional argument BOUND is a buffer position that bounds
  the search.  The match found must not end after that position.  A
  value of nil means search to the end of the accessible portion of
  the buffer.
The optional argument COUNT is a number that indicates the
  search direction and the number of occurrences to search for."
  (let ((parse))
    (while (> count 0)
      (with-syntax-table emacs-lisp-mode-syntax-table
        (re-search-backward regexp bound)
        (setq parse (syntax-ppss))
        (cond ((and (or (nth 3 parse))
                    (nth 8 parse))
               (goto-char (nth 8 parse)))
              ((and (nth 4 parse)
                    (nth 8 parse))
               (goto-char (nth 8 parse)))
              (t
               (setq count (1- count)))))))
  (point))

(defun elisp-bundle-re-search-forward-inner (regexp &optional bound count)
  "This function is helper for `elisp-bundle-re-search-forward'.
Search forward from point for regular expression REGEXP.
The optional argument BOUND is a buffer position that bounds
  the search.  The match found must not end after that position.  A
  value of nil means search to the end of the accessible portion of
  the buffer.
The optional argument COUNT is a number that indicates the
  search direction and the number of occurrences to search for."
  (let ((parse))
    (while (> count 0)
      (with-syntax-table emacs-lisp-mode-syntax-table
        (re-search-forward regexp bound)
        (setq parse (syntax-ppss))
        (cond ((and (nth 3 parse)
                    (nth 8 parse))
               (goto-char (nth 8 parse))
               (forward-sexp))
              ((and (nth 4 parse)
                    (nth 8 parse))
               (goto-char (nth 8 parse))
               (forward-line))
              (t
               (setq count (1- count)))))))
  (point))

(defun elisp-bundle-forward-sexp (&optional arg)
  "Move forward across ARG balanced group of parentheses.
Return new position if changed, nil otherwise."
  (elisp-bundle-move-with 'forward-sexp arg))

(defun elisp-bundle-backward-list (&optional n)
  "Move backward across N balanced group of parentheses.
Return new position if changed, nil otherwise."
  (elisp-bundle-move-with 'backward-list n))

(defun elisp-bundle-re-search-backward (regexp &optional bound noerror count)
  "Search backward from point for REGEXP ignoring elisp comments and strings.
Arguments BOUND, NOERROR, COUNT has the same meaning as `re-search-forward'."
  (elisp-bundle-re-search-forward regexp bound noerror (if count (- count) -1)))

(defun elisp-bundle-re-search-forward (regexp &optional bound noerror count)
  "Search forward from point for REGEXP ignoring elisp comments and strings.
Arguments BOUND, NOERROR, COUNT has the same meaning as `re-search-forward'."
  (unless count (setq count 1))
  (let ((init-point (point))
        (search-fun
         (cond ((< count 0)
                (setq count (- count))
                #'elisp-bundle-re-search-backward-inner)
               ((> count 0) #'elisp-bundle-re-search-forward-inner)
               (t #'ignore))))
    (condition-case err
        (funcall search-fun regexp bound count)
      (search-failed
       (goto-char init-point)
       (unless noerror
         (signal (car err)
                 (cdr err)))))))

(defun elisp-bundle-sexp-to-skip ()
  "Return non nil if sexp should be skipped."
  (when-let ((sexp (sexp-at-point)))
    (and
     (listp sexp)
     (memq (car sexp)
           '(eval-and-compile
              require eval-when-compile
              declare-function)))))

(defun elisp-bundle-find-place-to-insert ()
  "Jump to place where to insert new definition."
  (goto-char (point-min))
  (elisp-bundle-forward-sexp 1)
  (elisp-bundle-backward-list)
  (let ((prev-pos))
    (while (and (or (null prev-pos)
                    (< prev-pos (point)))
                (elisp-bundle-sexp-to-skip))
      (setq prev-pos (progn
                       (elisp-bundle-forward-sexp 2)
                       (elisp-bundle-backward-list)))))
  (when (elisp-bundle-sexp-to-skip)
    (elisp-bundle-forward-sexp 2)))

(defun elisp-bundle-extract-quoted-text (text)
  "Extract quoted name from TEXT."
  (let ((names))
    (with-temp-buffer
      (insert text)
      (while (re-search-backward "‘\\([^’]+\\)" nil t 1)
        (push (match-string-no-properties 1) names))
      names)))

(defun elisp-bundle-compile-current-buffer ()
  "Batch compile current buffer."
  (let ((temp-file (make-temp-file "elisp-bundle-flymake-byte-compile"))
        (command (expand-file-name invocation-name invocation-directory))
        (args))
    (save-restriction
      (widen)
      (write-region (point-min)
                    (point-max) temp-file nil 'nomessage))
    (setq args `("-Q"
                 "--batch"
                 ,@(mapcan (lambda (path)
                             (list "-L" path))
                           (append (list "./")
                                   load-path))
                 "--eval" "(batch-byte-compile)"
                 ,temp-file))
    (with-temp-buffer
      (cons (= 0 (apply #'call-process command nil t nil (remq nil args)))
            (string-trim (buffer-string))))))

(defun elisp-bundle-extract-undefs ()
  "Return cons of undefined functions and variables names in current buffer."
  (let ((lines (split-string (cdr (elisp-bundle-compile-current-buffer)) "\n"))
        (line)
        (vars)
        (fns))
    (while (setq line (pop lines))
      (cond ((string-match-p "is not known to be defined" line)
             (when-let ((names (elisp-bundle-extract-quoted-text line)))
               (setq fns (nconc fns names))))
            ((string-match-p "reference to free variable" line)
             (when-let ((names (elisp-bundle-extract-quoted-text line)))
               (setq vars (nconc vars names))))))
    (cons (seq-uniq fns)
          (seq-uniq vars))))

(defun elisp-bundle-copy-sexp (position)
  "Copy SEXP at POSITION."
  (goto-char position)
  (when-let ((bounds (bounds-of-thing-at-point
                      'sexp)))
    (buffer-substring-no-properties
     (car bounds)
     (cdr bounds))))

(defun elisp-bundle-find-definition (symbol)
  "Return a pair `(BUFFER . POINT)' pointing to the definition of SYMBOL."
  (or
   (ignore-errors
     (find-definition-noselect symbol nil))
   (ignore-errors
     (find-definition-noselect
      symbol 'defvar))))

(defun elisp-bundle--provided-feature ()
  "Return the last-provided feature name, as a string, or nil if none."
  (goto-char (point-max))
  (when (elisp-bundle-re-search-backward
         "[(]provide[\s\t\n\r\f]+'\\([^\s\t\n\r\f)]+\\)"
         nil t 1)
    (match-string-no-properties 1)))

(defun elisp-bundle-extract-undefs-in-buffer (buffer searched-names)
  "Return undefined definitions in BUFFER, exluding SEARCHED-NAMES."
  (let ((undefs (with-current-buffer buffer
                  (elisp-bundle-extract-undefs))))
    (or
     (seq-difference (car undefs) searched-names)
     (seq-difference (cdr undefs) searched-names))))

(defun elisp-bundle-insert-definition (definition)
  "Insert DEFINITION name in current buffer."
  (elisp-bundle-find-place-to-insert)
  (let ((str (concat
              (if (looking-back "\n\n" 0)
                  ""
                "\n")
              definition
              (if (looking-at "\n\n")
                  ""
                "\n"))))
    (insert str)))

;;;###autoload
(defun elisp-bundle-include-undefined ()
  "Insert defintions of unused functions or variables in current buffer.
Other available strategies - insert declare-functions form or require."
  (interactive)
  (let ((initial-buff (current-buffer))
        (searched-names)
        (libs-strategies)
        (names))
    (while (setq names (elisp-bundle-extract-undefs-in-buffer initial-buff
                                                              searched-names))
      (setq searched-names (nconc searched-names names))
      (let ((name))
        (while (setq name (pop names))
          (when-let ((definition
                      (when-let* ((sym (if (stringp name)
                                           (intern name)
                                         name))
                                  (cell (elisp-bundle-find-definition sym)))
                        (with-current-buffer (car cell)
                          (let* ((lib (elisp-bundle--provided-feature))
                                 (strategy
                                  (or
                                   (cdr (assoc lib libs-strategies))
                                   (let ((char (car
                                                (read-multiple-choice
                                                 (format "Strategy for %s " lib)
                                                 '((?r "require")
                                                   (?d "declare")
                                                   (?b "bundle")
                                                   (?n "nothing"))))))
                                     (push (cons lib char) libs-strategies)
                                     char))))
                            (pcase strategy
                              (?r (with-current-buffer
                                      initial-buff
                                    (goto-char (point-min))
                                    (unless (elisp-bundle-re-search-forward
                                             (concat "(require[\s\t\n]+'"
                                                     lib "[\s\t\n)]")
                                             nil t 1)
                                      (concat "(require '" lib ")"))))
                              (?b (elisp-bundle-copy-sexp (cdr cell)))
                              (?d (if (or (functionp sym)
                                          (macrop sym))
                                      (format "(declare-function %s \"%s\")"
                                              sym lib)
                                    (format "(defvar %s)" sym)))))))))
            (with-current-buffer initial-buff
              (elisp-bundle-insert-definition definition))))))))

(provide 'elisp-bundle)
;;; elisp-bundle.el ends here