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


(defvar elisp-bundle-overlay-cleanup-hooks-fns
  '((post-command-hook . t)
    (before-change-functions . t)
    (window-buffer-change-functions . nil)
    (window-selection-change-functions . nil)))

(defvar elisp-bundle-overlay nil
  "Overlay variable for `elisp-bundle-overlay-show'.")

(defun elisp-bundle-overlay-unset-and-remove (var-symbol)
  "Remove overlay from VAR-SYMBOL value."
  (when (overlayp (symbol-value var-symbol))
    (delete-overlay (symbol-value var-symbol)))
  (set var-symbol nil))

(defun elisp-bundle-make-overlay (start end &optional buffer front-advance
                                        rear-advance &rest props)
  "Create a new overlay with range BEG to END in BUFFER and return it.
If omitted, BUFFER defaults to the current buffer.
START and END may be integers or markers.
The fourth arg FRONT-ADVANCE, if non-nil, makes the marker
for the front of the overlay advance when text is inserted there
\(which means the text *is not* included in the overlay).
The fifth arg REAR-ADVANCE, if non-nil, makes the marker
for the rear of the overlay advance when text is inserted there
\(which means the text *is* included in the overlay).
PROPS is a plist to put on overlay."
  (let ((overlay (make-overlay start end buffer front-advance
                               rear-advance)))
    (dotimes (idx (length props))
      (when (eq (logand idx 1) 0)
        (let* ((prop-name (nth idx props))
               (val (plist-get props prop-name)))
          (overlay-put overlay prop-name val))))
    overlay))

(defun elisp-bundle-set-overlay (var-symbol start end &optional buffer
                                            front-advance rear-advance &rest
                                            props)
  "Create a new overlay and set value of VAR-SYMBOL to it.
If omitted, BUFFER defaults to the current buffer.
START and END may be integers or markers.
The fourth arg FRONT-ADVANCE, if non-nil, makes the marker
for the front of the overlay advance when text is inserted there
\(which means the text *is not* included in the overlay).
The fifth arg REAR-ADVANCE, if non-nil, makes the marker
for the rear of the overlay advance when text is inserted there
\(which means the text *is* included in the overlay).
PROPS is a plist to put on overlay."
  (elisp-bundle-overlay-unset-and-remove var-symbol)
  (set var-symbol (apply #'elisp-bundle-make-overlay
                         (append (list start end
                                       buffer front-advance
                                       rear-advance)
                                 props))))

(defun elisp-bundle-overlay-cleanup (&rest _args)
  "Remove overlay defined in `elisp-bundle-overlay'."
  (elisp-bundle-overlay-unset-and-remove 'elisp-bundle-overlay)
  (dolist (cell elisp-bundle-overlay-cleanup-hooks-fns)
    (remove-hook (car cell)
                 'elisp-bundle-overlay-cleanup (cdr cell))))

(defun elisp-bundle-overlay-add-hooks (ov)
  "Add cleanup hooks to overlay OV."
  (when-let ((buff (when (overlayp ov)
                     (overlay-buffer ov))))
    (when (buffer-live-p buff)
      (with-current-buffer (overlay-buffer ov)
        (dolist (cell elisp-bundle-overlay-cleanup-hooks-fns)
          (add-hook
           (car cell)
           #'elisp-bundle-overlay-cleanup
           nil (cdr cell)))))))

(defun elisp-bundle-overlay-higlight (start end &optional face)
  "Temporarily highlight region from START to END with FACE.
Default value of TIMEOUT is 0 seconds."
  (elisp-bundle-overlay-cleanup)
  (elisp-bundle-set-overlay 'elisp-bundle-overlay
                            start end nil nil nil 'face
                            (or face 'success))
  (run-with-timer 0 nil
                  #'elisp-bundle-overlay-add-hooks
                  elisp-bundle-overlay))

(defun elisp-bundle--unquote (exp)
  "Return EXP unquoted."
  (declare (pure t)
           (side-effect-free t))
  (while (memq (car-safe exp) '(quote function))
    (setq exp (cadr exp)))
  exp)

(defun elisp-bundle--parse-require ()
  "Parse list at point and return alist of form (symbol-name args doc deftype).
E.g. (\"elisp-bundle--parse-list-at-point\" (arg) \"Doc string\" defun)"
  (when-let ((sexp (unless (or (nth 4 (syntax-ppss (point)))
                               (nth 3 (syntax-ppss (point))))
                     (sexp-at-point))))
    (when (listp sexp)
      (elisp-bundle--format-sexp-to-require sexp))))

(defmacro elisp-bundle--with-temp-lisp-buffer (&rest body)
  "Execute BODY in temp buffer with Emacs Lisp mode without hooks."
  (declare (indent 2)
           (debug t))
  `(with-temp-buffer
     (erase-buffer)
     (let (emacs-lisp-mode-hook)
       (emacs-lisp-mode))
     (progn
       ,@body)))

(defun elisp-bundle--format-sexp-to-require (sexp)
  "Return string with package name if SEXP is valid require call.
If package is optional, also add suffix (optional)."
  (pcase sexp
    (`(require ,(and name
                     (guard (listp name))
                     (guard (eq (car-safe name) 'quote))))
     (cons (elisp-bundle--unquote name) nil))
    (`(require ,(and name
                     (guard (listp name))
                     (guard (eq (car-safe name) 'quote)))
               ,_)
     (cons (elisp-bundle--unquote name) nil))
    (`(require ,(and name
                     (guard (listp name))
                     (guard (eq (car-safe name) 'quote)))
               ,_
               ,(and optional (guard (not (eq optional nil)))))
     (cons (elisp-bundle--unquote name) t))))

(defun elisp-bundle--backward-list (&optional n)
  "Move backward across N balanced group of parentheses.
Return new position if changed, nil otherwise."
  (let ((pos (point))
        (end))
    (setq end (ignore-errors
                (backward-list (or n 1))
                (point)))
    (unless (equal pos end)
      end)))

(defun elisp-bundle-confirm-replace (beg end &optional replacement)
  "Make overlay between BEG and END and show REPLACEMENT at the END."
  (let ((cover-ov (make-overlay beg end)))
    (unwind-protect
        (progn
          (overlay-put cover-ov 'face 'error)
          (when (and replacement (not (string-empty-p replacement)))
            (overlay-put cover-ov 'after-string
                         (propertize (concat "\s"
                                             replacement)
                                     'face 'success)))
          (yes-or-no-p (if replacement "Replace?" "Remove?")))
      (delete-overlay cover-ov))))

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

(defun elisp-bundle-backward-up-list (&optional n)
  "Move backward across N balanced group of parentheses.
Return new position if changed, nil otherwise."
  (elisp-bundle-move-with 'backward-up-list n))

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
  (with-temp-buffer
    (insert text)
    (when (re-search-backward "‘\\([^’]+\\)" nil t 1)
      (match-string-no-properties 1))))

;;;###autoload
(defun elisp-bundle-byte-compile-buffer (&optional load)
  "Byte compile the current buffer, but don't write a file.
If LOAD is non-nil, load byte-compiled data.  When called
interactively, this is the prefix argument."
  (interactive "P")
  (let ((bfn buffer-file-name)
        file elc)
    (require 'bytecomp)
    (unwind-protect
        (progn
          (setq file (make-temp-file "compile" nil ".el")
                elc (funcall byte-compile-dest-file-function file))
          (write-region (point-min)
                        (point-max) file nil 'silent)
          (let ((set-message-function
                 (lambda (message)
                   (when (string-match-p "\\`Wrote " message)
                     'ignore)))
                (byte-compile-log-warning-function
                 (lambda (string position &optional fill level)
                   (if bfn
                       ;; Massage the warnings to that they point to
                       ;; this file, not the one in /tmp.
                       (let ((byte-compile-current-file bfn)
                             (byte-compile-root-dir (file-name-directory bfn)))
                         (byte-compile--log-warning-for-byte-compile
                          string position fill level))
                     ;; We don't have a file name, so the warnings
                     ;; will point to a file that doesn't exist.  This
                     ;; should be fixed in some way.
                     (byte-compile--log-warning-for-byte-compile
                      string position fill level)))))
            (byte-compile-file file))
          (when (and bfn (get-buffer "*Compile-Log*"))
            (with-current-buffer "*Compile-Log*"
              (setq default-directory (file-name-directory bfn))))
          (if load
              (load elc)
            (message "Byte-compiled the current buffer")))
      (when file
        (when (file-exists-p file)
          (delete-file file))
        (when (file-exists-p elc)
          (delete-file elc))))))

(defun elisp-bundle-compile-current-buffer ()
  "Batch compile current buffer."
  (let* ((temp-file (make-temp-file "elisp-bundle-flymake-byte-compile"))
         (temp-file-re (regexp-quote temp-file))
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
      (let ((status (apply #'call-process command nil t nil (remq nil args))))
        (if (zerop status)
            (mapcar
             (apply-partially #'replace-regexp-in-string temp-file-re "")
             (split-string (string-trim (buffer-string)) "\n" t))
          (minibuffer-message (buffer-string))
          nil)))))

(defun elisp-bundle-extract-undefs ()
  "Return cons of undefined functions and variables name in current buffer."
  (let ((lines (elisp-bundle-compile-current-buffer))
        (line)
        (vars)
        (fns))
    (while (setq line (pop lines))
      (cond ((or (string-match-p "is not known to be defined" line)
                 (string-match-p "might not be defined at runtime" line))
             (when-let
                 ((name (elisp-bundle-extract-quoted-text line))
                  (n (ignore-errors (string-to-number (car (split-string
                                                            line ":" t))))))
               (setq fns (push (cons name n) fns))))
            ((string-match-p "reference to free variable" line)
             (when-let ((name (elisp-bundle-extract-quoted-text line))
                        (n (ignore-errors (string-to-number (car
                                                             (split-string
                                                              line ":" t))))))
               (setq vars (push (cons name n) vars))))))
    (when (or fns
              vars)
      (cons (seq-uniq fns)
            (seq-uniq vars)))))

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

(defvar-local elisp-bundle-top-level-sexps nil)
(defvar-local elisp-bundle-top-level-tick nil)
(defun elisp-bundle-scan-top-level-lists (buffer)
  "Scan and return top-level s-expressions from a given BUFFER.

Argument BUFFER is a buffer object."
  (with-current-buffer buffer
    (let ((tick (buffer-modified-tick)))
      (if (eq elisp-bundle-top-level-tick tick)
          elisp-bundle-top-level-sexps
        (setq elisp-bundle-top-level-tick tick)
        (let ((sexps)
              (sexp))
          (goto-char (point-min))
          (while (setq sexp (ignore-errors (read (current-buffer))))
            (push sexp sexps))
          (setq elisp-bundle-top-level-sexps (nreverse sexps)))))))

(defun elisp-bundle-find-provide-sexp (sexps)
  "Find and return the symbol provided by the given SEXPS.

Argument SEXPS is a list of S-expressions."
  (let ((items (reverse sexps))
        (found))
    (while (and items (not found))
      (let ((curr (car items)))
        (pcase curr
          (`(provide ',(and sym (guard (symbolp sym))))
           (setq found curr))
          (`(provide ',(and sym (guard (symbolp sym))) . _)
           (setq found curr))))
      (setq items (cdr items)))
    (cadadr found)))

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
              (if (looking-at "\n")
                  ""
                "\n"))))
    (insert str)))

(defun elisp-bundle-jump-to-line (line)
  "Jump to LINE."
  (goto-char (point-min))
  (forward-line (1- line)))

(defun elisp-bundle-jump-to-body-start ()
  "Parse list at point and return alist of form (symbol-name args doc deftype).
E.g. (\"autofix-parse-list-at-point\" (arg) \"Doc string\" defun)"
  (let ((found nil))
    (when-let ((start (car-safe (nth 9 (syntax-ppss (point))))))
      (goto-char start)
      (when-let* ((sexp (sexp-at-point))
                  (sexp-type (car-safe sexp)))
        (not (setq found (and (or (eq sexp-type 'defun)
                                  (eq sexp-type 'cl-defun))
                              (and (listp sexp)
                                   (symbolp (nth 1 sexp))
                                   (listp (nth 2 sexp))))))
        (when found
          (down-list)
          (forward-sexp 3)
          (skip-chars-forward "\s\t\n")
          (when (looking-at "\"")
            (forward-sexp 1)
            (skip-chars-forward "\s\t\n"))
          (when (looking-at "[(]interactive[^a-z]")
            (forward-sexp 1)
            (skip-chars-forward "\s\t\n"))
          (point))))))

(defun elisp-bundle-get-requires-from-sexp (sexp)
  "Extract list of unquoted features required in SEXP."
  (let ((requires))
    (while (and (listp sexp)
                (setq sexp (memq 'require (flatten-list
                                           sexp))))
      (pop sexp)
      (when (and (eq (car-safe sexp) 'quote)
                 (symbolp (nth 1 sexp)))
        (push (nth 1 sexp) requires)))
    requires))

(defun elisp-bundle-add-fbound (name line)
  "Search for NAME on the LINE and add to the parent list `fbound' check."
  (elisp-bundle-jump-to-line line)
  (when (re-search-forward (regexp-quote name)
                           (line-end-position) t 1)
    (when-let* ((start (elisp-bundle-backward-up-list))
                (end (progn (forward-sexp 1)
                            (point)))
                (content (buffer-substring-no-properties
                          start end)))
      (when (elisp-bundle-confirm-replace start end
                                          (format "(when (fboundp '%s\n%s))"
                                                  name content))
        (delete-region start end)
        (insert (format "(when (fboundp '%s))"
                        name))
        (forward-char -1)
        (newline-and-indent)
        (insert content))
      (when-let ((lib
                  (ignore-errors
                    (with-current-buffer
                        (car-safe (find-definition-noselect
                                   (intern
                                    name)
                                   nil))
                      (elisp-bundle--provided-feature)))))
        (let ((found nil))
          (when-let ((start (car-safe (nth 9 (syntax-ppss (point))))))
            (goto-char start)
            (when-let* ((sexp (sexp-at-point))
                        (sexp-type (car-safe sexp)))
              (not (setq found (and (or (eq sexp-type 'defun)
                                        (eq sexp-type 'cl-defun))
                                    (and (listp sexp)
                                         (symbolp (nth 1 sexp))
                                         (listp (nth 2 sexp))))))
              (when found
                (let ((libs (mapcar #'symbol-name
                                    (elisp-bundle-get-requires-from-sexp
                                     (sexp-at-point)))))
                  (unless (member lib libs)
                    (down-list)
                    (forward-sexp 3)
                    (skip-chars-forward "\s\t\n")
                    (when (looking-at "\"")
                      (forward-sexp 1)
                      (skip-chars-forward "\s\t\n"))
                    (when (looking-at "[(]interactive[^a-z]")
                      (forward-sexp 1)
                      (skip-chars-forward "\s\t\n"))
                    (when (elisp-bundle-confirm-replace (point)
                                                        (point)
                                                        (format
                                                         "(require '%s)\n" lib))
                      (insert (format "(require '%s)\n" lib)))))))))))))

(defun elisp-bundle-eval-requires ()
  "Evaluate all require all statements in current buffer."
  (let ((requires
         (let ((requires '())
               (deps))
           (save-excursion
             (goto-char (point-max))
             (while (elisp-bundle--backward-list)
               (when-let ((sexp (unless (nth 4 (syntax-ppss (point)))
                                  (list-at-point))))
                 (if-let ((dep
                           (elisp-bundle--format-sexp-to-require
                            sexp)))
                     (push dep requires)
                   (when (listp sexp)
                     (push sexp deps))))))
           (when deps
             (elisp-bundle--with-temp-lisp-buffer
                 (insert (prin1-to-string deps))
                 (while (re-search-backward "[(]require[\s\t\n\r\f]+'"
                                            nil t 1)
                   (when-let ((found (elisp-bundle--parse-require)))
                     (unless (member found requires)
                       (push found requires))))))
           requires)))
    (dolist (lib requires)
      (require (car lib) nil t))))


;;;###autoload
(defun elisp-bundle-include-undefined ()
  "Insert defintions of unused functions or variables in current buffer.
Other available strategies - insert declare-functions form or require."
  (interactive)
  (let ((initial-buff (current-buffer))
        (searched-names)
        (libs-strategies)
        (names))
    (elisp-bundle-eval-requires)
    (while (setq names (elisp-bundle-extract-undefs-in-buffer initial-buff
                                                              searched-names))
      (setq searched-names (nconc searched-names names))
      (let ((name-cell))
        (while (setq name-cell (pop names))
          (let ((name (car name-cell))
                (line (cdr name-cell)))
            (with-current-buffer initial-buff
              (elisp-bundle-jump-to-line line)
              (elisp-bundle-overlay-higlight
               (line-beginning-position)
               (line-end-position) 'error))
            (unless (get-buffer-window initial-buff)
              (pop-to-buffer initial-buff))
            (when-let ((definition
                        (pcase-let* ((sym (if (stringp name)
                                              (intern name)
                                            name))
                                     (`(,lib-buff . ,position)
                                      (and sym
                                           (elisp-bundle-find-definition sym)))
                                     (sexps (and (buffer-live-p lib-buff)
                                                 (elisp-bundle-scan-top-level-lists
                                                  lib-buff)))
                                     (lib-sym (elisp-bundle-find-provide-sexp
                                               sexps))
                                     (lib (and lib-sym (symbol-name lib-sym)))
                                     (strategy
                                      (and lib
                                           (or
                                            (cdr (assoc lib libs-strategies))
                                            (let ((char (car
                                                         (read-multiple-choice
                                                          (format
                                                           "Strategy for %s (%s) in %s"
                                                           name lib initial-buff)
                                                          '((?r "require")
                                                            (?f "fbound")
                                                            (?d "declare")
                                                            (?b "bundle")
                                                            (?n "nothing"))))))
                                              (push (cons lib char)
                                                    libs-strategies)
                                              char)))))
                          (pcase strategy
                            (?r (with-current-buffer
                                    initial-buff
                                  (goto-char (point-min))
                                  (unless (elisp-bundle-re-search-forward
                                           (concat "(require[\s\t\n]+'"
                                                   lib "[\s\t\n)]")
                                           nil t 1)
                                    (concat "(require '" lib ")"))))
                            (?f (with-current-buffer initial-buff
                                  (elisp-bundle-add-fbound name line)))
                            (?b (with-current-buffer lib-buff
                                  (elisp-bundle-copy-sexp position)))
                            (?d (if (or (functionp sym)
                                        (macrop sym))
                                    (format "(declare-function %s \"%s\")"
                                            sym lib)
                                  (format "(defvar %s)" sym)))))))
              (with-current-buffer initial-buff
                (elisp-bundle-insert-definition definition)))))))))

(provide 'elisp-bundle)
;;; elisp-bundle.el ends here