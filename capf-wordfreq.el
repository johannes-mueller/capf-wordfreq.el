;;; capf-wordfreq.el --- Capf backend for human language texts -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Johannes Mueller

;; Author: Johannes Mueller <github@johannes-mueller.org>
;; URL: https://github.com/johannes-mueller/capf-wordfreq.el
;; Version: 0.1.0
;; Keywords: capf, completion, convenience, matching
;; Package-Requires: ((emacs "27.1")

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation version 2. <https://www.gnu.org/licenses/>

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

(defun capf-wordfreq--default-path ()
  "Set up the default for `capf-wordfreq-path'."
  (concat (file-name-as-directory user-emacs-directory) "wordfreq-dicts"))

(defcustom capf-wordfreq-path (capf-wordfreq--default-path)
  "Path where the dictionary files reside.

The dictionary files are expected to have the name <language>.txt
where <language> is the contents of `ispell-local-dictionary' in
the current buffer."
  :type 'string
  :group 'capf)

(defun capf-wordfreq--dictionary ()
  "Determine the path of the word list file."
  (concat (file-name-as-directory capf-wordfreq-path) ispell-local-dictionary ".txt"))

(defvar capf-wordfreq--cached-grep-executable nil)

(defun capf-wordfreq--find-grep-executable ()
  (or capf-wordfreq--cached-grep-executable
      (executable-find "grep")))

(defun capf-wordfreq--grep-executable ()
  (capf-wordfreq--find-grep-executable))

(defun capf-wordfreq--shell-command (prefix)
  (concat
   (capf-wordfreq--grep-executable)
   " -i "
   (shell-quote-argument (concat "^" prefix))
   " " (capf-wordfreq--dictionary)))

(defun capf-wordfreq--fetch-candidates-raw (prefix)
  (shell-command-to-string (capf-wordfreq--shell-command prefix)))

(defun capf-wordfreq--enforce-exact-prefix (cand prefix)
  (concat prefix (substring cand (length prefix) nil)))

(defun capf-wordfreq--drop-empty-last-candidate (candlist)
  (if (equal (nth (1- (length candlist)) candlist) "")
      (butlast candlist)
    candlist))

(defun capf-wordfreq--candidates (prefix)
  "Fetches the candidates matching PREFIX."
  (mapcar (lambda (cand) (capf-wordfreq--enforce-exact-prefix cand prefix))
          (capf-wordfreq--drop-empty-last-candidate
           (split-string (capf-wordfreq--fetch-candidates-raw prefix) "\n"))))

;;;###autoload
(defun capf-wordfreq-completion-at-point-function ()
  (let* ((bounds (bounds-of-thing-at-point 'word))
	 (beg (car bounds))
	 (end (cdr bounds))
	 (prefix (buffer-substring-no-properties beg end)))
    `(,beg ,end ,(capf-wordfreq--candidates prefix))))

(provide 'capf-wordfreq)
;;; capf-wordfreq.el ends here
