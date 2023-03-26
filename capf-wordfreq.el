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

(defvar-local capf-wordfreq--begin nil)
(defvar-local capf-wordfreq--cands nil)

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

(defcustom capf-wordfreq-minimal-candidate-length 0
  "The minimal length of the candidates"
  :type 'integer
  :group 'capf)

(defun capf-wordfreq--dictionary ()
  "Determine the path of the word list file."
  (when-let* ((dct ispell-local-dictionary)
              (dct-file (concat (file-name-as-directory capf-wordfreq-path) dct ".txt")))
    (if (file-exists-p dct-file) (expand-file-name dct-file))))

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
  (if (capf-wordfreq--dictionary)
      (shell-command-to-string (capf-wordfreq--shell-command prefix))
    ""))

(defun capf-wordfreq--enforce-exact-prefix (cand prefix)
  (concat prefix (substring cand (length prefix) nil)))

(defun capf-wordfreq--drop-empty-last-candidate (candlist)
  (if (equal (nth (1- (length candlist)) candlist) "")
      (butlast candlist)
    candlist))

(defun capf-wordfreq--candidates (prefix)
  "Fetches the candidates matching PREFIX."
  (mapcar
   (lambda (cand) (capf-wordfreq--enforce-exact-prefix cand prefix))
   (seq-filter
    (lambda (cand) (>= (length cand) capf-wordfreq-minimal-candidate-length))
    (capf-wordfreq--drop-empty-last-candidate
     (split-string (capf-wordfreq--fetch-candidates-raw prefix) "\n")))))

(defvar-local capf-wordfreq--msg-fragment "")

(defun capf-wordfreq--process-msg (msg)
  (let* ((last-message (car (last (split-string (string-trim-right msg) "[\n]"))))
         (fields (split-string last-message))
         (beg (string-to-number (car fields)))
         (prefix (car (cdr fields)))
         (cands (mapcar
                 (lambda (cand) (capf-wordfreq--enforce-exact-prefix cand prefix))
                 (cddr fields))))
    (setq capf-wordfreq--msg-fragment "")
    (setq capf-wordfreq--begin (when cands beg)
          capf-wordfreq--cands cands)))

(defun capf-wordfreq--return-buffer-filter (_proc msg)
  (if (string-suffix-p "\n" msg)
      (capf-wordfreq--process-msg (concat capf-wordfreq--msg-fragment msg))
    (setq capf-wordfreq--msg-fragment (concat capf-wordfreq--msg-fragment msg))))


(defun capf-wordfreq--external-process-observer (&rest _)
  (if-let* ((dict-file (capf-wordfreq--dictionary))
            (bounds (bounds-of-thing-at-point 'word))
	    (beg (car bounds))
	    (end (cdr bounds))
	    (prefix (buffer-substring-no-properties beg end)))
      (process-send-string
       (capf-wordfreq--start-external-process)
       (format "%s %s %s\n" beg dict-file prefix))
    (setq capf-wordfreq--begin nil
          capf-wordfreq--cands nil)))


(defconst capf-wordfreq--script "
while read -r line; do
    split=($line)
    beg=${split[0]}
    dictfile=${split[1]}
    prefix=${split[2]}
    grep_command='grep -i ^\"$prefix\" $dictfile | tr \"\\n\" \" \"'
    words=\$(eval $grep_command)
    echo \"$beg $prefix $words\"
done
")

(defvar capf-wordfreq--external-process nil)
(defvar capf-wordfreq--begin nil)
(defvar capf-wordfreq--cands nil)

(defun capf-wordfreq--start-external-process ()
  (unless (capf-wordfreq--external-process-live-p)
    (ignore-errors (delete-process capf-wordfreq--external-process))
    (setq capf-wordfreq--external-process
          (make-process :name "capf-wordfreq-external"
                        :command `("/bin/bash" "-c" ,capf-wordfreq--script)
                        :noquery t
                        :stderr "*external-stderr*"
                        :filter #'capf-wordfreq--return-buffer-filter)))
  capf-wordfreq--external-process)


(defun capf-wordfreq--external-process-live-p ()
  (process-live-p capf-wordfreq--external-process))


(defun capf-wordfreq--enforce-min-length (cands)
  (seq-filter
   (lambda (cand) (>= (length cand) capf-wordfreq-minimal-candidate-length))
   cands))


;;;###autoload
(defun capf-wordfreq-completion-at-point-function ()
  "The completion at point function."
  (if (and capf-wordfreq--begin capf-wordfreq--cands)
      (list capf-wordfreq--begin (point) (capf-wordfreq--enforce-min-length capf-wordfreq--cands))
    (add-hook 'after-change-functions #'capf-wordfreq--external-process-observer nil 'local)))


(provide 'capf-wordfreq)
;;; capf-wordfreq.el ends here
