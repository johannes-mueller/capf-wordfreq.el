;;; capf-wordfreq.el-test.el --- Tests for capf-wordfreq

;;; Commentary:

;; usually run by ert-runner

;;; Code:

(require 'mocker)
(require 'capf-wordfreq)

(ert-deftest test-dict-path-esperanto ()
  (let ((native-comp-enable-subr-trampolines nil))
    (mocker-let ((file-exists-p (filename) ((:input '("/path/to/dicts/esperanto.txt") :output t))))
      (let ((ispell-local-dictionary "esperanto")
            (capf-wordfreq-path "/path/to/dicts"))
        (should (equal (capf-wordfreq--dictionary) "/path/to/dicts/esperanto.txt"))))))

(ert-deftest test-dict-path-english ()
  (let ((native-comp-enable-subr-trampolines nil))
    (mocker-let ((file-exists-p (filename) ((:input '("/other/path/to/dicts/english.txt") :output t))))
      (let ((ispell-local-dictionary "english")
            (capf-wordfreq-path "/other/path/to/dicts"))
        (should (equal (capf-wordfreq--dictionary) "/other/path/to/dicts/english.txt"))))))

(ert-deftest test-no-ispell-dictionary-dict-path-nil ()
  (let ((ispell-local-dictionary nil))
    (should (equal (capf-wordfreq--dictionary) nil))))

(ert-deftest test-candidates-filter ()
  (dolist (struct '(("23 foo foo foobar\n" 23 ("foo" "foobar"))
                    ("42 bar barfoo bar\n" 42 ("barfoo" "bar"))
                    ("23 Foo foo foobar\n" 23 ("Foo" "Foobar"))))
    (let ((contents (pop struct))
          (expected-begin (pop struct))
          (expected-cands (pop struct))
          (capf-wordfreq--begin nil)
          (capf-wordfreq--cands nil))
      (insert contents)
      (capf-wordfreq--return-buffer-filter nil contents)
      (should (equal capf-wordfreq--begin expected-begin))
      (should (equal capf-wordfreq--cands expected-cands)))))

(ert-deftest test-candidates-filter-two-messages ()
  (capf-wordfreq--return-buffer-filter nil "23 foo foo foobar\n42 bar barfoo bar\n")
    (should (equal capf-wordfreq--begin 42))
      (should (equal capf-wordfreq--cands '("barfoo" "bar"))))


(ert-deftest test-candidates-filter-fragmented-message ()
  (let ((capf-wordfreq--begin 23)
        (capf-wordfreq--cands '("foo" "foobar")))
    (capf-wordfreq--return-buffer-filter nil "23 Foo foo")
    (should (equal capf-wordfreq--begin 23))
    (should (equal capf-wordfreq--cands '("foo" "foobar")))
    (should (equal capf-wordfreq--msg-fragment "23 Foo foo"))))


(ert-deftest test-candidates-filter-fragmented-message-second-part ()
  (let ((capf-wordfreq--msg-fragment "23 Foo foo"))
    (capf-wordfreq--return-buffer-filter nil " foobar\n")
    (should (equal capf-wordfreq--begin 23))
    (should (equal capf-wordfreq--cands '("Foo" "Foobar")))
    (should (equal capf-wordfreq--msg-fragment ""))))


(ert-deftest test-candidates-filter-fragmented-message-third-part ()
  (let ((capf-wordfreq--msg-fragment "23 Foo foo"))
    (capf-wordfreq--return-buffer-filter nil " foo")
    (capf-wordfreq--return-buffer-filter nil "bar\n")
    (should (equal capf-wordfreq--begin 23))
    (should (equal capf-wordfreq--cands '("Foo" "Foobar")))
    (should (equal capf-wordfreq--msg-fragment ""))))


(ert-deftest test-candidates-filter-no-candidates ()
  (capf-wordfreq--return-buffer-filter nil "23 foo")
    (should (equal capf-wordfreq--begin nil))
      (should (equal capf-wordfreq--cands nil)))


(ert-deftest test-observer-foo ()
  (mocker-let ((capf-wordfreq--start-external-process () ((:input '() :output 'start-external-process)))
               (capf-wordfreq--dictionary () ((:input '() :output "/foo/bar/esperanto.txt")))
               (process-send-string (process string) ((:input '(start-external-process "1 /foo/bar/esperanto.txt foo\n")))))
    (with-temp-buffer
      (insert "foo")
      (capf-wordfreq--external-process-observer 'foo))))

(ert-deftest test-observer-bar ()
  (mocker-let ((capf-wordfreq--start-external-process () ((:input '() :output 'start-external-process)))
               (capf-wordfreq--dictionary () ((:input '() :output "/foo/bar/deutsch.txt")))
               (process-send-string (process string) ((:input '(start-external-process "5 /foo/bar/deutsch.txt bar\n")))))
    (with-temp-buffer
      (insert "foo bar")
      (capf-wordfreq--external-process-observer 'foo))))


(ert-deftest test-observer-dict-file-not-found ()
  (mocker-let ((capf-wordfreq--dictionary () ((:output nil)))
               (process-send-string (process string) ((:input '() :occur 0))))
    (with-temp-buffer
      (insert "foo bar")
      (capf-wordfreq--external-process-observer 'foo))))


(ert-deftest test-observer-nil ()
  (mocker-let ((capf-wordfreq--dictionary () ((:output "/foo/bar/english.txt")))
               (process-send-string (process string) ((:input '() :occur 0))))
    (let ((capf-wordfreq--begin 17)
          (capf-wordfreq--cands '("foo" "foobar")))
      (with-temp-buffer
        (insert "foo bar ")
        (capf-wordfreq--external-process-observer 'foo)
        (should (eq capf-wordfreq--begin nil))
        (should (eq capf-wordfreq--cands nil))))))


(ert-deftest test-start-external-process-still-live ()
  (mocker-let ((capf-wordfreq--external-process-live-p () ((:input '() :output t))))
    (let ((capf-wordfreq--external-process 'foo))
      (should (eq (capf-wordfreq--start-external-process) 'foo)))
    (let ((capf-wordfreq--external-process 'bar))
      (should (eq (capf-wordfreq--start-external-process) 'bar)))))


(ert-deftest test-start-external-process-not-live ()
  (setq make-process-prms `(:name "capf-wordfreq-external"
                            :command ,`("/bin/bash" "-c" ,capf-wordfreq--script)
                            :noquery t
                            :stderr "*external-stderr*"
                            :filter capf-wordfreq--return-buffer-filter))
  (mocker-let ((capf-wordfreq--external-process-live-p () ((:input '() :output nil)))
               (delete-process (process) ((:input '(bfoo) :output nil :occur 1)
                                          (:input '(bbar) :output nil :occur 1)))
               (make-process (&rest _) ((:input make-process-prms :output 'foo :occur 1)
                                        (:input make-process-prms :output 'bar :occur 1))))
    (let ((capf-wordfreq--external-process 'bfoo))
      (should (eq (capf-wordfreq--start-external-process) 'foo))
      (should (eq capf-wordfreq--external-process 'foo)))
    (let ((capf-wordfreq--external-process 'bbar))
      (should (eq (capf-wordfreq--start-external-process) 'bar))
      (should (eq capf-wordfreq--external-process 'bar)))))


(ert-deftest test-external-process-live-p ()
  (mocker-let ((process-live-p (process) ((:input '(foo) :output t)
                                          (:input '(bar) :output nil))))
    (let ((capf-wordfreq--external-process 'foo))
      (should (capf-wordfreq--external-process-live-p)))
    (let ((capf-wordfreq--external-process 'bar))
      (should (not (capf-wordfreq--external-process-live-p))))))


(ert-deftest test-capf-wordfreq-completion-at-point-function-nil ()
  (mocker-let ((add-hook (hook function depth local) ((:input
                                                       '(after-change-functions
                                                         capf-wordfreq--external-process-observer
                                                         nil
                                                         local)))))
    (let ((capf-wordfreq--begin nil)
          (capf-wordfreq--cands nil))
      (should (eq (capf-wordfreq-completion-at-point-function) nil)))))


(ert-deftest test-capf-wordfreq-completion-at-point-function-nonnil ()
  (let ((capf-wordfreq--begin 23)
        (capf-wordfreq--cands '("foobar" "foo")))
    (mocker-let ((point () ((:input '() :output 25))))
      (should (equal (capf-wordfreq-completion-at-point-function) '(23 25 ("foobar" "foo"))))))
  (let ((capf-wordfreq--begin 42)
        (capf-wordfreq--cands '("barfoo" "bar")))
    (mocker-let ((point () ((:input '() :output 47))))
      (should (equal (capf-wordfreq-completion-at-point-function) '(42 47 ("barfoo" "bar")))))))


(ert-deftest test-capf-wordfreq-completion-at-point-function-minimal-candidate-length ()
  (let ((capf-wordfreq--begin 23)
        (capf-wordfreq--cands '("foobar" "foo"))
        (capf-wordfreq-minimal-candidate-length 4))
    (mocker-let ((point () ((:input '() :output 25))))
      (should (equal (capf-wordfreq-completion-at-point-function) '(23 25 ("foobar")))))))


(ert-deftest test-wordfreq-path-default ()
  (let ((user-emacs-directory "/home/user/.emacs.d"))
    (should (equal (capf-wordfreq--default-path) "/home/user/.emacs.d/wordfreq-dicts"))))

(ert-deftest test-language-proposal-list ()
  (let ((capf-wordfreq--language-alist '(("esperanto" . "eo")
                                            ("english" . "en"))))
    (should (equal (capf-wordfreq--proposal-list) '("esperanto" "english")))))

(ert-deftest test-language-iso-code-list ()
  (let ((capf-wordfreq--language-alist '(("esperanto" . "eo")
                                            ("english" . "en"))))
    (should (equal (capf-wordfreq--iso-code "esperanto") "eo"))))

(ert-deftest test-make-url-full ()
  (should (equal (capf-wordfreq--dict-url "eo" "full")
                 "https://raw.githubusercontent.com/johannes-mueller/FrequencyWords/master/content/2018/eo/eo_full.txt")))

(ert-deftest test-make-url-50k ()
  (should (equal (capf-wordfreq--dict-url "eo" "50k")
                 "https://raw.githubusercontent.com/johannes-mueller/FrequencyWords/master/content/2018/eo/eo_50k.txt")))

(defun return-code (code)
  (let ((buffer (generate-new-buffer "urltmp")))
    (with-current-buffer buffer
      (insert (concat "HTTP/1.1 " code "\nother headers")))
    buffer))

(ert-deftest test-probe-50k-word-list-true ()
  (mocker-let
      ((url-retrieve-synchronously (url inhibit-cookies)
                                   ((:input
                                     '("http://example.com/eo_50k.txt" :inhibit-cookies)
                                     :output (return-code "200 OK"))))
       (capf-wordfreq--dict-url (lang-code kind)
                                   ((:input '("eo" "50k") :output "http://example.com/eo_50k.txt"))))
    (should (capf-wordfreq--probe-50k "eo"))))

(ert-deftest test-probe-50k-word-list-false ()
  (mocker-let
      ((url-retrieve-synchronously (url inhibit-cookies)
                                   ((:input
                                     '("http://example.com/eo_50k.txt" :inhibit-cookies)
                                     :output (return-code "404 Not Found"))))
       (capf-wordfreq--dict-url (lang-code kind)
                                   ((:input '("eo" "50k") :output "http://example.com/eo_50k.txt"))))
    (should-not (capf-wordfreq--probe-50k "eo"))))

(ert-deftest test-drop-frequency-values ()
  (with-temp-buffer
    (insert "mi 16311
vi 13927
ne 11163
estas 10726
")
    (capf-wordfreq--drop-frequency-values)
    (should (equal (buffer-string) "mi\nvi\nne\nestas\n"))))

(ert-deftest test-fetch-short ()
  (mocker-let ((y-or-n-p (str) ((:input '("Use reduced length 50k words? ")))))
    (capf-wordfreq--prompt-fetch-short)))

(ert-deftest test-download-new-word-list-no50k ()
  (let ((capf-wordfreq--word-list-buffer nil))
    (mocker-let ((capf-wordfreq--proposal-list () ((:output '("esperanto" "english"))))
                (completing-read (prompt choices)
                                 ((:input
                                   '("Choose language: " ("esperanto" "english"))
                                   :output "esperanto")))
                (capf-wordfreq--iso-code (language) ((:input '("esperanto") :output "eo")))
                (capf-wordfreq--probe-50k (lang-code) ((:input '("eo") :output nil)))
                (capf-wordfreq--prompt-fetch-short () ((:occur 0)))
                (capf-wordfreq--dict-url (lang-code kind)
                                            ((:input '("eo" "full") :output "http://example.com/eo_full.txt")))
                (url-retrieve (url callback language inhibit-cookies)
                              ((:input
                                '("http://example.com/eo_full.txt"
                                  capf-wordfreq--list-retrieved-callback
                                  ("esperanto")
                                  :inhibit-cookies)
                                :output 'buffer))))
     (capf-wordfreq-download-list)
     (should (eq capf-wordfreq--word-list-buffer 'buffer)))))

(ert-deftest test-download-new-word-list-50k-no ()
  (let ((capf-wordfreq--word-list-buffer nil))
    (mocker-let ((capf-wordfreq--proposal-list () ((:output '("esperanto" "english"))))
                (completing-read (prompt choices)
                                 ((:input
                                   '("Choose language: " ("esperanto" "english"))
                                   :output "esperanto")))
                (capf-wordfreq--iso-code (language) ((:input '("esperanto") :output "eo")))
                (capf-wordfreq--probe-50k (lang-code) ((:input '("eo") :output t)))
                (capf-wordfreq--prompt-fetch-short () ((:output nil)))
                (capf-wordfreq--dict-url (lang-code kind)
                                            ((:input '("eo" "full") :output "http://example.com/eo_full.txt")))
                (url-retrieve (url callback language inhibit-cookies)
                              ((:input
                                '("http://example.com/eo_full.txt"
                                  capf-wordfreq--list-retrieved-callback
                                  ("esperanto")
                                  :inhibit-cookies)
                                :output 'buffer))))
     (capf-wordfreq-download-list)
     (should (eq capf-wordfreq--word-list-buffer 'buffer)))))

(ert-deftest test-download-new-word-list-50k-yes ()
  (let ((capf-wordfreq--word-list-buffer nil))
    (mocker-let ((capf-wordfreq--proposal-list () ((:output '("esperanto" "english"))))
                (completing-read (prompt choices)
                                 ((:input
                                   '("Choose language: " ("esperanto" "english"))
                                   :output "esperanto")))
                (capf-wordfreq--iso-code (language) ((:input '("esperanto") :output "eo")))
                (capf-wordfreq--probe-50k (lang-code) ((:input '("eo") :output t)))
                (capf-wordfreq--prompt-fetch-short () ((:output t)))
                (capf-wordfreq--dict-url (lang-code kind)
                                            ((:input '("eo" "50k") :output "http://example.com/eo_50k.txt")))
                (url-retrieve (url callback language inhibit-cookies)
                              ((:input '("http://example.com/eo_50k.txt"
                                         capf-wordfreq--list-retrieved-callback
                                         ("esperanto")
                                         :inhibit-cookies)
                                       :output 'buffer))))
     (capf-wordfreq-download-list)
     (should (eq capf-wordfreq--word-list-buffer 'buffer)))))

(ert-deftest test-drop-http-response-header ()
  (with-temp-buffer
    (insert "HTTP/1.1 200 OK
other headers

mi 16311
vi 13927
ne 11163
estas 10726
")
    (capf-wordfreq--drop-http-response-header)
    (should (equal (buffer-string) "mi 16311\nvi 13927\nne 11163\nestas 10726\n"))))

(ert-deftest test-list-retrieved-callback-success ()
  (let ((capf-wordfreq--word-list-buffer (generate-new-buffer "word-list-test-buffer"))
        (capf-wordfreq-path (concat (file-name-directory (temporary-file-directory))
                                       (make-temp-name ".emacs.d")))
        (buffer-tmp nil))
    (with-current-buffer capf-wordfreq--word-list-buffer
      (insert "HTTP/1.1 200 OK
other headers

mi 16311
vi 13927
ne 11163
estas 10726
"))
    (setq buffer-tmp capf-wordfreq--word-list-buffer)
    (capf-wordfreq--list-retrieved-callback '(:peer 'foo) "esperanto")
    (should (equal (with-temp-buffer
                     (insert-file-contents (concat (file-name-as-directory capf-wordfreq-path)
                                                   "esperanto.txt"))
                     (buffer-string))
                   "mi\nvi\nne\nestas\n"))
    (should-error (switch-to-buffer buffer-tmp))
    (should (eq capf-wordfreq--word-list-buffer nil))))

(ert-deftest test-list-retrieved-callback-error ()
  (mocker-let ((capf-wordfreq--drop-frequency-values () ((:occur 0))))
    (should-error (capf-wordfreq--list-retrieved-callback '(:error 'foo) '("esperanto")))))


;;; capf-wordfreq.el-test.el ends here
