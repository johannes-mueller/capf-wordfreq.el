;;; capf-wordfreq.el-test.el --- Tests for capf-wordfreq

;;; Commentary:

;; usually run by ert-runner

;;; Code:

(require 'mocker)
(require 'capf-wordfreq)

(ert-deftest test-dict-path-esperanto ()
  (mocker-let ((file-exists-p (filename) ((:input '("/path/to/dicts/esperanto.txt") :output t))))
    (let ((ispell-local-dictionary "esperanto")
          (capf-wordfreq-path "/path/to/dicts"))
      (should (equal (capf-wordfreq--dictionary) "/path/to/dicts/esperanto.txt")))))

(ert-deftest test-dict-path-english ()
  (mocker-let ((file-exists-p (filename) ((:input '("/other/path/to/dicts/english.txt") :output t))))
    (let ((ispell-local-dictionary "english")
          (capf-wordfreq-path "/other/path/to/dicts"))
      (should (equal (capf-wordfreq--dictionary) "/other/path/to/dicts/english.txt")))))

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
