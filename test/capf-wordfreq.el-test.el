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

(ert-deftest test-no-ispell-dictionary-candidates-empty-list()
  (let ((ispell-local-dictionary nil))
    (should (equal (capf-wordfreq--candidates "foo") '()))))

(ert-deftest test-candidates-foo ()
  (mocker-let ((capf-wordfreq--grep-executable () ((:input '() :output "/path/to/grep-program")))
	       (shell-command-to-string (command)
                                        ((:input
                                          '("/path/to/grep-program -i \\^foo /path/to/dict.txt")
                                          :output "foobar\nfoobaz\nfoo\n")))
               (capf-wordfreq--dictionary () ((:output "/path/to/dict.txt"))))
    (should (equal (capf-wordfreq--candidates "foo") '("foobar" "foobaz" "foo")))))

(ert-deftest test-candidates-foo-case-sensitive ()
  (mocker-let ((capf-wordfreq--grep-executable () ((:input '() :output "/path/to/grep-program")))
	       (shell-command-to-string (command)
                                        ((:input
                                          '("/path/to/grep-program -i \\^Foo /path/to/dict.txt")
                                          :output "foobar\nfoobaz\nfoo\n")))
               (capf-wordfreq--dictionary () ((:output "/path/to/dict.txt"))))
    (should (equal (capf-wordfreq--candidates "Foo") '("Foobar" "Foobaz" "Foo")))))

(ert-deftest test-candidates-bar ()
  (mocker-let ((capf-wordfreq--grep-executable () ((:input '() :output "/other/path/to/grep-program")))
	       (shell-command-to-string (command)
                                        ((:input
                                          '("/other/path/to/grep-program -i \\^bar /other/path/to/dict.txt")
                                          :output "barbar\nbarbaz\nbar\n")))
               (capf-wordfreq--dictionary () ((:output "/other/path/to/dict.txt"))))
    (should (equal (capf-wordfreq--candidates "bar") '("barbar" "barbaz" "bar")))))

(ert-deftest test-candidates-no-newline-at-end ()
  (mocker-let ((capf-wordfreq--grep-executable () ((:input '() :output "/path/to/grep-program")))
	       (shell-command-to-string (command)
                                        ((:input
                                          '("/path/to/grep-program -i \\^foo /path/to/dict.txt")
                                          :output "foobar\nfoobaz\nfoo")))
               (capf-wordfreq--dictionary () ((:output "/path/to/dict.txt"))))
    (should (equal (capf-wordfreq--candidates "foo") '("foobar" "foobaz" "foo")))))

(ert-deftest test-dict-file-not-found ()
  (mocker-let ((capf-wordfreq--dictionary () ((:output nil))))
    (should (equal (capf-wordfreq--candidates "foo") '()))))


(ert-deftest test-capf-completion-at-point-function ()
  (let ((buffer-contents "f b"))
    (mocker-let ((capf-wordfreq--candidates (prefix)
					    ((:input '("f") :output '("foo"))
					     (:input '("b") :output '("baz" "bar")))))
      (setq completion-at-point-functions '(capf-wordfreq-completion-at-point-function))
      (with-temp-buffer
	(insert buffer-contents)
	(goto-char (point-min))
	(forward-char 1)
	(completion-at-point)
	(should (equal (buffer-string) "foo b"))
	(forward-char 1)
	(completion-at-point)
	(should (equal (buffer-string) "foo ba"))))))
