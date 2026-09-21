;;; majutsu-embark-test.el --- Tests for Majutsu Embark actions  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Tests for Embark section targets and action behavior.

;;; Code:

(require 'ert)
(require 'embark)
(require 'majutsu-embark)

(ert-deftest majutsu-embark-section-target-does-not-require-list-or-row-cache ()
  (with-temp-buffer
    (majutsu-mode)
    (let ((inhibit-read-only t))
      (magit-insert-section (root)
        (magit-insert-section (jj-bookmark "topic@origin" nil :remote "origin" :tracked t)
          (magit-insert-heading "Custom remote label"))))
    (goto-char (point-min))
    (should (equal (majutsu-embark-target-section)
                   '(majutsu-tracked-bookmark . "topic@origin")))))

(ert-deftest majutsu-embark-empty-remote-has-remote-actions ()
  (with-temp-buffer
    (majutsu-bookmark-list-mode)
    (let ((inhibit-read-only t)
          (majutsu-bookmark--list-remotes (make-hash-table :test #'equal)))
      (puthash "origin" '(:fetch-url "https://example.org/repo") majutsu-bookmark--list-remotes)
      (magit-insert-section (bookmark-list) (majutsu-bookmark--wash-list nil)))
    (goto-char (point-min))
    (search-forward "https://")
    (should (equal (majutsu-embark-target-section)
                   '(majutsu-remote . "origin")))))

(ert-deftest majutsu-embark-track-actions-use-explicit-target ()
  (let (seen)
    (cl-letf (((symbol-function 'majutsu-start-jj) (lambda (args) (setq seen args))))
      (majutsu-bookmark-track-ref "topic*@fork*")
      (should (equal seen '("bookmark" "track" "exact:topic*" "--remote" "exact:fork*")))
      (majutsu-bookmark-untrack-ref "topic@fork")
      (should (equal seen '("bookmark" "untrack" "exact:topic" "--remote" "exact:fork")))
      (should-error (majutsu-bookmark-track-ref "topic@git") :type 'user-error)
      (should-error (majutsu-bookmark-untrack-ref "topic") :type 'user-error))))

(provide 'majutsu-embark-test)
;;; majutsu-embark-test.el ends here
