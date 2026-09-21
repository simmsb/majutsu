;;; majutsu-evil-test.el --- Tests for majutsu-evil  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Tests for Evil-specific keymaps.

;;; Code:

(require 'ert)
(require 'majutsu-conflict)
(require 'majutsu-evil)

(ert-deftest majutsu-evil-test-conflict-side-bindings ()
  "Evil digit bindings resolve the selected side or base of a conflict."
  (pcase-dolist (`(,map ,key ,expected)
                 '((majutsu-conflict-evil-before-map "1" "base\n")
                   (majutsu-conflict-evil-resolve-map "2" "second\n")))
    (with-temp-buffer
      (insert "<<<<<<< conflict 1 of 1\n"
              "+++++++ side-1\nfirst\n"
              "------- base\nbase\n"
              "+++++++ side-2\nsecond\n"
              ">>>>>>> conflict 1 of 1 ends\n")
      (goto-char (point-min))
      (search-forward "<<<<<<<")
      (call-interactively (lookup-key (symbol-value map) (kbd key)))
      (should (equal (buffer-string) expected)))))

(provide 'majutsu-evil-test)
;;; majutsu-evil-test.el ends here
