;;; majutsu-split.el --- Split transient for Majutsu  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <me@0wd0.com>
;; Maintainer: 0WD0 <me@0wd0.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This library provides jj split transients, managing revision selection
;; and interactive hunk/region patch selection for diff buffers.

;;; Code:

(require 'majutsu)
(require 'majutsu-diff-editor)

;;;; Remaining changes

(defvar-local majutsu-split--pending nil
  "Non-nil while a split started from this diff buffer is running.")

(defun majutsu-split--show-remaining (buffer operation-before)
  "Show the remaining changes of BUFFER's split since OPERATION-BEFORE.
Without --onto, --insert-after or --insert-before, jj moves the remaining
changes into the only change created by the split."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when-let* (((prog1 majutsu-split--pending
                     (setq majutsu-split--pending nil)))
                  (candidates
                   (majutsu-jj-lines
                    "log" "--no-graph" "-T" "change_id ++ \"\\n\""
                    "-r" (format "all() ~ at_operation(%s, all())"
                                 operation-before)))
                  (existing
                   (majutsu-jj-lines
                    "log" "--no-graph" "-T" "change_id ++ \"\\n\""
                    "-r" (format "at_operation(%s, %s)" operation-before
                                 (mapconcat (lambda (c) (format "change_id(%s)" c))
                                            candidates " | "))))
                  (new (seq-difference (seq-uniq candidates) existing))
                  ((length= new 1)))
        (setq-local majutsu-buffer-diff-range
                    (list (concat "--revisions=" (car new))))))))

(add-hook 'majutsu-interactive-repository-operation-functions
          #'majutsu-split--show-remaining)

(defun majutsu-split--mark-pending (args)
  "Mark a split with ARGS whose remaining changes go into a new change."
  (setq majutsu-split--pending
        (and (derived-mode-p 'majutsu-diff-mode)
             (not (seq-some (lambda (option)
                              (majutsu-jj-option-values args option))
                            '("--onto" "-o" "--insert-after" "-A"
                              "--insert-before" "-B"))))))

(defun majutsu-split--check-patch-source (args patch-source)
  "Signal when ARGS would split a commit other than PATCH-SOURCE."
  (unless patch-source
    (user-error "Patch selection for split requires a diff for exactly one commit"))
  (let ((revisions (majutsu-jj-option-values args "--revision" "-r")))
    (unless (<= (length revisions) 1)
      (user-error "Patch selection for split requires exactly one revision"))
    (let ((source (majutsu-jj-resolve-single-commit
                   (if revisions (car revisions) "@"))))
      (unless (and source (equal source patch-source))
        (user-error "Patch selection for split requires the rendered diff source")))))

(defun majutsu-split--log-default-args ()
  "Return a single revision default from log context."
  (when-let* ((revision (majutsu-revision-at-point)))
    (list (concat "--revision=" revision))))

(defun majutsu-split--default-args ()
  "Return a revision default from the current diff or log context."
  (if (derived-mode-p 'majutsu-diff-mode)
      (when-let* ((range majutsu-buffer-diff-range)
                  (commit (majutsu-diff-single-source-commit)))
        (list (concat "--revision=" commit)))
    (majutsu-split--log-default-args)))

(transient-define-suffix majutsu-split-execute (args)
  "Execute split with selections recorded in the transient."
  :description "Execute split"
  :class 'majutsu-transient-default-action-suffix
  (interactive (list (transient-args 'majutsu-split)))
  (pcase-let ((`(,args ,filesets) (majutsu-filesets-split-transient-value args)))
    ;; An explicit jj editor request takes precedence over an Emacs-owned
    ;; patch selection.  Do not inspect its owner here: jj does not consume
    ;; the selection, so it remains available for the originating command.
    (if (majutsu-diff-editor-interactive-arguments-p args)
        (progn
          (majutsu-split--mark-pending args)
          (majutsu-diff-editor-start
           "split" args filesets
           :origin-buffer (current-buffer)))
      (let* (;; Text hunks and hunkless files coexist in one operation.
             (plan (majutsu-interactive-build-replay-plan-if-selected
                    nil nil 'majutsu-split))
             (patch-source (and plan
                                (majutsu-diff-single-source-commit))))
        (when plan
          (majutsu-split--check-patch-source args patch-source)
          ;; Execute against the immutable commit which was just validated;
          ;; a dynamic @/bookmark must not move between the guard and jj.
          (setq args (majutsu-jj-set-option-value
                      args "--revision" patch-source "-r")))
        (cond
         (plan
          ;; Reset to the left tree, then replay precisely the selections.
          (majutsu-split--mark-pending args)
          (majutsu-interactive-run-replay-plan
           "split"
           (majutsu-diff-editor-strip-interactive-arguments args)
           filesets plan))
         ;; `jj split' selects interactively by default when no fileset is given.
         ((null filesets)
          (majutsu-split--mark-pending args)
          (majutsu-diff-editor-start
           "split" args filesets
           :origin-buffer (current-buffer)))
         (t
          (majutsu-run-jj-with-editor
           (cons "split" (majutsu-jj-append-filesets args filesets)))))))))

;;;; Infix Commands

(transient-define-argument majutsu-split:--revision ()
  :description "Revision"
  :class 'majutsu-revision-selection-option
  :selection-label "[REV]"
  :selection-face '(:background "goldenrod" :foreground "black")
  :selection-toggle-key "r"
  :selection-toggle-if-not #'majutsu-diff-single-source-commit
  :shortarg "-r"
  :argument "--revision="
  :reader #'majutsu-transient-read-revset)

(transient-define-argument majutsu-split:--onto ()
  :description "Onto"
  :class 'majutsu-revision-selection-option
  :selection-label "[ONTO]"
  :selection-face '(:background "dark green" :foreground "white")
  :selection-toggle-key "o"
  :selection-toggle-if-not #'majutsu-diff-single-source-commit
  :shortarg "-o"
  :argument "--onto="
  :multi-value 'repeat
  :reader #'majutsu-transient-read-revset)

(transient-define-argument majutsu-split:--insert-after ()
  :description "Insert after"
  :class 'majutsu-revision-selection-option
  :selection-label "[AFTER]"
  :selection-face '(:background "dark blue" :foreground "white")
  :selection-toggle-key "a"
  :selection-toggle-if-not #'majutsu-diff-single-source-commit
  :shortarg "-A"
  :argument "--insert-after="
  :multi-value 'repeat
  :reader #'majutsu-transient-read-revset)

(transient-define-argument majutsu-split:--insert-before ()
  :description "Insert before"
  :class 'majutsu-revision-selection-option
  :selection-label "[BEFORE]"
  :selection-face '(:background "dark magenta" :foreground "white")
  :selection-toggle-key "b"
  :selection-toggle-if-not #'majutsu-diff-single-source-commit
  :shortarg "-B"
  :argument "--insert-before="
  :multi-value 'repeat
  :reader #'majutsu-transient-read-revset)

(transient-define-argument majutsu-split:--message ()
  :description "Message"
  :shortarg "-m"
  :argument "--message="
  :reader #'read-string)

(transient-define-argument majutsu-split:-- ()
  :description "Limit to files"
  :class 'transient-files
  :key "--"
  :argument "--"
  :prompt "Limit to file,s: "
  :reader #'majutsu-read-files
  :multi-value t)

;;;; Prefix

;;;###autoload(autoload 'majutsu-split "majutsu-split" nil t)
(transient-define-prefix majutsu-split ()
  "Transient for jj split operations."
  :man-page "jj-split"
  :description "JJ Split"
  :class 'majutsu-jj-transient-prefix
  :jj-command "split"
  :transient-non-suffix t
  [["Selection"
    (majutsu-split:--revision)
    (majutsu-split:--onto)
    (majutsu-split:--insert-after)
    (majutsu-split:--insert-before)
    ("c" "Clear selections" majutsu-selection-clear :transient t)]
   ["Patch Selection" :if majutsu-diff-single-source-commit
    (majutsu-interactive:select-hunk)
    (majutsu-interactive:select-file)
    (majutsu-interactive:select-region)
    ("C" "Clear patch selections" majutsu-interactive-clear :transient t)]
   ["Paths" :if-not majutsu-diff-single-source-commit
    (majutsu-split:--)]
   ["Options"
    ("-i" "Interactive" ("-i" "--interactive"))
    ("-p" "Parallel" ("-p" "--parallel"))
    ("-e" "Editor" "--editor")
    ("=t" "Tool" "--tool=")
    (majutsu-transient-arg-ignore-immutable)]
   ["Actions"
    ("s" "Execute split" majutsu-split-execute)]]
  (interactive)
  (transient-setup
   'majutsu-split nil nil
   :scope
   (majutsu-selection-session-begin)
   :value (or (majutsu-split--default-args) '())))

;;; _
(provide 'majutsu-split)
;;; majutsu-split.el ends here
