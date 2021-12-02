;;; emacs-overleaf.el --- Using Emacs to edit Overleaf documents -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Jonghyun Yun
;;
;; Author: Jonghyun Yun <https://github.com/jonghyun-yun>
;; Maintainer: Jonghyun Yun <jonghyun.yun@gmail.com>
;; Created: November 24, 2021
;; Modified: November 24, 2021
;; Version: 0.0.1
;; Keywords: Overleaf sync LaTeX
;; Homepage: https://github.com/jonghyun-yun/emacs-overleaf
;; Package-Requires: ((emacs "26.1") (posframe "1.0.0") (projectile "2.5.0") (magit "3.0.0"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'magit)
(require 'posframe)
(require 'projectile)

(defvar-local overleaf-auto-sync "ask"
  "Determines how saving a buffer will trigger pushing changes to Overleaf.

Should be one of the following strings:
- `ask':
- `always':
- `never':
- nil:
")

(defvar-local overleaf-directory nil
  "A project directory to sync using Overleaf.")

(defvar overleaf-last-sync-time nil
"The last overleaf sync time.")

(defcustom overleaf-ask-time-interval (* 5 60)
  "A minimum time interval (in seconds) since the last sync to get asked again.")

(defvar overleaf-posframe-last-position nil
  "Last position for which a overleaf posframe was displayed.")

(defcustom overleaf-posframe-parameters nil
  "The frame parameters used by overleaf-posframe."
  :type 'string
  :group 'overleaf-posframe)

(defcustom overleaf-posframe-border-width 1
  "The border width used by overleaf-posframe.
When 0, no border is showed."
  :group 'overleaf-posframe
  :type 'number)

(defcustom overleaf-posframe-timeout 3
  "The number of seconds after which the posframe will auto-hide."
  :group 'overleaf-posframe
  :type 'number)

(defcustom overleaf-posframe-poshandler 'posframe-poshandler-frame-center
  "The poshandler used by overleaf-posframe."
  :group 'overleaf-posframe
  :type 'function)

(defface overleaf-posframe-face
  '((t :inherit default))
  "The background and foreground color of the posframe.
`background' and `foreground` are used in this face."
  :group 'overleaf-posframe)

(defface overleaf-posframe-border-face
  '((t (:background "gray50")))
  "The border color of the posframe.
Only `background` is used in this face."
  :group 'overleaf-posframe)

(defvar overleaf-posframe-buffer "*overleaf-posframe-buffer*"
  "The posframe-buffer used by overleaf-posframe.")

(defun overleaf-commit-message-format ()
  "A commit message format."
  (format "Updates @ %s" (format-time-string "%Y-%m-%d %H:%M %Z")))


(defun jyun/setup-overleaf-pull ()
  "Add hook for local overleaf pull."
  (progn
    (setq-local overleaf-directory (file-truename (projectile-project-name)))
    (add-hook 'projectile-after-switch-project-hook (lambda () (jyun/magit-pull-overleaf overleaf-directory)))))


(defun jyun/magit-pull-overleaf (directory)
  "Run `git pull origin master' using asynchronous magit processes."
  (when directory
    (let ((default-directory (magit-toplevel directory)))
    (magit-run-git-async "pull" "origin" "master"))))


(defun jyun/setup-overleaf-push ()
  "Add hook for local overleaf push."
  (progn
    (setq-local overleaf-directory (file-truename (projectile-project-name)))
    (setq-local overleaf-auto-sync nil)
    (add-hook 'after-save-hook (lambda ()
                                 (when (or (eq major-mode 'latex-mode) (eq major-mode 'bibtex-mode))
                                   (jyun/magit-push-overleaf overleaf-directory overleaf-auto-sync overleaf-last-sync-time))))))

(defun jyun/magit-push-overleaf (directory &optional auto-sync overleaf-last-sync-time)
  "Use Magit to stage files if there are unstaged ones.
Call asynchronous magit processes to commit and push staged files (if exist) to origin"
  (when directory
    (let ((check-time (overleaf--check-sync-time overleaf-last-sync-time)))
      (let ((do-sync (cond ((string= auto-sync "always") t)
                           ((string= auto-sync "ask") (if check-time (y-or-n-p "Push updates to Overleaf? ") nil))
                           (t nil)) )
            (default-directory (magit-toplevel directory)))
        (if do-sync
            (when (or (magit-anything-unstaged-p) (magit-anything-staged-p))
              (magit-with-toplevel
                (magit-stage-1 "--u" magit-buffer-diff-files))
              (let ((message (overleaf-commit-message-format)))
                (magit-run-git-async "commit" "-m" message)
                (magit-run-git-async "push" "origin" "master")
                )
              (overleaf-posframe-show-posframe "A buffer has been synced with Overleaf.")
              (setq-local overleaf-last-sync-time (current-time)))
          (unless (or (string= auto-sync "never") (not check-time))
            (overleaf-posframe-show-posframe
             (propertize "A buffer is not being synced with Overleaf."
                         'face
                         `(:inherit 'error)))))))))

(defun overleaf--check-sync-time (overleaf-last-sync-time)
  "Check if required time passed since the last sync."
    (or (not overleaf-last-sync-time)
              (> (float-time (time-since overleaf-last-sync-time)) overleaf-ask-time-interval)))

(defun overleaf-posframe-check-position ()
  "Update overleaf-posframe-last-position, returning t if there was no change."
  (equal overleaf-posframe-last-position
         (setq overleaf-posframe-last-position
               (list (current-buffer) (point)))))


(defun overleaf-posframe-hidehandler (_info)
  "Hide posframe if position has changed since last display."
  (not (overleaf-posframe-check-position)))


(defun overleaf-posframe-show-posframe (str)
  "Show overleaf sync status on the posframe"
  (posframe-hide overleaf-posframe-buffer)
  (overleaf-posframe-check-position)
  (posframe-show overleaf-posframe-buffer
                 :poshandler overleaf-posframe-poshandler
                 :foreground-color (face-foreground 'overleaf-posframe-face nil t)
                 :background-color (face-background 'overleaf-posframe-face nil t)
                 :internal-border-width overleaf-posframe-border-width
                 :internal-border-color (face-attribute 'overleaf-posframe-border-face :background)
                 :string str
                 :timeout overleaf-posframe-timeout
                 :hidehandler #'overleaf-posframe-hidehandler
                 :override-parameters overleaf-posframe-parameters)
  (let ((current-frame
         (buffer-local-value 'posframe--frame
                             (get-buffer overleaf-posframe-buffer))))
    (redirect-frame-focus current-frame
                          (frame-parent current-frame))))


(provide 'emacs-overleaf)
;;; emacs-overleaf.el ends here
