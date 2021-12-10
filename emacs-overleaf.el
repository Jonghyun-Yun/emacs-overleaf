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
(require 'browse-at-remote)
(require 'posframe)
(require 'projectile)

;;;; overleaf
;; TODO: list of major mode to push changes on save
(define-minor-mode emacs-overleaf-mode
  "Toggle `emacs-overleaf-mode' with additional parameters."
  :init-value nil
  :global nil
  (if emacs-overleaf-mode
      (progn
        (unless (emacs-overleaf--is-remote-overleaf)
          (user-error "Not Overleaf project."))
        (emacs-overleaf-setup))
    (progn
      (remove-hook 'projectile-after-switch-project-hook #'emacs-overleaf-after-switch-project)
      (remove-hook 'after-save-hook #'emacs-overleaf-after-save))
    ))

(defun emacs-overleaf--remote-homepage ()
  "Get the url of the remote."
  (or (let ((url (browse-at-remote--remote-ref)))
        (cdr (browse-at-remote--get-url-from-remote (car url))))
      (user-error "Can't find the remote for current project")))

(defun emacs-overleaf--is-remote-overleaf ()
  "non-nil if the remote is overleaf repo."
  (string-match-p (regexp-quote "overleaf") (emacs-overleaf--remote-homepage)))

(defvar-local emacs-overleaf-auto-sync "ask"
  "Determines how saving a buffer will trigger pushing changes to Overleaf.

Should be one of the following strings:
- `ask':
- `always':
- `never':
- nil:
")

(defvar-local emacs-overleaf-directory nil
  "A project directory to sync using Overleaf.")

(defvar emacs-overleaf-last-sync-time nil
"The last overleaf sync time.")

(defvar emacs-overleaf-last-ask-time nil
"The last time overleaf ask for push.")

(defcustom emacs-overleaf-dont-ask-too-often-time-interval (* 5 60)
  "If non-nil, you won't be asked for `overleaf-dont-ask-too-often-time-interval' (in seconds) since the last you asked.")

(defcustom emacs-overleaf-ask-time-interval (* 5 60)
  "A minimum time interval (in seconds) since the last sync to get asked again.")

(defvar emacs-overleaf-posframe-last-position nil
  "Last position for which a overleaf posframe was displayed.")

(defcustom emacs-overleaf-posframe-parameters nil
  "The frame parameters used by emacs-overleaf-posframe."
  :type 'string
  :group 'emacs-overleaf-posframe)

(defcustom emacs-overleaf-posframe-border-width 1
  "The border width used by emacs-overleaf-posframe.
When 0, no border is showed."
  :group 'emacs-overleaf-posframe
  :type 'number)

(defcustom emacs-overleaf-posframe-timeout 3
  "The number of seconds after which the posframe will auto-hide."
  :group 'emacs-overleaf-posframe
  :type 'number)

(defcustom emacs-overleaf-posframe-poshandler 'posframe-poshandler-frame-center
  "The poshandler used by emacs-overleaf-posframe."
  :group 'emacs-overleaf-posframe
  :type 'function)

(defface emacs-overleaf-posframe-face
  '((t :inherit default))
  "The background and foreground color of the posframe.
`background' and `foreground` are used in this face."
  :group 'emacs-overleaf-posframe)

(defface emacs-overleaf-posframe-border-face
  '((t (:background "gray50")))
  "The border color of the posframe.
Only `background` is used in this face."
  :group 'emacs-overleaf-posframe)

(defvar emacs-overleaf-posframe-buffer "*overleaf-posframe-buffer*"
  "The posframe-buffer used by emacs-overleaf-posframe.")

(defun emacs-overleaf-commit-message-format ()
  "A commit message format."
  (format "Updates @ %s" (format-time-string "%Y-%m-%d %H:%M %Z")))


(defun emacs-overleaf-after-switch-project ()
  (progn
    (setq-local emacs-overleaf-directory
                (file-name-directory (file-truename (projectile-project-name))))
    (emacs-overleaf-pull emacs-overleaf-directory)))


(defun emacs-overleaf-setup-pull ()
  "Add hook for local overleaf pull."
  (add-hook 'projectile-after-switch-project-hook #'emacs-overleaf-after-switch-project))


(defun emacs-overleaf-pull (directory)
  "Run `git pull origin master' using asynchronous magit processes."
  (when directory
    (let ((default-directory (magit-toplevel directory)))
      (magit-run-git-async "pull" "origin" "master"))))


(defun emacs-overleaf-after-save ()
  (progn
    (setq-local emacs-overleaf-directory
                (file-name-directory
                 (file-truename (projectile-project-name))))
    (when (or (eq major-mode 'latex-mode) (eq major-mode 'bibtex-mode))
      (emacs-overleaf-push emacs-overleaf-directory emacs-overleaf-auto-sync emacs-overleaf-last-sync-time))))


(defun emacs-overleaf-setup-push ()
  "Add hook for local overleaf push."
    (add-hook 'after-save-hook #'emacs-overleaf-after-save))


(defun emacs-overleaf-setup ()
  "Add hook for local overleaf push and pull."
  (progn (add-hook 'projectile-after-switch-project-hook #'emacs-overleaf-after-switch-project)
         (add-hook 'after-save-hook #'emacs-overleaf-after-save)))


(defun emacs-overleaf-push (directory &optional auto-sync last-sync-time)
  "Use Magit to stage files if there are unstaged ones.
Call asynchronous magit processes to commit and push staged files (if exist) to origin"
  (when directory
    (let ((check-time (emacs-overleaf--check-time last-sync-time emacs-overleaf-ask-time-interval))
          (ask-time (emacs-overleaf--check-time emacs-overleaf-last-ask-time emacs-overleaf-dont-ask-too-often-time-interval)))
      (let ((do-sync (cond ((string= auto-sync "always") t)
                           ((string= auto-sync "ask") (if (and check-time ask-time)
                                                          (progn
                                                            (setq-local emacs-overleaf-last-ask-time (current-time))
                                                            (y-or-n-p "Push updates to Overleaf? ")
                                                            ) nil))
                           (t nil)) )
            (default-directory (magit-toplevel directory)))
        (if do-sync
            (when (or (magit-anything-unstaged-p) (magit-anything-staged-p))
              (magit-with-toplevel
                (magit-stage-1 "--u" magit-buffer-diff-files))
              (let ((message (emacs-overleaf-commit-message-format)))
                (magit-run-git-async "commit" "-m" message)
                (magit-run-git-async "push" "origin" "master")
                )
              (emacs-overleaf-posframe-show-posframe "A buffer has been synced with Overleaf.")
              (setq-local last-sync-time (current-time)))
          (unless (or (string= auto-sync "never") (not check-time))
            (emacs-overleaf-posframe-show-posframe
             (propertize "A buffer is not being synced with Overleaf."
                         'face
                         `(:inherit 'error)))))))))

(defun emacs-overleaf--check-time (emacs-overleaf-last-time emacs-overleaf-time-interval)
  "Check if required time passed since the last sync."
    (or (not emacs-overleaf-last-time)
              (> (float-time (time-since emacs-overleaf-last-time)) emacs-overleaf-time-interval)))

(defun emacs-overleaf-posframe-check-position ()
  "Update emacs-overleaf-posframe-last-position, returning t if there was no change."
  (equal emacs-overleaf-posframe-last-position
         (setq emacs-overleaf-posframe-last-position
               (list (current-buffer) (point)))))


(defun emacs-overleaf-posframe-hidehandler (_info)
  "Hide posframe if position has changed since last display."
  (not (emacs-overleaf-posframe-check-position)))


(defun emacs-overleaf-posframe-show-posframe (str)
  "Show overleaf sync status on the posframe"
  (posframe-hide emacs-overleaf-posframe-buffer)
  (emacs-overleaf-posframe-check-position)
  (posframe-show emacs-overleaf-posframe-buffer
                 :poshandler emacs-overleaf-posframe-poshandler
                 :foreground-color (face-foreground 'emacs-overleaf-posframe-face nil t)
                 :background-color (face-background 'emacs-overleaf-posframe-face nil t)
                 :internal-border-width emacs-overleaf-posframe-border-width
                 :internal-border-color (face-attribute 'emacs-overleaf-posframe-border-face :background)
                 :string str
                 :timeout emacs-overleaf-posframe-timeout
                 :hidehandler #'emacs-overleaf-posframe-hidehandler
                 :override-parameters emacs-overleaf-posframe-parameters)
  (let ((current-frame
         (buffer-local-value 'posframe--frame
                             (get-buffer emacs-overleaf-posframe-buffer))))
    (redirect-frame-focus current-frame
                          (frame-parent current-frame))))


(provide 'emacs-overleaf)
;;; emacs-overleaf.el ends here
