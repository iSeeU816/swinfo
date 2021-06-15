;;; swver.el --- Get software version -*- lexical-binding: t; -*-

;; Copyright (C) 2021  iSeeU

;; Author: iSeeU
;; Created: 2021-06-03 07:12:19 +0300
;; Version: 0.0.1a12
;; Keywords: software version

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Get a software version to insert it in a buffer, echo it or even send
;; it to the kill ring.

;;; Code:

(defconst swver-version "0.0.1a12"
  "The version of Swver.")

(defvar swver-repo-dir
  '(("emacs-config" . "~/.emacs.d")
    ("emacs-src" . "~/my_clone/emacs-src"))
  "WIP; 2021-06-04 09:02:26 +0300.")

(defun swver--call-process (command &rest args)
  "WIP; 2021-06-14 14:05:41 +0300."
  (with-temp-buffer
    (list (apply 'call-process command nil '(t nil) nil args)
          (buffer-string))))

(defun swver--get-first-line (str)
  "WIP; 2021-06-15 10:42:55 +0300."
  (replace-regexp-in-string "\n.*" "" str))

(defun swver-repo-commit-hash (repo-name)
  "WIP; 2021-06-05 14:14:05 +0300"
  (with-temp-buffer
    (let* ((default-directory
             (cdr (assoc repo-name swver-repo-dir)))
           (latest-commit-hash
            (cadr (swver--call-process "git" "rev-parse" "HEAD")))
           (latest-commit-hash-short (substring latest-commit-hash 0 10)))
      (format "%s" latest-commit-hash-short))))

(defun swver-repo-commit-date (repo-name)
  "WIP; 2021-06-05 15:04:32 +0300."
  (with-temp-buffer
    (let* ((default-directory
             (cdr (assoc repo-name swver-repo-dir)))
           (latest-commit-date
            ;; Using `string-trim' to get rid of the newline at the end
            ;; of result string.
            (string-trim
             (cadr (swver--call-process "git" "log" "-1"
                                        "--date=short" "--format=%cd")))))
      (format "%s" latest-commit-date))))

(defun swver-repo-info (repo-name)
  "WIP; 2021-06-05 15:23:07 +0300."
  (let ((commit-hash (swver-repo-commit-hash repo-name))
        (commit-date (swver-repo-commit-date repo-name)))
    (cond
     ((equal repo-name "emacs-src")
      (format "Emacs %s =%s= (%s)" emacs-version commit-hash commit-date))
     ((equal repo-name "emacs-config")
      (format "Emacs config =%s= (%s)" commit-hash commit-date))
     (t (message "swver: %s is something else." repo-name)))))

(defun swver-package-info (name)
  "WIP; 2021-06-12 11:14:30 +0300."
  (let ((elpa-dir package-user-dir))
    (string-join (directory-files elpa-dir nil name nil 1))))

(defun swver-unix-tool-info (name)
  "WIP; 2021-06-14 17:37:54 +0300."
  (let ((output
         (cond
          ((eq 0 (car (swver--call-process name "--version")))
           (swver--get-first-line
            (cadr (swver--call-process name "--version"))))
          ((eq 0 (car (swver--call-process name "-v")))
           (swver--get-first-line
            (cadr (swver--call-process name "-v"))))
          ((eq 0 (car (swver--call-process name "-V")))
           (swver--get-first-line
            (cadr (swver--call-process name "-V"))))
          (t (message "swver: All conditions failed.")))))
    (format "%s" output)))

(defvar swver-software-list '()
  "WIP; 2021-06-13 11:18:02 +0300.")

(defun swver--combine-list ()
  "WIP; 2021-06-13 11:18:57 +0300."
  (setq swver-software-list
        (append swver-repo-dir package-activated-list)))

(defun swver--info (name)
  "WIP; 2021-06-12 13:04:17 +0300."
  (let ((name (reverse name))
        item
        info)
    (while name
      (setq item (pop name))
      (cond
       ((equal item (car (assoc item swver-repo-dir)))
        (message "swver: `%s' is a repo and its type is %s." item (type-of item))
        (push (swver-repo-info item) info))
       ((memq (intern item) package-activated-list)
        (message "swver: `%s' is a package and its type is %s." item (type-of item))
        (push (swver-package-info item) info))
       ((eq 0 (shell-command (format "type %s" item)))
        (push (swver-unix-tool-info item) info))
       (t (message "swver: Nothing matches `%s'." item))))
    (message "swver: `%s'; the type is %s." info (type-of info))
    (setq swver-info (string-join info "\n"))))

(defun swver-get-info (&optional arg)
  "WIP; 2021-06-12 07:47:12 +0300."
  (let ((arg (car current-prefix-arg)))
    (cond
     ((eq arg 4)
      (kill-new swver-info)
      (message "swver: Yanked software info into the kill ring."))
     ((eq arg 16)
      (let ((single-line
             (replace-regexp-in-string "\n" " -- " swver-info)))
        (message "swver: %s" single-line)))
     (t (insert swver-info)))))

(defvar swver--software-name-history nil
  "WIP; 2021-06-13 17:36:55 +0300.")

(defun swver (&rest name)
  "WIP; 2021-06-04 13:29:10 +0300."
  (interactive
   (completing-read-multiple "Software name: " swver-software-list
                             nil nil nil 'swver--software-name-history))
  (message "swver: `%s' and its type is %s" name (type-of name))
  (swver--info name)
  (swver-get-info))

(provide 'swver)

;;; swver.el ends here
