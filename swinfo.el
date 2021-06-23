;;; swinfo.el --- get software information -*- lexical-binding: t; -*-

;; Copyright (C) 2021  iSeeU

;; Author: iSeeU
;; Created: 2021-06-03 07:12:19 +0300
;; Version: 0.0.1a19
;; Keywords: software info information version

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

;; Get a software information to insert it in a buffer, echo it or even
;; send it to the kill ring.

;;; Code:

(require 'package)

(defconst swinfo-version "0.0.1a19"
  "The version of Swinfo.")

;;;; Options

(defvar swinfo-static '()
  "WIP; 2021-06-15 13:50:52 +0300.")

(defvar swinfo-repo-dir
  '((emacs-config . (dir "~/.emacs.d"))
    (emacs-src . ( dir "~/my_clone/emacs-src"
                   sw-name "Emacs"
                   command (funcall (lambda () (format "%s" emacs-version))))))
  "WIP; 2021-06-04 09:02:26 +0300.")

(defvar swinfo-built-in-package '()
  "WIP; 2021-06-15 15:26:38 +0300.")

;;;; Helpers

(defun swinfo--call-process (command &rest args)
  "WIP; 2021-06-14 14:05:41 +0300."
  (with-temp-buffer
    (list (apply 'call-process command nil '(t nil) nil args)
          (buffer-string))))

(defun swinfo--get-first-line (str)
  "WIP; 2021-06-15 10:42:55 +0300."
  (replace-regexp-in-string "\n.*" "" str))

(defun swinfo--plist-get (alist-list alist-key plist-prop)
  "WIP; 2021-06-19 15:39:52 +0300."
  (plist-get (alist-get alist-key alist-list nil nil 'equal) plist-prop))

(defun swinfo--plist-get-prop-p (list name prop)
  "WIP; 2021-06-21 16:34:53 +0300."
  (if (swinfo--plist-get list name prop) t nil))

;;;; Info

;;;;; Static

(defun swinfo-static-info (name)
  "WIP; 2021-06-21 15:58:49 +0300."
  (when swinfo-static
    (let ((sw-name (swinfo--plist-get swinfo-static name 'sw-name))
          (sw-ver (swinfo--plist-get swinfo-static name 'sw-ver)))
      (format "%s:%s%s" name
              (if sw-name (concat " " sw-name) "")
              (if sw-ver (concat " " sw-ver) "")))))

;;;;; Repository

(defun swinfo-repo-commit-hash (repo-name)
  "WIP; 2021-06-05 14:14:05 +0300"
  (with-temp-buffer
    (let* ((default-directory (swinfo--plist-get swinfo-repo-dir repo-name 'dir))
           (latest-commit-hash
            (cadr (swinfo--call-process "git" "rev-parse" "HEAD")))
           (latest-commit-hash-short (substring latest-commit-hash 0 10)))
      (format "%s" latest-commit-hash-short))))

(defun swinfo-repo-commit-date (repo-name)
  "WIP; 2021-06-05 15:04:32 +0300."
  (with-temp-buffer
    (let* ((default-directory (swinfo--plist-get swinfo-repo-dir repo-name 'dir))
           (latest-commit-date
            ;; Using `string-trim' to get rid of the newline at the end
            ;; of result string.
            (string-trim
             (cadr (swinfo--call-process "git" "log" "-1"
                                        "--date=short" "--format=%cd")))))
      (format "%s" latest-commit-date))))

(defun swinfo-repo-info (repo-name)
  "WIP; 2021-06-05 15:23:07 +0300."
  (let* ((commit-hash (swinfo-repo-commit-hash repo-name))
         (commit-date (swinfo-repo-commit-date repo-name))
         (sw-name (swinfo--plist-get swinfo-repo-dir repo-name 'sw-name))
         (command (swinfo--plist-get swinfo-repo-dir repo-name 'command))
         (command-result (when command (apply command))))
    (if command-result
        (format "%s: %s %s; rev %s on %s"
                repo-name sw-name command-result
                commit-hash commit-date)
      (format "%s: rev %s on %s" repo-name commit-hash commit-date))))

;;;;; Package

(defun swinfo--package-desc (pkg-name)
  "WIP; 2021-06-20 18:37:45 +0300."
  (car (cdr (assq pkg-name package-alist))))

(defun swinfo--package-desc-extras (pkg-name slot)
  "WIP; 2021-06-20 18:57:46 +0300."
  (cdr (assoc slot (package-desc-extras
                    (car (cdr (assq pkg-name package-alist)))))))

(defun swinfo-package-info (pkg-name)
  "WIP; 2021-06-12 11:14:30 +0300."
  (when (memq pkg-name (mapcar #'car package-alist))
    (let* ((name (package-desc-name (swinfo--package-desc pkg-name)))
           (full-name (package-desc-full-name (swinfo--package-desc pkg-name)))
           (commit-hash (swinfo--package-desc-extras pkg-name :commit))
           (commit-hash-short (substring commit-hash 0 10)))
      (format "%s: %s (rev %s)"
              pkg-name full-name commit-hash-short))))

;;;;; Unix tool

(defun swinfo-unix-tool-info (name)
  "WIP; 2021-06-14 17:37:54 +0300."
  (let ((output
         (cond
          ((eq 0 (car (swinfo--call-process name "--version")))
           (swinfo--get-first-line
            (cadr (swinfo--call-process name "--version"))))
          ((eq 0 (car (swinfo--call-process name "-v")))
           (swinfo--get-first-line
            (cadr (swinfo--call-process name "-v"))))
          ((eq 0 (car (swinfo--call-process name "-V")))
           (swinfo--get-first-line
            (cadr (swinfo--call-process name "-V"))))
          (t (message "swinfo: All conditions failed.")))))
    (format "%s" output)))

;;;; Output

(defvar swinfo-software-list '()
  "WIP; 2021-06-13 11:18:02 +0300.")

(defun swinfo--combine-list ()
  "WIP; 2021-06-13 11:18:57 +0300."
  (setq swinfo-software-list
        (append (mapcar #'car swinfo-static)
                (mapcar #'car swinfo-repo-dir)
                package-activated-list)))

(defun swinfo--info (name)
  "WIP; 2021-06-12 13:04:17 +0300."
  (let ((name (reverse name))
        item
        info)
    (message "swinfo: `%s' type is %s." name (type-of name))
    (while name
      (setq item (pop name))
      (message "swinfo: `%s' type is %s." item (type-of item))
      (cond
       ((equal item (car (assoc item swinfo-static)))
        (message "swinfo: `%s' is a static and its type is %s." item (type-of item))
        (push (swinfo-static-info item) info))
       ((equal item (car (assoc item swinfo-repo-dir)))
        (message "swinfo: `%s' is a repo and its type is %s." item (type-of item))
        (push (swinfo-repo-info item) info))
       ((equal item (car (assoc item swinfo-built-in-package)))
        (message "swinfo: `%s' is a built-in and its type is %s." item (type-of item))
        (push (funcall (cdr (assoc item swinfo-built-in-package))) info))
       ((memq item package-activated-list)
        (message "swinfo: `%s' is a package and its type is %s." item (type-of item))
        (push (swinfo-package-info item) info))
       ((eq 0 (shell-command (format "type %s" item)))
        (push (swinfo-unix-tool-info item) info))
       (t (message "swinfo: Nothing matches `%s'." item))))
    (message "swinfo: `%s'; the type is %s." info (type-of info))
    (setq swinfo-info (string-join info "\n"))))

(defun swinfo-get-info (&optional arg)
  "WIP; 2021-06-12 07:47:12 +0300."
  (let ((arg (car current-prefix-arg)))
    (cond
     ((eq arg 4)
      (kill-new swinfo-info)
      (message "swinfo: Yanked software info into the kill ring."))
     ((eq arg 16)
      (let ((single-line
             (replace-regexp-in-string "\n" " -- " swinfo-info)))
        (message "swinfo: %s" single-line)))
     (t (insert swinfo-info)))))

(defvar swinfo--software-name-history nil
  "WIP; 2021-06-13 17:36:55 +0300.")

(defun swinfo (&rest name)
  "WIP; 2021-06-04 13:29:10 +0300."
  (interactive
   (progn
     (swinfo--combine-list)
     (let ((name (completing-read-multiple "Software name: " swinfo-software-list
                                           nil nil nil 'swinfo--software-name-history)))
       (and (> (length name) 0) (mapcar #'intern name)))))
  (if (< (length name) 1)
      (user-error "swinfo: No software name specified")
    (message "swinfo: `%s' and its type is %s" name (type-of name))
    (message "swinfo: `%s' and its type is %s" (car name) (type-of (car name)))
    (swinfo--info name)
    (swinfo-get-info)))

;;;; Closing marks

(provide 'swinfo)

;;; swinfo.el ends here
