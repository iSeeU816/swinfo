;;; swver.el --- Get software version -*- lexical-binding: t; -*-

;; Copyright (C) 2021  iSeeU

;; Author: iSeeU
;; Created: 2021-06-03 07:12:19 +0300
;; Version: 0.0.1a11
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

;; Get a software version to insert it in a buffer, or even send it to
;; the kill ring.

;;; Code:

(defconst swver-version "0.0.1a11"
  "The version of Swver.")

(defvar swver-repo-dir
  '(("emacs-config" . "~/.emacs.d")
    ("emacs-src" . "~/my_clone/emacs-src"))
  "WIP; 2021-06-04 09:02:26 +0300.")

(defun swver-repo-commit-hash (repo-name)
  "WIP; 2021-06-05 14:14:05 +0300"
  (with-temp-buffer
    (let* ((default-directory
             (cdr (assoc repo-name swver-repo-dir)))
           (latest-commit-hash
            (progn (call-process "git" nil '(t nil) nil
                                 "rev-parse" "HEAD")
                   (goto-char (point-min))
                   (buffer-substring (point) (line-end-position))))
           (latest-commit-hash-short (substring latest-commit-hash 0 10)))
      (format "%s" latest-commit-hash-short))))

(defun swver-repo-commit-date (repo-name)
  "WIP; 2021-06-05 15:04:32 +0300."
  (with-temp-buffer
    (let* ((default-directory
             (cdr (assoc repo-name swver-repo-dir)))
           (latest-commit-date
            (progn
              (call-process "git" nil '(t nil) nil
                            "log" "-1" "--date=short" "--format=%cd")
              (goto-char (point-min))
              (buffer-substring (point) (line-end-position)))))
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

(defun swver--info (name)
  "WIP; 2021-06-10 13:03:34 +0300."
  (let* ((name (reverse name))
         info
         (collect (if (> (length name) 1)
                      (dolist (repo name)
                        (setq info (concat (swver-repo-info repo)
                                           (unless (not (equal (list repo) (last name)))
                                             "\n")
                                           info)))
                    (setq info (swver-repo-info (car name))))))
    (setq swver-info info)))

(defun swver-get-info (arg)
  "WIP; 2021-06-12 07:47:12 +0300."
  (cond
   ((eql arg 4)
    (kill-new swver-info)
    (message "swver: Yanked software info into the kill ring."))
   ((eql arg 16)
    (let ((single-line
           (replace-regexp-in-string "\n" " -- " swver-info)))
      (message "swver: %s" single-line)))
   (t (insert swver-info))))

(defun swver (&rest name)
  "WIP; 2021-06-04 13:29:10 +0300."
  (interactive
   (completing-read-multiple "Software name: " swver-repo-dir))
  (swver--info name)
  (swver-get-info (car current-prefix-arg)))

  (provide 'swver)

;;; swver.el ends here
