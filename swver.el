;;; swver.el --- Get software version -*- lexical-binding: t; -*-

;; Copyright (C) 2021  iSeeU

;; Author: iSeeU
;; Created: 2021-06-03 07:12:19 +0300
;; Version: 0.0.1a8
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

(defconst swver-version "0.0.1a8"
  "The version of Swver.")

(defconst swver-emacs-src-repo-dir
  "~/my_clone/emacs-src")

(defvar swver-repo-dir
  '(("emacs-config" . "~/.emacs.d")
    ("emacs-src" . "~/my_clone/emacs-src"))
  "WIP; 2021-06-04 09:02:26 +0300.")

(defvar swver-repo-dir
  '((emacs-config . "~/.emacs.d")
    (emacs-src . "~/my_clone/emacs-src"))
  "WIP; 2021-06-06 10:12:51 +0300.")

(defun swver-commit-date (repo-name)
  "WIP; 2021-06-03 08:28:48 +0300."
  (with-temp-buffer
    (message "swver: repo-name is %s" repo-name)
    (let* ((default-directory
             (cdr (assoc repo-name swver-repo-dir)))
           (commit-date
            (progn
              ;; (call-process "git" nil '(t nil) nil
              ;;               "log -1" "--date=short" "--format=%cd")
              ;; (call-process "git" nil '(t nil) nil
              ;;               "--no-pager" "log" "-1" "--date=short" "--format=%cd")
              ;;
              ;; "-1" must be alone and not with "log", like "log -1".
              (call-process "git" nil '(t nil) nil
                            "log" "-1" "--date=short" "--format=%cd")
              ;; (call-process "git" nil '(t nil) nil
              ;;               "rev-parse" "--abbrev-ref" "HEAD")
              (goto-char (point-min))
              (buffer-substring (point-min) (line-end-position))))
           ;; (commit-date "test")
           )
      (message "swver: %s--%s" default-directory commit-date))))

(defun swver-emacs ()
  "WIP; 2021-06-03 07:42:29 +0300."
  (let* ((repo-ver emacs-repository-version)
         (commit-short (substring repo-ver 0 10))
         (built-on emacs-build-time)
         (commit-date ))
    (message "iseeu-swver: =%s= (built on %s)"
             commit-short built-on)))

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
      (message "swver: %s--%s" default-directory latest-commit-hash-short)
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
      (message "swver: %s--%s" default-directory latest-commit-date)
      (format "%s" latest-commit-date))))

(defun swver-repo-info (repo-name)
  "WIP; 2021-06-05 15:23:07 +0300."
  (let ((commit-hash (swver-repo-commit-hash repo-name))
        (commit-date (swver-repo-commit-date repo-name)))
    (cond
     (
      (equal repo-name "emacs-src")
      ;; (equal repo-name 'emacs-src)
      (format "Emacs %s =%s= (%s)" emacs-version commit-hash commit-date))
     (
      (equal repo-name "emacs-config")
      ;; (equal repo-name 'emacs-config)
      (format "Emacs config =%s= (%s)" commit-hash commit-date))
     (t (message "swver: %s is something else." repo-name)))))

(defun swver-insert (item-count info)
  "WIP; 2021-06-06 14:09:58 +0300."
  (if (> item-count 1)
      ;; (insert info "\n")
      (dolist (text info)
        (insert text "\n"))
    (insert info)))

(defun swver--info (name)
  "WIP; 2021-06-10 13:03:34 +0300."
  (message "swver: `%s' type is %s" name (type-of name))
  (setq swver-multiple-line nil)
  (let* (info
         (collect (if (> (length name) 1)
                      (dolist (repo name)
                        (setq swver-multiple-line t)
                        (setq info (concat (swver-repo-info repo)
                                           (unless (not (equal (list repo) (last name)))
                                             "\n")
                                           info)))
                    (setq info (swver-repo-info (car name)))))
         ;; (collect-2 (dolist (repo name)
         ;;              (setq info (concat (swver-repo-info repo)
         ;;                                 (unless (not (equal (list repo) (last name)))
         ;;                                   "\n")
         ;;                                 info))))
         )
    (setq swver-info info)
    (message "swver: `%s' type is %s" swver-info (type-of swver-info))))

(defun swver (&rest name)
  ;; (defun swver (&rest name &optional arg)
  ;; (defun swver (name &optional arg)
  "WIP; 2021-06-04 13:29:10 +0300."
  ;; (let ((name (intern (completing-read "Software name: " swver-repo-dir))))
  ;;   (if (stringp name) (message "yes") (message "no"))
  ;;   (swver-commit-date name)
  ;;   (message "swver: The choice is %s." name))

  ;; (cond
  ;;  ((memq name (list "emacs"
  ;;                    "emsrc"
  ;;                    "emacs-src"))
  ;;   (swver-commit-date 'emacs-src)
  ;;   (message "swver: %s" name))
  ;;  (t (message "swver: %s is empty." name)))

  ;; (let ((name (completing-read-multiple "Software name: " swver-repo-dir)))
  ;;   ;; (swver-commit-date name)
  ;;   (message "swver: %s." name))

  ;; (let ((name (completing-read-multiple "Software name: " swver-repo-dir)))
  ;;   (cl-loop for repo in name
  ;;            do (swver-commit-date repo)))

  ;; (let ((name (completing-read-multiple "Software name: " swver-repo-dir)))
  ;;   (cl-loop for repo in name
  ;;            do (swver-repo-info repo)))

  ;; (interactive
  ;;  (list (completing-read-multiple "Software name: " swver-repo-dir)))
  ;; (string-to-list name)
  ;; (setq-local name '("eamcs-src"))
  ;; (setq-local name '(emacs-src))
  ;; (message "swver: To list %s" name)
  ;; (if (stringp name) (message "swver: String? Yes.") (message "swver: String? No."))
  ;; (cl-loop for repo in name
  ;;          do (if (stringp repo) (message "swver: String? Yes2.") (message "swver: String? No2."))
  ;;          do (message "swver: From inside the loop; %s" repo)
  ;;          do (swver-repo-info repo))
  ;; ;; (swver-repo-info name)

  ;; (message "swver: %s" name)

  ;; (let ((n '("emacs-src" "emacs-config"))
  ;;       (n2 '(emacs-src)))
  ;;   (cl-loop for repo in n2
  ;;            do (if (stringp repo) (message "swver: String? Yes2.") (message "swver: String? No2."))
  ;;            do (message "swver: %s" repo)
  ;;            do (swver-repo-info repo)))

  ;; (interactive
  ;;  (list (completing-read-multiple "Software name: " swver-repo-dir)))
  ;; (cl-loop for repo in name
  ;;          do (if (stringp repo) (message "swver: String? Yes2.") (message "swver: String? No2."))
  ;;          do (message "swver: From inside the loop; %s" repo)
  ;;          do (swver-repo-info repo))

  ;; (string-to-list name)
  ;; (dolist
  ;;     ;; (repo '(emacs-src emacs-config))
  ;;     (repo 'name)
  ;;   (message "swver: Dolist test %s" repo)
  ;;   (if (stringp repo) (message "swver: String? Yes2.") (message "swver: String? No2."))
  ;;   (swver-repo-info repo))

  ;; ;; (interactive
  ;; ;;  (list (completing-read-multiple "Software name: " swver-repo-dir)))
  ;; (interactive
  ;;  (completing-read-multiple "Software name: " swver-repo-dir))
  ;; ;; (interactive
  ;; ;;  (setq-local name (completing-read-multiple "Software name: " swver-repo-dir)))
  ;; (message "swver: The length of %s is %d" name (length name))
  ;; (if (> (length name) 1)
  ;;     (progn (cl-loop for repo in name
  ;;                     do (message "swver: I'm in the loop!")
  ;;                     do (if (stringp repo) (message "swver: String? Yes2.") (message "swver: String? No2."))
  ;;                     do (message "swver: From inside the loop; %s" repo)
  ;;                     ;; do (swver-repo-info repo)
  ;;                     ;; do (insert (swver-repo-info repo) "\n")
  ;;                     do (swver-insert (swver-repo-info repo))
  ;;                     ))

  ;;   (message "swver: I'm out of the loop!")
  ;;   ;; (prin1 name)
  ;;   ;; (intern name)
  ;;   ;; (intern (cdr name))
  ;;   ;; (setq-local name "emacs-src")
  ;;   ;; (setq-local name 'emacs-src)
  ;;   ;; (setq-local name (cdr name))
  ;;   ;; (setq-local name (format "%s" name))
  ;;   ;; (prin1-to-string name)
  ;;   ;; (setq-local  (mapconcat 'identity name " "))
  ;;   ;; (string-to-list name)
  ;;   ;; (setq-local swver-test (car name))
  ;;   ;; (message "swver: last %s" name)
  ;;   ;; (message "swver: last %s--%s" name swver-test)
  ;;   ;; (swver-repo-info name)
  ;;   ;; (swver-repo-info swver-test)
  ;;   ;; (swver-repo-info (car name))

  ;;   ;; (insert (swver-repo-info (car name)))
  ;;   (swver-insert (swver-repo-info (car name)))

  ;;   )

  ;; (interactive
  ;;  (completing-read-multiple "Software name: " swver-repo-dir)
  ;;  ;; (list (completing-read-multiple "Software name: " swver-repo-dir))
  ;;  )
  ;; (message "swver: The length of %s is %d" name (length name))
  ;; (let ((info
  ;;        (if (> (length name) 1)
  ;;            (progn (cl-loop for repo in name
  ;;                            do (message "swver: I'm in the loop!")
  ;;                            do (if (stringp repo) (message "swver: String? Yes2.") (message "swver: String? No2."))
  ;;                            do (message "swver: From inside the loop; %s" repo)
  ;;                            do (swver-repo-info repo)))

  ;;          (message "swver: I'm out of the loop! %s" name)
  ;;          (swver-repo-info (car name)))))
  ;;   (message "swver: 1. The length of %s is %d." info (length info))
  ;;   (if current-prefix-arg
  ;;       ;; (message "swver: First %s" info)
  ;;       (progn (kill-new info)
  ;;              (message "swver: Just killed %S" info))
  ;;     ;; (message "swver: Second %s" info)
  ;;     (message "swver: 2. The length of %s is %d." info (length info))
  ;;     (swver-insert info)))

  ;; ;; 2021-06-07 10:53:58 +0300; v0.0.1a6
  ;; (interactive
  ;;  (completing-read-multiple "Software name: " swver-repo-dir)
  ;;  ;; (list (completing-read-multiple "Software name: " swver-repo-dir))
  ;;  )
  ;; (message "swver: 0. The length of %s is %d" name (length name))
  ;; (message "swver: 0. Type of %s is %s" name (type-of name))
  ;; (let ((item-count (length name))
  ;;       (info
  ;;        (if (> (length name) 1)
  ;;            (progn
  ;;              ;; (cl-loop for repo in name
  ;;              ;;          do (message "swver: I'm in the loop!")
  ;;              ;;          do (message "swver: 1a. Type of %s is %s" repo (type-of repo))
  ;;              ;;          do (if (stringp repo) (message "swver: String? Yes2.") (message "swver: String? No2."))
  ;;              ;;          do (message "swver: From inside the loop; %s" repo)
  ;;              ;;          do (swver-repo-info repo)
  ;;              ;;          do (message "swver: The format %s" repo))

  ;;              (setq-local swver-test-0 '())
  ;;              (dolist (repo name)
  ;;                ;; (setq-local swver-test-0 (+ 1 swver-test-0))
  ;;                (message "swver: 1a. dolist count is %s" swver-test-0)
  ;;                ;; (format "%s" (swver-repo-info repo))
  ;;                (add-to-list 'swver-test-0
  ;;                             (format "%s" (swver-repo-info repo)) 'append)
  ;;                )
  ;;              (setq info swver-test-0))

  ;;          (message "swver: I'm out of the loop! %s" name)
  ;;          (message "swver: 1b. Type of %s is %s" name (type-of name))
  ;;          (swver-repo-info (car name)))))
  ;;   (message "swver: Item count is %s" item-count)
  ;;   (message "swver: 2. The length of %s--%s is %d." name info (length info))
  ;;   (message "swver: 2. Type of %s is %s" info (type-of info))
  ;;   (if current-prefix-arg
  ;;       ;; (message "swver: First %s" info)
  ;;       (progn (kill-new info)
  ;;              (message "swver: Just killed %S" info))
  ;;     ;; (message "swver: Second %s" info)
  ;;     (message "swver: 3. The length of %s is %d." info (length info))
  ;;     (swver-insert item-count info)
  ;;     ;; (insert info)
  ;;     ))

  ;; 2021-06-07 13:17:00 +0300; still v0.0.1a6
  ;; (interactive
  ;;  (completing-read "Software name: " swver-repo-dir))

  ;; (do)

  ;; ;; 2021-06-09 13:12:12 +0300; v0.0.1a7

  ;; (interactive
  ;;  (completing-read-multiple "Software name: " swver-repo-dir))

  ;; (message "swver: `%s' type is %s" name (type-of name))
  ;; (message "swver: Count %s" (length name))

  ;; (let* ((info '())
  ;;        ;; (info (format ""))
  ;;        (collect (if (> (length name) 1)
  ;;                     (dolist (repo name)
  ;;                       (message "swver: dolist `%s'; its type is %s" repo (type-of repo))
  ;;                       (message "swver: dolist data `%s'" (swver-repo-info repo))
  ;;                       (message "swver: dolist (swver-repo-info repo) type is %s"
  ;;                                (type-of (swver-repo-info repo)))
  ;;                       (push (swver-repo-info repo) info)
  ;;                       ;; (concat (swver-repo-info repo) info)
  ;;                       )
  ;;                   (push (swver-repo-info (car name)) info)))
  ;;        (info-count (length info))
  ;;        ;; (info-placeholder (format "TEST"))
  ;;        ;; (info-ready (dolist (text info)
  ;;        ;;               ;; (format "%s" text)
  ;;        ;;               ;; (concat info-placeholder text)
  ;;        ;;               (unless (equal (list text) (last info))
  ;;        ;;                 ;; (format "\n")
  ;;        ;;                 ;; (concat info-placeholder "\n")
  ;;        ;;                 )))
  ;;        )
  ;;   (message "swver: `%s' type %s" info (type-of info))
  ;;   (message "swver: Data %s" info)
  ;;   (message "swver: Count %s" info-count)
  ;;   ;; (swver-insert info-count (or info collect))
  ;;   ;; (dolist (text info)
  ;;   ;;   ;; (insert text "\n")

  ;;   ;;   (insert text)
  ;;   ;;   (message "swver: (Out) Last %s" (last info))
  ;;   ;;   (unless (equal (list text) (last info))
  ;;   ;;     (message "swver: (In) Text %s--Last %s" (list text) (last info))
  ;;   ;;     (insert "\n"))
  ;;   ;;   )

  ;;   ;; (insert info-ready)
  ;;   )

  ;; ;; 2021-06-10 11:35:20 +0300; v0.0.1a7

  ;; (interactive
  ;;  (completing-read-multiple "Software name: " swver-repo-dir))

  ;; (let* (info
  ;;        (collect (if (> (length name) 1)
  ;;                     (dolist (repo name)
  ;;                       (setq info (concat (swver-repo-info repo)
  ;;                                          (unless (not (equal (list repo) (last name)))
  ;;                                            "\n")
  ;;                                          info))
  ;;                       ;; (push (swver-repo-info repo) info)
  ;;                       )
  ;;                   ;; (push (swver-repo-info (car name) info))
  ;;                   (setq info (swver-repo-info (car name)))
  ;;                   )))
  ;;   ;; (message "swver: `%s' type is %s" info (type-of info))
  ;;   (if current-prefix-arg
  ;;       (progn (kill-new info)
  ;;              (message "swver: Info yanked to the kill ring."))
  ;;     (insert info)))

  ;; 2021-06-10 13:06:20 +0300; v0.0.1a8

  (interactive
   ;; (list
   (completing-read-multiple "Software name: " swver-repo-dir)
   ;; current-prefix-arg)
   )

  (message "swver: `%s' type is %s" name (type-of name))
  (swver--info name)
  ;; (message "swver: Info %s" swver-info)

  (message "swver: Current prefix arg value is %s" current-prefix-arg)
  (message "swver: Is is multiple line? `%s'" swver-multiple-line)

  (cond
   (
    ;; (equal arg 4)
    (eql (car current-prefix-arg) 4)
    ;; (message "swver: Yank method.")
    (kill-new swver-info))
   (
    ;; (equal arg 16)
    (eql (car current-prefix-arg) 16)
    ;; (message "swver: Echo method.")
    (message "----------------\nswver: \n%s\n----------------" swver-info)
    (when swver-multiple-line
      (message "swver: Visit `*Messages*' buffer to see the info.")))
   (t
    ;; (message "swver: Insert method.")
    (insert swver-info))))

  (provide 'swver)

;;; swver.el ends here
