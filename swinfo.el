;;; swinfo.el --- get software information -*- lexical-binding: t; -*-

;; Copyright (C) 2021  iSeeU

;; Author: iSeeU
;; Created: 2021-06-03 07:12:19 +0300
;; Version: 0.0.1a21
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

(defconst swinfo-version "0.0.1a21"
  "The version of Swinfo.")

;;;; Options

(defvar swinfo-static-list '()
  "List of software that are mostly in static phase.

An association list (Alist) where CAR is a symbol label for a
software, and CDR is a property list (Plist) that have (KEY1
VALUE1 KEY2 VALUE2 ...) style.

Currently, only `sw-name' and `sw-ver' property keys are
supported. The values of them should be string.

This is mostly for software that met these criteria:

- The software won't be updated anymore or the user choose not to
  do so.

- It's hard to parse the information or it's not worth it; so
  simply doing it manually is much faster.

- No automatic way to parse the information.")

(defvar swinfo-repo-list '()
  "List of software repositories to parse info from them.

An association list (Alist) where CAR is a symbol label for a
repository, and CAR is a property list (Plist) that have (KEY1
VALUE1 KEY2 VALUE2 ...) style.

Currently, only `dir', `sw-name' and `command' property keys are
supported.

`dir': A string of a repository absolute path.

`sw-name' (optional): A string of software name.

`command' (optional): Call a command with `funcall' function
which must return a string. It's useful to parse information that
can't be parsed from the repository.

Note that this can be used for repositories that are not related
to any software. It's a way of getting latest commit hash and
date for them.")

(defvar swinfo-built-in-package '()
  "List of built-in packages.")

;;;; Helpers

(defun swinfo--call-process (command &rest args)
  "Return output (as string) of shell COMMAND that can be called
with optional arguments ARGS. Consult `call-process' for more
info."
  (with-temp-buffer
    (list (apply 'call-process command nil '(t nil) nil args)
          (buffer-string))))

(defun swinfo--get-first-line (str)
  "Return first line of multiple lines string STR.

This is useful to only get the first line of a shell command
output."
  (replace-regexp-in-string "\n.*" "" str))

(defun swinfo--plist-get (alist-list alist-key plist-prop)
  "Get value of PLIST-PROP key that is nested under ALIST-KEY for ALIST-LIST.

Suppose you have something like this:

  (setq swinfo-repo-list
        '((repo-name . (dir \"/tmp/foo/repo-name\" sw-name \"Foo\"))))

And you want to get \"Foo\", so you call:

  (swinfo--plist-get swinfo-repo-list 'repo-name 'sw-name)"
  (plist-get (alist-get alist-key alist-list nil nil 'equal) plist-prop))

(defun swinfo--plist-get-prop-p (alist-list alist-key plist-prop)
  "Return `t' if PLIST-PROP key is exist under ALIST-KEY that
belong to ALIST-LIST; otherwise, return `nil'."
  (if (swinfo--plist-get alist-list alist-key plist-prop) t nil))

;;;; Info

;;;;; Static

(defun swinfo-static-info (name)
  "Return information about static software that goes with the NAME.

Currently, only software name and version are supported."
  (when swinfo-static-list
    (let ((sw-name (swinfo--plist-get swinfo-static-list name 'sw-name))
          (sw-ver (swinfo--plist-get swinfo-static-list name 'sw-ver)))
      (format "%s:%s%s" name
              (if sw-name (concat " " sw-name) "")
              (if sw-ver (concat " " sw-ver) "")))))

;;;;; Repository

(defun swinfo-repo-commit-hash (repo-name)
  "Return repository REPO-NAME commit hash (10 digits)."
  (with-temp-buffer
    (let* ((default-directory (swinfo--plist-get swinfo-repo-list repo-name 'dir))
           (latest-commit-hash
            (cadr (swinfo--call-process "git" "rev-parse" "HEAD")))
           (latest-commit-hash-short (substring latest-commit-hash 0 10)))
      (format "%s" latest-commit-hash-short))))

(defun swinfo-repo-commit-date (repo-name)
  "Return repository REPO-NAME commit date (ISO format)."
  (with-temp-buffer
    (let* ((default-directory (swinfo--plist-get swinfo-repo-list repo-name 'dir))
           (latest-commit-date
            ;; Using `string-trim' to get rid of the newline at the end
            ;; of result string.
            (string-trim
             (cadr (swinfo--call-process "git" "log" "-1"
                                         "--date=short" "--format=%cd")))))
      (format "%s" latest-commit-date))))

(defun swinfo-repo-info (repo-name)
  "Return information about repository REPO-NAME.

Supported information so far:

- Repository name (as the user label it in `swinfo-repo-list'
  variable).

- Software name (if any provided).

- A command output, something like software version that related
  to the repository (if any).

- Latest commit hash and date."
  (let* ((commit-hash (swinfo-repo-commit-hash repo-name))
         (commit-date (swinfo-repo-commit-date repo-name))
         (sw-name (swinfo--plist-get swinfo-repo-list repo-name 'sw-name))
         (command (swinfo--plist-get swinfo-repo-list repo-name 'command))
         (command-result (when command (apply command))))
    (if command-result
        (format "%s: %s %s; rev %s on %s"
                repo-name sw-name command-result
                commit-hash commit-date)
      (format "%s: rev %s on %s" repo-name commit-hash commit-date))))

;;;;; Package

(defun swinfo--package-desc (pkg-name)
  "Return package name as symbol for PKG-NAME from `package-alist'
variable."
  ;; Using `package--alist' function is to make sure that
  ;; `pacakge-alist' variable value is not `nil'.
  (car (cdr (assq pkg-name (package--alist)))))

(defun swinfo--package-desc-extras (pkg-name slot)
  "Return value of SLOT for PKG-NAME from `package-alist' variable
with the help of `package-desc-extras' function."
  (cdr (assoc slot (package-desc-extras
                    (car (cdr (assq pkg-name (package--alist))))))))

(defun swinfo-package-info (pkg-name)
  "Return information about package PKG-NAME.

Supported information so far:

- Package name to be used as label.

- Full name that is package name and its version as latest's
  commit date. Something like `foo-20210624.18.59'.

- Latest commit hash (10 digits)."
  (when (memq pkg-name (mapcar #'car (package--alist)))
    (let* ((name (package-desc-name (swinfo--package-desc pkg-name)))
           (full-name (package-desc-full-name (swinfo--package-desc pkg-name)))
           (commit-hash (swinfo--package-desc-extras pkg-name :commit))
           (commit-hash-short (substring commit-hash 0 10)))
      (format "%s: %s; rev %s"
              pkg-name full-name commit-hash-short))))

;;;;; Unix tool

(defun swinfo-unix-tool-info (name)
  "Return Unix tool NAME information.

Currently, it's only return what Unix tool version command
returns."
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
    (format "%s: %s" name output)))

;;;; Output

(defvar swinfo-software-list '()
  "A list to gather software names in as symbols.")

(defun swinfo--combine-list ()
  "Combine lists to have one list of software names as symbols.

Note that only installed packages (`package-activated-list') are
gathered and not all packages as what `package-alist' variable is
for."
  (setq swinfo-software-list
        (append (mapcar #'car swinfo-static-list)
                (mapcar #'car swinfo-repo-list)
                package-activated-list)))

(defun swinfo--info (name)
  "Loop through NAME (list of software names) to gather information
about them and set the result to `swinfo-info' variable."
  (let ((name (reverse name))
        item
        info)
    (message "swinfo: `%s' type is %s." name (type-of name))
    (while name
      (setq item (pop name))
      (message "swinfo: `%s' type is %s." item (type-of item))
      (cond
       ((equal item (car (assoc item swinfo-static-list)))
        (message "swinfo: `%s' is a static and its type is %s." item (type-of item))
        (push (swinfo-static-info item) info))
       ((equal item (car (assoc item swinfo-repo-list)))
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

(defun swinfo--get-info (&optional arg)
  "Get software information in different methods.

If called with one \\[universal-argument], send info to
`kill-ring', but if called with two \\[universal-argument] then
echo info to echo area. Otherwise, insert info in active buffer."
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
  "Variable to hold Swinfo history when type/select software names
in minibuffer.")

(defun swinfo (&rest name)
  "Get software information for NAME.

When called interactively, you can choose multiple candidates by
separate them with `crm-separator' character. For Unix tool, you
can type them as is, as `swinfo--info' function will check them
if no match was found in other categories' list.

In Lisp form, you can type Unix tool name as string, for other
categories' list, it must be a symbol.

See `swinfo--get-info' as how information is returned."
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
    (swinfo--combine-list)
    (swinfo--info name)
    (swinfo--get-info)))

;;;; Closing marks

(provide 'swinfo)

;;; swinfo.el ends here
