;;; sqlite3.el --- sqlite3 binding of Emacs Lisp

;; Copyright (C) 2017 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-sqlite3
;; Package-Requires: ((emacs "25"))
;; Version: 0.01

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'sqlite3-core)

;;;###autoload
(defun sqlite3-new (&optional dbpath)
  "Create `sqlite3' instance. If `dbpath' is omitted, then database is stored
into memory."
  (sqlite3-core-new (and dbpath (file-truename dbpath))))

(defun sqlite3-execute-batch (sqlite query &optional bounds)
  "Execute SQL `query' for `db' database."
  (cl-assert (not (null sqlite)))
  (cl-assert (stringp query))
  (if (null bounds)
      (sqlite3-core-execute-batch sqlite query nil)
    (unless (vectorp bounds)
      (cl-assert (listp bounds))
      (setq bounds (vconcat bounds)))
    (sqlite3-core-execute-batch sqlite query bounds)))

(defun sqlite3-execute (sqlite query &rest args)
  "Execute SQL `query' which has `SELECT' command.

Rest parameters are `bounds' and `callback'. You can its argument as,
either '(bounds) or '(callback) or '(bounds callback). `callback' function
is called with database row. `callback' takes two arguments, first argument
is row element of list, second argument is field names of list."
  (cl-assert (not (null sqlite)))
  (cl-assert (stringp query))
  (cl-assert (<= (length args) 2))
  (let* ((rargs (reverse args))
         (callback (car rargs))
         bounds)
    (if (functionp callback)
        (setq rargs (cdr rargs))
      (setq callback nil))
    (when rargs
      (setq bounds (car rargs)))
    (when (and bounds (not (vectorp bounds)))
      (setq bounds (vconcat bounds)))
    (sqlite3-core-execute sqlite query bounds callback)))

(provide 'sqlite3)

;;; sqlite3.el ends here
