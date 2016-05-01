;;; sqlite3.el --- sqlite3 binding of Emacs Lisp

;; Copyright (C) 2016 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-sqlite3
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
  (sqlite3-core-new dbpath))

(defun sqlite3-execute-batch (sqlite query &optional bounds)
  "Execute SQL `query' for `db' database."
  (cl-assert (not (null sqlite)))
  (cl-assert (stringp query))
  (if (null bounds)
      (sqlite3-core-execute-batch sqlite query)
    (unless (vectorp bounds)
      (cl-assert (listp bounds))
      (setq bounds (apply #'vector bounds)))
    (sqlite3-core-execute-batch sqlite query bounds)))

(defun sqlite3-execute (sqlite query &optional callback)
  "Execute SQL `query' which has `SELECT' command. If `callback' argument is
specified, `callback' function is called with database row. `callback' takes
two arguments, first argument is row element of list, second argument is
field names of list."
  (cl-assert (not (null sqlite)))
  (cl-assert (stringp query))
  (sqlite3-core-execute sqlite query callback))

(provide 'sqlite3)

;;; sqlite3.el ends here
