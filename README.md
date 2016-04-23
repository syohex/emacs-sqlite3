# sqlite3.el

[sqlite](https://www.sqlite.org/) binding of Emacs Lisp inspired by [mruby-sqlite3](https://github.com/mattn/mruby-sqlite3)

## Sample

``` lisp
(require 'sqlite3)

(progn
  (with-current-buffer (get-buffer-create "*sqlite*")
    (erase-buffer))

  (let ((db (sqlite3-new "example/example.db")))
    (sqlite3-execute-batch db "CREATE TABLE foo(id integer primary key, editor text);")
    (sqlite3-execute-batch db "INSERT INTO foo(editor) values(?)" ["Vim"])
    (sqlite3-execute-batch db "INSERT INTO foo(editor) values(?)" ["Emacs"])
    (sqlite3-execute-batch db "INSERT INTO foo(editor) values(?)" ["Atom"])
    (sqlite3-execute-batch db "INSERT INTO foo(editor) values(?)" ["Notepad"])

    (sqlite3-execute
     db
     "SELECT * FROM foo"
     (lambda (row fields)
       (let ((id (car row))
             (editor (cadr row)))
         (with-current-buffer (get-buffer "*sqlite*")
           (insert (format "@@ ID=%d, Editor=%s\n" id editor))))))
    (pop-to-buffer (get-buffer "*sqlite*"))))
```
