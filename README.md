# sqlite3.el

*NOTE Emacs 29 supports native sqlite3. You can access sqlite3 DB without this module*

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

## Interfaces

#### `(sqlite3-new &optional db-path)`

Create sqlite3 instance. If `db-path` is omitted, data is stored in memory.

#### `(sqlite3-execute-batch db query &optional bounds)`

Execute SQL `query` for `db` database.
First argument `db` must be sqlite3 instance. If you use placeholders in `query`,
then you must pass `bounds` too.

#### `(sqlite3-execute db query &rest args)`

Interface for executing `SELECT` query.

Rest parameters are `bounds` and `callback`. You can its argument as, either '(bounds) or '(callback) or '(bounds callback). `callback` function is called with database row. `callback' takes two arguments, first argument is row element of list, second argument is field names of list.

#### `(sqlite3-resultset-next resultset)`

Return next row.

#### `(sqlite3-resultset-fields resultset)`

Return fields name of row.

#### `(sqlite3-resultset-eof resultset)`

Return `t` if there is no more row.
