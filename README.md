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

## Interfaces

#### `(sqlite3-new &optional db-path)`

Create sqlite3 instance. If `db-path` is omitted, data is stored in memory.

#### `(sqlite3-execute-batch db query &optional bounds)`

First argument `db` must be sqlite3 instance. If you use placeholders in `query`,
then you must pass `bounds` too.

#### `(sqlite3-execute db query &optional callback)`

Interface for executing `SELECT` query. If you pass `callback` argument,
`callback` is called with `SELECT` results. `callback` takes 2 arguments,
`row` and `fields`. `row` is value, `fields` are column names. If `callback`
is not specified, this function returns `resultset` instance.

#### `(sqlite3-resultset-next resultset)`

Return next row.

#### `(sqlite3-resultset-fields resultset)`

Return fields name of row.

#### `(sqlite3-resultset-eof resultset)`

Return `t` if there is no more row.
