(require 'sqlite3)

(let ((db (sqlite3-new)))
  (sqlite3-execute-batch db "CREATE TABLE foo(id integer primary key, name text);")
  (sqlite3-execute-batch db "INSERT INTO foo(name) values(?)" ["Tom"])
  (sqlite3-execute-batch db "INSERT INTO foo(name) values(?)" ["Bob"])
  (sqlite3-execute-batch db "INSERT INTO foo(name) values(?)" ["Alice"])
  (sqlite3-execute-batch db "INSERT INTO foo(name) values(?)" ["Mika"])

  (let ((row (sqlite3-execute db "SELECT * from foo")))
    (message "@@ fields=[%s]" (sqlite3-resultset-fields row))
    (let (cols)
      (while (setq cols (sqlite3-resultset-next db))
        (message "@@ (ID,NAME)=[%s]" cols)))))
