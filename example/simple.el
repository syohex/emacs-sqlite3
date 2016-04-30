(require 'sqlite3)

(let ((db (sqlite3-new))
      (buf (get-buffer-create "*sqlite3*")))
  (with-current-buffer buf
    (let ((inhibit-read-only t))
      (erase-buffer)))
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
       (with-current-buffer (get-buffer buf)
         (insert (format "@@ ID=%d, Editor=%s\n" id editor))))))
  (pop-to-buffer (get-buffer buf)))
