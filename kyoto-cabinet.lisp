(in-package #:kyoto-cabinet)

(deftype int32 ()
  "The 32bit built-in DBM key type."
  '(signed-byte 32))

(deftype int64 ()
  "The 64bit built-in DBM key type."
  '(signed-byte 64))

(defparameter *in-transaction-p* nil
  "Bound when in a transaction.")

(define-condition dbm-error (error)
  ((error-code :initform nil
               :initarg :error-code
               :reader error-code-of
               :documentation "The error code provided by KC.")
   (error-msg :initform nil
              :initarg :error-msg
              :reader error-msg-of
              :documentation "The error message provided by KC.")
   (text :initform nil
         :initarg :text
         :reader text
         :documentation "Any additional message provided by the CL API."))
  (:report (lambda (condition stream)
             (format stream "DBM error (~a) ~a~@[: ~a~]."
                     (error-code-of condition)
                     (error-msg-of condition)
                     (text condition)))))

(defclass kc-dbm ()
  ((ptr :initarg :ptr
        :accessor ptr-of
        :documentation "A pointer to a KC native database object."))
  (:documentation "A KC database."))

(defclass kc-bdb (tc-dbm)
  ()
  (:documentation "A KC B+ tree database."))

(defclass kc-hdb (tc-dbm)
  ()
  (:documentation "A KC hash database."))

(defclass kc-iterator ()
  ((ptr :initarg :ptr
        :accessor ptr-of
        :documentation "A KC pointer."))
  (:documentation "A KC database iterator, the superclass of both B+
tree cursors and hash iterators."))

(defclass bdb-iterator (tc-iterator)
  ()
  (:documentation "A B+ tree database cursor."))

(defclass hdb-iterator (tc-iterator)
  ((next-key :accessor next-key-of)
   (key-size :accessor key-size-of))
  (:documentation "A hash database iterator."))

(defgeneric dbm-open (db filespec &rest mode)
  (:documentation "Opens a new, or existing TC database.

Arguments:

- db (object): A KC dbm object.
- filespec (string): A pathname designator for the database file.

Rest:

- mode (list symbol): A list of mode keywords used when opening the
file. The modes are :KCOWRITER :KCOREADER :KCOCREATE :KCOTRUNCATE
:KCOAUTOTRAN :KCOAUTOSYNC :KCONOLOCK :KCOTRYLOCK :KCONOREPAIR
which correspond to those described in the KC specification.

Returns:

- The KC dbm object, now open."))

(defgeneric dbm-close (db)
  (:documentation "Closes an open KC database.

Arguments:

- db (object): A KC dbm object.

Returns:

- T on success, or NIL otherwise."))

(defgeneric dbm-delete (db)
  (:documentation "Deletes a KC database. If open, implicitly closes
it first.

Arguments:

- db (object): A KC dbm object.

Returns: 

- NIL ."))

(defgeneric dbm-vanish (db)
  (:documentation "Removes all records from DB."))

(defgeneric dbm-begin (db)
  (:documentation "Begins a transaction with DB."))

(defgeneric dbm-commit (db)
  (:documentation "Commits a transaction with DB."))

(defgeneric dbm-abort (db)
  (:documentation "Aborts a transaction with DB."))

(defgeneric dbm-put (db key value &key mode)
  (:documentation "Inserts KEY and VALUE into DB. MODE varies with DB
class.

Arguments:

- db (object): A KC database object.
- key (object): A key under which to insert.
- value (object): A value to insert under key.

Key:

- :mode (symbol): A symbol designating one of the KC insertion modes:
replace, keep, concat, etc.

Valid modes for B+ tree databases are:

- :REPLACE : If a record with the same key exists in the database, it
is overwritten.
- :KEEP : If a record with the same key exists in the database, this
function has no effect.
- :CONCAT : Concatenates a value at the end of the existing record in
the database. If there is no corresponding record, a new record is
created.

Valid modes for hash databases are:

:REPLACE , :KEEP and :CONCAT , as above."))

