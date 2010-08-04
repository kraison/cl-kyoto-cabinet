(in-package :kyoto-cabinet)

(defmethod initialize-instance :after ((db kc-dbm) &key)
  (with-slots (ptr)
      db
    (setf ptr (kcdbnew))))

(defmethod raise-error ((db kc-dbm) &optional (message "")
			&rest message-arguments)
  (let* ((code (kcdbecode (ptr-of db)))
	 (msg (kcdbemsg code)))
    (error 'dbm-error :error-code code :error-msg msg
	   :text (apply #'format nil message message-arguments))))


;(defmethod maybe-raise-error ((db kc-dbm) &optional message
;			      &rest message-arguments)
;  (let ((ecode (kcdbecode (ptr-of db))))
;    (cond ((= +tcesuccess+ ecode)
;           t)
;          ((= +kcenorec+ ecode)
;          nil)
;          (t
;           (apply #'raise-error db message message-arguments)))))

(defmethod dbm-open ((db kc-dbm) filename &rest mode)
  (let ((db-ptr (ptr-of db)))
    (check-open-mode mode)
    (unless (kcdbopen db-ptr filename mode) ; opens db by side-effect
      (let* ((code (kcdbecode db-ptr))
             (msg (kcdbemsg code)))
        (kcdbdel db-ptr) ; clean up on error
        (error 'dbm-error :error-code code :error-msg msg))))
  db)


(defmethod dbm-close ((db kc-dbm))
  (kcdbclose (ptr-of db)))

(defmethod dbm-delete ((db kc-dbm))
  (kcdbdel (ptr-of db)))

(defmethod dbm-clear ((db kc-dbm))
  (kcdbclear (ptr-of db)))

(defmethod dbm-begin ((db kc-dbm) &rest hard)
  (kcdbbegintran (ptr-of db) hard))

(defmethod dbm-commit ((db kc-dbm))
  (kcdbendtran (ptr-of db) T))

(defmethod dbm-rollback ((db kc-dbm) commit)
  (kcdbendtran (ptr-of db) NIL))

(defmethod dbm-put ((db kc-dbm) key value &key mode overwrite-if-exists)
  (let ((func (if overwrite-if-exists
		  #'kcdbset
		  #'kcdbadd)))
    (funcall func (ptr-of db) key (length key) value (length value))))


(defmethod dbm-get ((db kc-dbm) (key string) &optional (type :string))
  (get-string->string db key #'kcdbget))