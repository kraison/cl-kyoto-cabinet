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


(defmethod maybe-raise-error ((db kc-dbm) &optional message
			      &rest message-arguments)
  (let ((ecode (kcdbecode (ptr-of db))))
    (cond ((= (foreign-bitfield-value 'dbm-return-flags '(:success))
	      ecode)
	   t)
	  ((= (foreign-bitfield-value 'dbm-return-flags '(:norec))
	      ecode)
	   nil)
	  (t
	   (apply #'raise-error db message message-arguments)))))
  

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


;; Define overloaded put methods

(defmethod dbm-put ((db kc-dbm) (key string) (value string) &key (overwrite-if-exists NIL))
  (let ((func (put-method-from overwrite-if-exists)))
	(put-string->string db key value func)))

(defmethod dbm-put ((db kc-dbm) (key string) (value vector) &key (overwrite-if-exists NIL))
  (let ((func (put-method-from overwrite-if-exists)))
    (put-string->octets db key value func)))

(defmethod dbm-put ((db kc-dbm) (key vector) (value vector) &key (overwrite-if-exists NIL))
  (let ((func (put-method-from overwrite-if-exists)))
    (put-octets->octets db key value func)))

(defmethod dbm-put ((db kc-dbm) (key integer) (value string) &key (overwrite-if-exists NIL))
  (let ((func (put-method-from overwrite-if-exists)))
    (put-int32->string db key value func)))

(defmethod dbm-put ((db kc-dbm) (key integer) (value vector) &key (overwrite-if-exists NIL))
  (let ((func (put-method-from overwrite-if-exists)))
    (put-int32->octets db key value func)))


;; Define overloaded get methods

(defmethod dbm-get ((db kc-dbm) (key string) &optional (type :string))
  (let ((fn #'kcdbget))
    (ecase type
      (:string (get-string->string db key fn))
      (:octets (get-string->octets db key fn)))))

(defmethod dbm-get ((db kc-dbm) (key integer) &optional (type :string))
  (let ((fn #'kcdbget))
    (ecase type
      (:string (get-int32->string db key fn))
      (:octets (get-int32->octets db key fn)))))

(defmethod dbm-get ((db kc-dbm) (key vector) &optional (type :string))
  (let ((fn #'kcdbget))
    (ecase type
      (:string (get-octets->string db key fn))
      (:octets (get-octets->octets db key fn)))))