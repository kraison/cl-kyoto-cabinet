(in-package #:kyoto-cabinet)

(defmethod initialize-instance :after ((db kc-dbm) &key)
  (with-slots (ptr)
      db
    (setf ptr (kcdbnew))))

(defmethod raise-error ((db kc-dbm) &optional (message "")
			&rest message-arguments)
  (let* ((code (kcdbecode (ptr-of db)))
	 (msg (kcdbemsg (ptr-of db))))
    (error 'dbm-error :error-code code :error-msg msg
	   :text (apply #'format nil message message-arguments))))


(defmethod maybe-raise-error ((db kc-dbm) &optional message
			      &rest message-arguments)
  (let ((ecode (kcdbecode (ptr-of db))))
    (cond ((= (foreign-enum-value 'dbm-return-values :success)
	      ecode)
	   t)
	  ((= (foreign-enum-value 'dbm-return-values :norec)
	      ecode)
	   nil)
	  ((= (foreign-enum-value 'dbm-return-values :duprec)
	      ecode)
	   nil)
	  (t
	   (apply #'raise-error db message message-arguments)))))
  

(defmethod dbm-open ((db kc-dbm) filename &rest mode)
  (let ((db-ptr (ptr-of db)))
    (check-open-mode mode)
    (unless (kcdbopen db-ptr filename mode) ; opens db by side-effect
      (let* ((code (kcdbecode db-ptr))
             (msg (kcdbemsg db-ptr)))
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

(defmethod dbm-rollback ((db kc-dbm))
  (kcdbendtran (ptr-of db) NIL))


;; Define overloaded put methods

(defmethod dbm-put ((db kc-dbm) (key string) (value string) &key (mode :replace))
  (let ((func (put-method-for mode)))
	(put-string->string db key value func)))

(defmethod dbm-put ((db kc-dbm) (key string) (value vector) &key (mode :replace))
  (let ((func (put-method-for mode)))
    (put-string->octets db key value func)))

(defmethod dbm-put ((db kc-dbm) (key vector) (value vector) &key (mode :replace))
  (let ((func (put-method-for mode)))
    (put-octets->octets db key value func)))

(defmethod dbm-put ((db kc-dbm) (key integer) (value string) &key (mode :replace))
  (let ((func (put-method-for mode)))
    (put-int32->string db key value func)))

(defmethod dbm-put ((db kc-dbm) (key integer) (value vector) &key (mode :replace))
  (let ((func (put-method-for mode)))
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