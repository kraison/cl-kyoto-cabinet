(in-package #:kyoto-cabinet)

(defmethod initialize-instance :after ((db kc-dbm) &key instance)     
  (with-slots (ptr) db
    (if (null instance)
	(setf ptr (kcdbnew))
	(setf ptr instance))))

(defmethod initialize-instance :after ((iter kc-iterator) &key)
  (with-slots (ptr) iter
    (setf ptr iter)))


(defmethod raise-error ((db kc-dbm) &optional (message "")
			&rest message-arguments)
  (let* ((code (kcdbecode (ptr-of db)))
	 (msg (kcdbemsg (ptr-of db))))
    (error 'dbm-error :error-code code :error-msg msg
	   :text (apply #'format nil message message-arguments))))


(defmethod raise-error ((iter kc-iterator) &optional (message "")
			&rest message-arguments)
  (raise-error (make-instance 'kc-dbm :instance (kccurdb (ptr-of iter))) message message-arguments))

(defmethod maybe-raise-error ((db kc-dbm) &optional message
			      &rest message-arguments)
  (let ((ecode (kcdbecode (ptr-of db))))
    (maybe-raise-error-with-ecode db ecode message message-arguments)))

(defmethod maybe-raise-error ((iter kc-iterator) &optional message
			      &rest message-arguments)
  (let ((ecode (kcdbecode (kccurdb (ptr-of iter)))))
    (maybe-raise-error-with-ecode iter ecode message message-arguments)))
	   
(defun maybe-raise-error-with-ecode (what ecode &optional message
				     &rest message-arguments)
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
	   (apply #'raise-error what message message-arguments))))

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


;; Define overloaded remove methods

(defmethod dbm-remove ((db kc-dbm) (key string))
  (rem-string->value db key))

(defmethod dbm-remove ((db kc-dbm) (key integer))
  (rem-int32->value  db key))

(defmethod dbm-remove ((db kc-dbm) (key vector))
  (rem-octets->value  db key))


;; Define iterator methods below

(defmethod iter-open ((db kc-dbm))
  (let ((iterator (make-instance 'kc-iterator)))
    (with-slots ((iter-ptr ptr)) iterator
      (with-slots ((db-ptr ptr)) db
	(setf iter-ptr (kcdbcursor db-ptr))))
    iterator))


(defmethod iter-item ((iter kc-iterator) &key (key-type :string) (value-type :string))
  (let* ((key-size (foreign-alloc :pointer))
	 (key-ptr (foreign-alloc :pointer))
	 (value-size (foreign-alloc :pointer))
	 (value-ptr (foreign-alloc :pointer))
	 (key-ptr (kccurget (ptr-of iter) key-size value-ptr value-size NIL))
	 (key (convert-to-lisp key-type key-ptr key-size))
	 (value (convert-to-lisp value-type (mem-ref value-ptr :pointer) value-size)))
    (format t "TYPE: ~a ~a~%" key-type key-ptr)
    (foreign-free key-size) (foreign-free key-ptr)
    (foreign-free value-size) (foreign-free value-ptr)
    (format t "KEY: ~a~%" key)
    (format t "VALUE: ~a~%" value)
    (if (null key)
	(progn
	  (maybe-raise-error iter)
	  ())
	(values key value))))

(defmethod iter-first ((iter kc-iterator))
  (kccurjump (ptr-of iter)))

(defmethod iter-next ((iter kc-iterator))
  (kccurstep (ptr-of iter)))

(defmethod iter-go-to ((iter kc-iterator) (key string))
  (with-foreign-string ((key-ptr key-len) key :null-terminated-p nil)
    (kccurjumpkey (ptr-of iter) key-ptr key-len)))

(defmethod iter-go-to ((iter kc-iterator) (key integer))
  (with-foreign-object (key-ptr :int32)
      (setf (mem-ref key-ptr :int32) key)
      (kccurjumpkey (ptr-of iter) key-ptr (foreign-type-size :int32))))

(defmethod iter-go-to ((iter kc-iterator) (key vector))
  (let ((key-len (length key)))
    (with-foreign-objects ((key-ptr :unsigned-char key-len))
      (loop for i from 0 below key-len
	 do (setf (mem-aref key-ptr :unsigned-char i) (aref key i)))
    (kccurjumpkey (ptr-of iter) key-ptr key-len))))

(defmethod iter-remove ((iter kc-iterator))
  (kccurremove (ptr-of iter)))

(defmethod iter-key ((iter kc-iterator) &optional (type :string))
  (get-something iter 'kccurgetkey type))

(defmethod iter-value ((iter kc-iterator) &optional (type :string))
  (get-something iter 'kccurgetvalue type))
