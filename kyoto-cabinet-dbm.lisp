(in-package #:kyoto-cabinet)

(defmethod initialize-instance :after ((db kc-dbm) &key)
  (with-slots (ptr) db
    (setf ptr (kcdbnew))))

(defmethod initialize-instance :after ((iter kc-iterator) &key)
  (with-slots (ptr) iter
    (setf ptr iter)))


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


(defmethod iter-item ((iter kc-iterator))
  (let ((key-size (foreign-alloc :pointer))
	(value-size (foreign-alloc :pointer))
	(value-ptr (foreign-alloc :pointer)))
    (with-string-value (key-ptr (kccurget (ptr-of iter) key-size value-ptr value-size NIL))
      (foreign-free key-size)
      (foreign-free value-size)
      (format t "KEY: ~a~%" key-ptr)
      (format t "VALUE: ~a~%" (foreign-string-to-lisp value-ptr))
      (let ((key (foreign-string-to-lisp key-ptr)) (value (foreign-string-to-lisp value-ptr)))
	(foreign-free key-ptr)
	(foreign-free value-ptr)
	(if (null-pointer-p key-ptr)
	    (maybe-raise-error (kccurdb (ptr-of iter)))
	    (list key value))))))

(defmethod iter-first ((iter kc-iterator))
  (kccurjump (ptr-of iter)))

(defmethod iter-next ((iter kc-iterator))
  (kccurstep (ptr-of iter)))

;; (defmethod iter-key ((iter kc-iterator))
  

;;(defmethod iter-iterate ((iter kc-iterator) (fn function))
;;  (iter-first iter)
;;  (loop while (iter-next iter) do
;;       (funcall fn 
  


