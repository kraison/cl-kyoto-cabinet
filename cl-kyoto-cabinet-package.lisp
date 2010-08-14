(defpackage #:kyoto-cabinet-ffi
  (:use #:common-lisp #:cffi)
  (:export :kcdbnew
	   :kcdbopen
	   :kcdbecode
	   :kcdbemsg
	   :kcdbdel
	   :kcdbclose
	   :kcdbclear
	   :kcdbbegintran
	   :kcdbendtran
	   :kcdbset
	   :kcdbadd
	   :kcdbappend
	   :kcdbget
	   :kcdbremove

	   :kcdbcursor
	   :kccurdel
	   :kccurjump
	   :kccurstep
	   :kccurget
	   :kccurgetvalue	   
	   :kccurgetkey
	   :kccurdb
	   :kccurjumpkey
	   :kccurremove

	   :dbm-open-flags
	   :dbm-return-values)
  
  (:documentation "CFFI interface to Kyoto Cabinet functions. The
  original C function names are preserved."))

(defpackage #:kyoto-cabinet
  (:use #:common-lisp #:cffi #:kyoto-cabinet-ffi)
  (:nicknames #:kc)
  (:export
   ;; Classes
   #:kc-dbm

   ;; Generics
   #:dbm-open
   #:dbm-close
   #:dbm-begin
   #:dbm-commit
   #:dbm-rollback
   #:dbm-delete
   #:dbm-remove
   #:dbm-put
   #:dbm-get
   #:iter-open
   #:iter-first
   #:iter-last
   #:iter-prev
   #:iter-next
   #:iter-go-to
   #:iter-put
   #:iter-remove
   #:iter-key
   #:iter-get
   #:iter-close
   #:iter-item

   #:dbm-num-records
   #:dbm-file-size
   #:dbm-optimize
   #:dbm-cache
   #:dbm-xmsize
   #:set-comparator

   ;; Macros
   #:with-database
   #:with-transaction
   #:with-iterator)
  (:documentation "A Lisp-style abstract interface to Kyoto
  Cabinet. The original C function names are not preserved (see
  the :kyoto-cabinet-ffi package for functions that do preserve the
  nomenclature)."))

