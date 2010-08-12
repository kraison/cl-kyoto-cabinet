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
	   :kccurkey
	   :kccurvalue
	   :kccurdb
	   :kccurjumpkey

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
   #:dbm-delete
   #:dbm-vanish
   #:dbm-put
   #:dbm-get
   #:dbm-rem
   #:dbm-abort
   #:iter-open
   #:iter-first
   #:iter-last
   #:iter-prev
   #:iter-next
   #:iter-jump
   #:iter-put
   #:iter-rem
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

