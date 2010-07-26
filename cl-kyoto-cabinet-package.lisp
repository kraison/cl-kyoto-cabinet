(defpackage :kyoto-cabinet-ffi
  (:use #:common-lisp #:cffi)
  (:export)
  (:documentation "CFFI interface to Kyoto Cabinet functions. The
  original C function names are preserved."))

(defpackage :kyoto-cabinet
  (:use #:common-lisp #:cffi #:kyoto-cabinet-ffi)
  (:nicknames #:kc)
  (:export)
  (:documentation "A Lisp-style abstract interface to Kyoto
  Cabinet. The original C function names are not preserved (see
  the :kyoto-cabinet-ffi package for functions that do preserve the
  nomenclature)."))

