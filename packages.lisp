;;
;; Copyright (c) 2005, Gigamonkeys Consulting All rights reserved.
;;

(in-package :cl-user)

(defpackage :binary-data
  (:use :common-lisp :alexandria)
  (:nicknames :binary-data)
  (:export :define-binary-class
           :define-tagged-binary-class
           :define-binary-type
	   :define-enumeration
           :read-value
	   :type->read-value
           :write-value
	   :generic-read-byte
	   :generic-write-byte
           :*in-progress-objects*
           :parent-of-type
           :immediate-parent
           :current-binary-object
           :+null+))

(defpackage :binary-data.common-datatypes
  (:use :common-lisp :binary-data)
  (:nicknames :monkey-types)
  (:export
   :optional
   :u1
   :u2
   :u3 
   :u4
   :u8
   :*endianness*
   :u1-o
   :u2-o
   :u3-o
   :u4-o
   :u8-o
   :s1-o
   :s2-o
   :s3-o
   :s4-o
   :s8-o
   :generic-string 
   :generic-terminated-string
   :swap-bytes
   :iso-8859-1-char 
   :iso-8859-1-string 
   :iso-8859-1-terminated-string 
   :ucs-2-char 
   :ucs-2-char-big-endian 
   :ucs-2-char-little-endian 
   :ucs-2-char-type 
   :ucs-2-string 
   :ucs-2-terminated-string 
   :unsigned-integer

   ;; Conditions/restarts
   :no-symbol-for-value-enum
   :return-unknown-value))
