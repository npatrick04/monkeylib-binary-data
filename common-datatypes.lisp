;;
;; Copyright (c) 2005, Gigamonkeys Consulting All rights reserved.
;;

(in-package :com.gigamonkeys.binary-data.common-datatypes)

(defvar *endianness* :big-endian
  "*endianness* defines how variables are read or written.")

(define-binary-type unsigned-integer (bytes bits-per-byte order)
  (:reader (in)
	   (loop with value = 0
	      for low-bit downfrom (* bits-per-byte (1- bytes)) to 0 by bits-per-byte do
		(setf (ldb (byte bits-per-byte low-bit) value) (generic-read-byte in))
	      finally (if (eql (or order *endianness*) :big-endian)
			  (return value)
			  (return (swap-bytes value bytes)))))
  (:writer (out value)
	   (let ((final-value (if (eql (or order *endianness*) :big-endian)
				  value
				  (swap-bytes value bytes))))
	     (loop for low-bit downfrom (* bits-per-byte (1- bytes)) to 0 by bits-per-byte
		do (generic-write-byte (ldb (byte bits-per-byte low-bit) final-value) out)))))

(define-binary-type u1 () (unsigned-integer :bytes 1 :bits-per-byte 8))
(define-binary-type u2 () (unsigned-integer :bytes 2 :bits-per-byte 8))
(define-binary-type u3 () (unsigned-integer :bytes 3 :bits-per-byte 8))
(define-binary-type u4 () (unsigned-integer :bytes 4 :bits-per-byte 8))
(define-binary-type u8 () (unsigned-integer :bytes 8 :bits-per-byte 8))

(define-binary-type u1-o (order) (unsigned-integer :bytes 1 :bits-per-byte 8 :order order))
(define-binary-type u2-o (order) (unsigned-integer :bytes 2 :bits-per-byte 8 :order order))
(define-binary-type u3-o (order) (unsigned-integer :bytes 3 :bits-per-byte 8 :order order))
(define-binary-type u4-o (order) (unsigned-integer :bytes 4 :bits-per-byte 8 :order order))
(define-binary-type u8-o (order) (unsigned-integer :bytes 8 :bits-per-byte 8 :order order))

(define-binary-type signed-integer (bytes bits-per-byte order)
  (:reader (in)
	   (loop with value = 0
	      for low-bit downfrom (* bits-per-byte (1- bytes)) to 0 by bits-per-byte do
		(setf (ldb (byte bits-per-byte low-bit) value) (generic-read-byte in))
	      finally (if (eql (or order *endianness*) :big-endian)
			  (return (coerce value '(signed-byte (* 8 bytes))))
			  (return (coerce (swap-bytes value bytes) '(signed-byte (* 8 bytes)))))))
  (:writer (out value)
	   (let ((final-value (coerce (if (eql (or order *endianness*) :big-endian)
                                          value
                                          (swap-bytes value bytes))
                                      '(signed-byte (* 8 bytes)))))
	     (loop for low-bit downfrom (* bits-per-byte (1- bytes)) to 0 by bits-per-byte
		do (generic-write-byte (ldb (byte bits-per-byte low-bit) final-value) out)))))

(define-binary-type s1-o (order) (signed-integer :bytes 1 :bits-per-byte 8 :order order))
(define-binary-type s2-o (order) (signed-integer :bytes 2 :bits-per-byte 8 :order order))
(define-binary-type s3-o (order) (signed-integer :bytes 3 :bits-per-byte 8 :order order))
(define-binary-type s4-o (order) (signed-integer :bytes 4 :bits-per-byte 8 :order order))
(define-binary-type s8-o (order) (signed-integer :bytes 8 :bits-per-byte 8 :order order))

;;; Strings

(define-binary-type generic-string (length character-type)
  (:reader (in)
    (let ((string (make-string length)))
      (dotimes (i length)
        (setf (char string i) (read-value character-type in)))
      string))
  (:writer (out string)
    (dotimes (i length)
      (write-value character-type out (char string i)))))

(define-binary-type generic-terminated-string (terminator character-type)
  (:reader (in)
    (with-output-to-string (s)
      (loop for char = (read-value character-type in)
            until (char= char terminator) do (write-char char s))))
  (:writer (out string)
    (loop for char across string
          do (write-value character-type out char)
          finally (write-value character-type out terminator))))

;;; ISO-8859-1 strings

(define-binary-type iso-8859-1-char ()
  (:reader (in)
    (let ((code (generic-read-byte in)))
      (or (code-char code)
          (error "Character code ~d not supported" code))))
  (:writer (out char)
    (let ((code (char-code char)))
      (if (<= 0 code #xff)
          (generic-write-byte code out)
          (error "Illegal character for iso-8859-1 encoding: character: ~c with code: ~d" char code)))))

(define-binary-type iso-8859-1-string (length)
  (generic-string :length length :character-type 'iso-8859-1-char))

(define-binary-type iso-8859-1-terminated-string (terminator)
  (generic-terminated-string :terminator terminator :character-type 'iso-8859-1-char))

;;; UCS-2 (Unicode) strings (i.e. UTF-16 without surrogate pairs, phew.)

;;; Define a binary type for reading a UCS-2 character relative to a
;;; particular byte ordering as indicated by the BOM value.
 ;; v2.3 specifies that the BOM should be present. v2.2 is silent
 ;; though it is arguably inherent in the definition of UCS-2) Length
 ;; is in bytesty. On the write side, since we don't have any way of
 ;; knowing what BOM was used to read the string we just pick one.
 ;; This does mean roundtrip transparency could be broken.

(define-binary-type ucs-2-char (swap)
  (:reader (in)
    (let ((code (read-value 'u2 in)))
      (when swap (setf code (swap-bytes code 2)))
      (or (code-char code) (error "Character code ~d not supported" code))))
  (:writer (out char)
    (let ((code (char-code char)))
      (unless (<= 0 code #xffff)
        (error "Illegal character for ucs-2 encoding: ~c with char-code: ~d" char code))
      (when swap (setf code (swap-bytes code 2)))
      (write-value 'u2 out code))))

;; TODO See if this can be more efficient or can incorporate an
;; assert.  As it stands, this version will simply chop off any bits
;; over the max number of bytes indicated by size.
(defun swap-bytes (value size)
  "Swap endianness"
  (let ((result 0))
    (loop for i from 0 to (* 8 (1- size)) by 8
	 for j downfrom (* 8 (1- size)) to 0 by 8
	 do (setf (ldb (byte 8 i) result)
		  (ldb (byte 8 j) value))
	 finally (return result))))
;; (defun swap-bytes (code)
;;   (assert (<= code #xffff))
;;   (rotatef (ldb (byte 8 0) code) (ldb (byte 8 8) code))
;;   code)

(define-binary-type ucs-2-char-big-endian () (ucs-2-char :swap nil))

(define-binary-type ucs-2-char-little-endian () (ucs-2-char :swap t))

(defun ucs-2-char-type (byte-order-mark)
  (ecase byte-order-mark
    (#xfeff 'ucs-2-char-big-endian)
    (#xfffe 'ucs-2-char-little-endian)))

(define-binary-type ucs-2-string (length)
  (:reader (in)
    (let ((byte-order-mark (read-value 'u2 in))
          (characters (1- (/ length 2))))
      (read-value
       'generic-string in
       :length characters
       :character-type (ucs-2-char-type byte-order-mark))))
  (:writer (out string)
    (write-value 'u2 out #xfeff)
    (write-value
     'generic-string out string
     :length (length string)
     :character-type (ucs-2-char-type #xfeff))))

(define-binary-type ucs-2-terminated-string (terminator)
  (:reader (in)
    (let ((byte-order-mark (read-value 'u2 in)))
      (read-value
       'generic-terminated-string in
       :terminator terminator
       :character-type (ucs-2-char-type byte-order-mark))))
  (:writer (out string)
    (write-value 'u2 out #xfeff)
    (write-value 
     'generic-terminated-string out string
     :terminator terminator
     :character-type (ucs-2-char-type #xfeff))))

(define-binary-type optional (type if)
  (:reader (in)
	   (when if (read-value type in)))
  (:writer (out data)
	   (when if (write-value type out data))))
