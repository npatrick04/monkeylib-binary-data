(defpackage :binary-data.test
  (:use :common-lisp
        :binary-data
        :binary-data.common-datatypes
        :flexi-streams
        :5am))
(in-package :binary-data.test)

(defun range (r1 &optional r2)
  (if r2
      (loop for i from r1 to r2 collect i)
      (if (consp r1)
          (apply #'range r1)
          (loop for i from 1 to r1 collect i))))

(def-suite common-data-types)
(def-fixture n-stream (range)
  (let ((in (flexi-streams:make-in-memory-input-stream
             (apply #'vector
                    (range range)))))
    (&body)))

(test read-uint
  (with-fixture n-stream ((+ 1 2 3 4 5))
    (is (= 1 (read-value 'u1 in)))
    (is (= #x203 (read-value 'u2 in)))
    (is (= #x40506 (read-value 'u3 in)))
    (is (= #x708090a (read-value 'u4 in)))
    (is (= #xb0c0d0e0f (read-value 'unsigned-integer in
                                   :bytes 5 :bits-per-byte 8))))
  (mapc (lambda (range-val seq-val)
          (is (= range-val seq-val))) 
        (range 15)
        (flexi-streams:with-output-to-sequence
            (out :as-list t)
          (write-value 'u1 out 1)
          (write-value 'u2 out #x203)
          (write-value 'u3 out #x40506)
          (write-value 'u4 out #x708090a)
          (write-value 'unsigned-integer out
                       #xb0c0d0e0f :bytes 5 :bits-per-byte 8)))
  (with-fixture n-stream ((+ 1 2 3 4 5))
    (is (= 1 (read-value 'u1-o in :order :little-endian)))
    (is (= #x302 (read-value 'u2-o in  :order :little-endian)))
    (is (= #x60504 (read-value 'u3-o in :order :little-endian)))
    (is (= #xa090807 (read-value 'u4-o in :order :little-endian)))
    (is (= #xf0e0d0c0b (read-value 'unsigned-integer in :bytes 5 :bits-per-byte 8 :order :little-endian))))
  (mapc (lambda (range-val seq-val)
          (is (= range-val seq-val)))
        '(1 3 2 6 5 4 10 9 8 7 15 14 13 12 11)
        (flexi-streams:with-output-to-sequence
            (out :as-list t)
          (write-value 'u1-o out 1 :order :little-endian)
          (write-value 'u2-o out #x203 :order :little-endian)
          (write-value 'u3-o out #x40506 :order :little-endian)
          (write-value 'u4-o out #x708090a :order :little-endian)
          (write-value 'unsigned-integer out #xb0c0d0e0f :bytes 5
                       :bits-per-byte 8 :order :little-endian))))
(test read-int
  (with-fixture n-stream ('(255 255))
    (is (= -1 (read-value 's1-o in)))))
