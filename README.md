monkeylib-binary-data
=====================

Binary data library based on code from chapter 24 of Practical Common Lisp

Non-book alterations
====================

This version has some updates to handle endianness in the generic integers, updated enumeration definition (which I don't think was even in the book), and added tagged data structure inheritance.

Also added are pre/post read/write hooks for individual data items.  Let's say you have a regular binary class where you want to take an action based on an endianness indicator.

(define-binary-class special-file ()
  ((endian endianness)
   (data1 (u4-o :order endian))
   (data2 (u4-o :order endian))
   (data3 (u4-o :order endian))
   (data4 (u4-o :order endian)))

Instead, you can add a post-read hook to endian.

(define-binary-class special-file ()
  ((endian endianness
      :post-read (setf monkey-types:*endianness* endian)
      :post-write (setf monkey-types:*endianness* endian))
   (data1 u4)
   (data2 u4)
   (data3 u4)
   (data4 u4)))
