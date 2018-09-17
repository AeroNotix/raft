(defpackage raft/binary
  (:use :cl)
  (:export #:read-sized-string-from-stream))

(in-package raft/binary)

;; TODO: expand this into its own system, handle endianness


(defun write-sized (value n stream)
  (let ((value (ldb (byte n 0) value)))
    ;; silly byte counting
    (length (loop for i from (/ n 8) downto 1
               collect
                 (write-byte (ldb (byte 8 (* (1- i) 8)) value) stream)))))

(defun read-sized (n stream)
  (let ((unsigned-value 0)
        (byte-size (/ n 8)))
    (dotimes (i byte-size)
      (setf unsigned-value (+ (* unsigned-value #x100)
                              (read-byte stream))))
    unsigned-value))

(defmacro define-binary-write (name size)
  (labels ((create-symbol (fmt name)
             (intern (format nil fmt name) *package*)))
    (let ((write-fun-name (create-symbol "WRITE-~A" name))
          (read-fun-name (create-symbol "READ-~A" name)))
      `(progn
         (defun ,write-fun-name (value stream)
           (write-sized value ,size stream))
         (defun ,read-fun-name (stream)
           (read-sized ,size stream))

         (export (list ',write-fun-name ',read-fun-name) *package*)))))

(define-binary-write octet  8)
(define-binary-write short  16)
(define-binary-write int    32)
(define-binary-write bigint 64)

(defmethod read-sized-string-from-stream ((stream stream))
  (let* ((string-size (read-byte stream))
         (buffer (make-array string-size)))
    (read-sequence buffer stream)
    (flexi-streams:octets-to-string buffer)))
