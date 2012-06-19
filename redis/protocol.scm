;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; Redis Protocol Handler
;;; Note: Currently this module only support (new) unified request protocol.
;;;

(define-module redis.protocol
  (use gauche.uvector)
  (export-all))
(select-module redis.protocol)

(define-condition-type <redis-error> <error>
  redis-error?)

(define-method object-equal? ((err1 <redis-error>) (err2 <redis-error>))
  (equal? (slot-ref err1 'message) (slot-ref err2 'message)))

(define (do-command-sync in out cmd . args)
  (apply unified-request out cmd args)
  (parse-reply in))

(define (eod out)
  (display "\r\n" out))

(define (write-arg out arg)
  (let1 argstr (x->string arg)
    (display "$" out)
    (display (string-size argstr) out)
    (eod out)
    (display argstr out)
    (eod out)))

(define (unified-request out cmd . args)
  (display "*" out)
  (display (+ 1 (length args)) out)
  (eod out)
  (write-arg out cmd)
  (for-each (cut write-arg out <>) args))

(define (parse-reply in :optional (return-error? #f))
  (case (read-char in)
    ((#\+) (status-reply in))
    ((#\-) (error-reply in return-error?))
    ((#\:) (integer-reply in))
    ((#\$) (bulk-reply in))
    ((#\*) (multi-bulk-reply in))
    (else
     => (lambda (c)
          (if (eof-object? c)
              c
              (error "Unknown prefix: " c))))))

(define (status-reply in)
  (string->symbol (read-line in)))

(define (error-reply in :optional (return-error? #f))
  (let1 exc (make-condition <redis-error> 'message (read-line in))
    (if return-error?
        exc
        (raise exc))))

(define (integer-reply in)
  (x->integer (read-line in)))

(define (bulk-reply in)
  (let1 size (x->integer (read-line in))
    (and (>= size 0)
         (begin0
           (read-block-greedily size in)
           (read-line in) ; ignore
           ))))

(define (multi-bulk-reply in)
  (let* ((count (x->integer (read-line in)))
         (ret (make-vector count)))
    (dotimes (i count)
      (vector-set! ret i (parse-reply in #t)))
    ret))

(define (read-block-greedily size in)
  (let1 buf (make-u8vector size)
    (let loop ((start 0))
      (let1 readsize (read-block! buf in start size)
        (cond ((eof-object? readsize) (u8vector->string buf 0 start))
              ((>= start size) (u8vector->string buf))
              (else (loop (+ start readsize))))))))
