;;; -*- mode: scheme; coding: utf-8 -*-

(define-module dbm.redis
  (extend redis)
  (use dbm)
  (use gauche.sequence)
  (export-all))
(select-module dbm.redis)

(define-class <redis-meta> (<dbm-meta>)
  ())

(define-class <redis> (<dbm>)
  ((host :init-keyword :host)
   (port :init-keyword :port)
   (connection :getter connection-of))
  :metaclass <redis-meta>)

(define-method dbm-open ((dbm <redis>))
  (next-method)
  (unless (slot-bound? dbm 'host)
    (error "host must be set to connect as the redis server"))
  (unless (slot-bound? dbm 'port)
    (slot-set! dbm 'port (redis-default-port)))
  (slot-set! dbm 'connection (redis-open (slot-ref dbm 'host) (slot-ref dbm 'port)))
  dbm)

(define-method dbm-close ((dbm <redis>))
  (and-let* (((slot-bound? dbm 'connection))
             (redis (connection-of dbm)))
    (begin0
      (redis-close redis)
      (slot-set! dbm 'connection #f))))

(define-method dbm-closed? ((dbm <redis>))
  (and (slot-bound? dbm 'connection)
       (let1 redis (connection-of dbm)
         (or (not redis)
             (redis-closed? redis)))))

(define-method dbm-put! ((dbm <redis>) key value)
  (redis-set (connection-of dbm) key value))

(define-method dbm-get ((dbm <redis>) key)
  (or (redis-get (connection-of dbm) key)
      (errorf "Key ~a does not exist" key)))

(define-method dbm-exists? ((dbm <redis>) key)
  (eqv? 1 (redis-exists (connection-of dbm) key)))

(define-method dbm-delete! ((dbm <redis>) key)
  (eqv? 1 (redis-del (connection-of dbm) key)))

(define-method dbm-fold ((dbm <redis>) proc knil)
  (let* ((redis (connection-of dbm))
         (keys (redis-keys redis "*"))
         (key-count (vector-length keys)))
    (let loop ((acc knil)
               (i 0))
      (if (< i key-count)
          (let* ((k (vector-ref keys i))
                 (v (redis-get redis k)))
            (loop (proc k v acc) (+ i 1)))
          acc))))

(define (parse-redis-db-name name)
  (rxmatch-if (#/^(.+):(\d+)?/ name) (#f host port)
    (list host (x->integer (or port (redis-default-port))))
    (error "Cannot recognize as host:port : " name)))

(define-method dbm-db-exists? ((class <dbm-meta>) name)
  (guard (e (else #f))
    (and-let* ((redis (apply redis-open (parse-redis-db-name name))))
      (redis-close redis)
      #t)))

(define-method dbm-db-remove ((class <dbm-meta>) name)
  (guard (e (else #f))
    (and-let* ((redis (apply redis-open (parse-redis-db-name name))))
      (redis-flushdb redis)
      #t)))
