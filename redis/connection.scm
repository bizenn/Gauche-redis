;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; Redis Connection Handler
;;;

(define-module redis.connection
  (use gauche.net)
  (export-all))
(select-module redis.connection)

(define-constant *DEFAULT-PORT* 6379)

(define (redis-default-port) *DEFAULT-PORT*)

(define-class <redis-connection> ()
  ((socket :init-keyword :socket)
   (in :init-keyword :input-port)
   (out :init-keyword :output-port)))

(define (redis-open host :optional (port *DEFAULT-PORT*))
  (let1 socket (make-client-socket 'inet host port)
    (make <redis-connection>
      :socket socket
      :input-port (socket-input-port socket)
      :output-port (socket-output-port socket))))

(define-method redis-close ((conn <redis-connection>) . maybe-shutdown?)
  (when (get-optional maybe-shutdown? #t)
    (socket-shutdown (ref conn 'socket)))
  (socket-close (ref conn 'socket)))

(define-method redis-closed? ((conn <redis-connection>))
  (or (not (slot-bound? conn 'socket))
      (not (eq? 'connected (socket-status (slot-ref conn 'socket))))
      (not (slot-bound? conn 'in))
      (port-closed? (slot-ref conn 'in))
      (not (slot-bound? conn 'out))
      (port-closed? (slot-ref conn 'out))))
