;;; -*- mode: scheme; coding: utf-8 -*-
;;; Test redis.connection

(use gauche.test)
(use gauche.net)
(use _redis-test-util)

(test-start "redis.connection")
(use redis.connection)
(test-module 'redis.connection)

(define-constant *redis-server-cmd* (car *argv*))
(define-constant *bind-address* "127.0.0.1")
(define-constant *bind-port* 16379)

(define *redis-server* (redis-server-start *redis-server-cmd* *bind-port*))
(sys-sleep 1)

(let1 redis (redis-open *bind-address* *bind-port*)
  (test* "redis-open" <redis-connection> (class-of redis))
  (test* "redis-open:socket-status" 'connected (socket-status (ref redis 'socket)))
  (test* "redis-open:input-port" #t (input-port? (ref redis 'in)))
  (test* "redis-open:input-port:closed?" #f (port-closed? (ref redis 'in)))
  (test* "redis-open:output-port" #t (output-port? (ref redis 'out)))
  (test* "redis-open:output-port:closed?" #f (port-closed? (ref redis 'out)))
  (redis-close redis)
  (test* "redis-close" <redis-connection> (class-of redis))
  (test* "redis-close:socket-status" 'closed (socket-status (ref redis 'socket)))
  (test* "redis-close:input-port" #t (input-port? (ref redis 'in)))
  (test* "redis-close:input-port:closed?" #t (port-closed? (ref redis 'in)))
  (test* "redis-close:output-port" #t (output-port? (ref redis 'out)))
  (test* "redis-close:output-port:closed?" #t (port-closed? (ref redis 'out)))
  )

(redis-server-stop *redis-server*)

(test-end :exit-on-failure #t)
