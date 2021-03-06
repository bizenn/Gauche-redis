;;; -*- mode: scheme; coding: utf-8 -*-
;;; Test redis.protocol

(use gauche.test)
(use util.queue)
(use _redis-test-util)

(test-start "redis.async")
(use redis)
(use redis.async)
(test-module 'redis.async)


(define-constant *redis-server-cmd* (car *argv*))
(define-constant *bind-address* "127.0.0.1")
(define-constant *bind-port* 16379)

(define *redis-server* (redis-server-start *redis-server-cmd* *bind-port*))
(sys-sleep 1)

(let1 redis (redis-open-async *bind-address* *bind-port*)
  (test* "redis-open-async" <redis-async-connection> (class-of redis))
  (test-section "redis-async-threads")
  (let ((send-queue (ref redis 'send-queue))
        (recv-queue (ref redis 'recv-queue))
        (hndl-queue (ref redis 'hndl-queue)))
    (test* "redis-async-send-queue" #t (mtqueue? send-queue))
    (test* "redis-async-recv-queue" #t (mtqueue? recv-queue))
    (test* "redis-async-response-handler-queue" #t (queue? hndl-queue))

    (test-section "redis-async")
    ((redis-async-set redis "A" "123") (cut test* "set" 'OK <>))
    ((redis-async-get redis "A") (cut test* "get" "123" <>))
    ((redis-async-del redis "A") (cut test* "delete" 1 <>))

    (until (queue-empty? hndl-queue)
      (redis-async-update! redis))
    ))


(test-section "redis-async-pubsub")

(let ((pub (redis-open *bind-address* *bind-port*))
      (sub (redis-open-async *bind-address* *bind-port*)))

  (define (test-eql x)
    (lambda (res) (test* "subscribe" `("message" "channel" ,x) (vector->list res))))

  ((redis-async-subscribe sub "channel")
   (lambda (res) (test* "subscribe" '("subscribe" "channel" 1) (vector->list res))))
  (until (queue-empty? (ref sub 'hndl-queue)) (redis-async-update! sub))

  (redis-publish pub "channel" "0")
  ((redis-async-wait-for-publish! sub) (test-eql "0"))
  (redis-async-update! sub)
  (redis-publish pub "channel" "1")
  ((redis-async-wait-for-publish! sub) (test-eql "1"))
  (redis-async-update! sub)
  (redis-publish pub "channel" "2")
  (redis-publish pub "channel" "3")
  ((redis-async-wait-for-publish! sub) (test-eql "2"))
  ((redis-async-wait-for-publish! sub) (test-eql "3"))
  (redis-publish pub "channel" "4")
  (redis-async-update! sub)
  (redis-publish pub "channel" "5")
  ((redis-async-wait-for-publish! sub) (test-eql "4"))
  ((redis-async-wait-for-publish! sub) (test-eql "5"))
  (redis-async-update! sub)
  )

(redis-server-stop *redis-server*)

(test-end :exit-on-failure #t)
