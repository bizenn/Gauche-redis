#!/usr/bin/env gosh
;;; -*- mode: scheme; coding: utf-8 -*-

(use gauche.test)
(use _redis-test-util)

(test-start "dbm.redis")
(use dbm)
(use dbm.redis)
(test-module 'dbm.redis)

(define-constant *redis-server-cmd* (car *argv*))
(define-constant *bind-address* "127.0.0.1")
(define-constant *bind-port* 16379)

(define *redis-server* (redis-server-start *redis-server-cmd* *bind-port*))
(sys-sleep 1)

(let1 dbm (dbm-open <redis> :host *bind-address* :port *bind-port*)
  (test* "dbm-open" <redis> (class-of dbm))
  (test* "dbm-closed?" #f (dbm-closed? dbm))
  (test* "dbm-put!(not exist)" 'OK (dbm-put! dbm "1" "one"))
  (test* "dbm-get(not exist)" (test-error) (dbm-get dbm "2"))
  (test* "dbm-exists?(not exist)" #f (dbm-exists? dbm "2"))
  (test* "dbm-put!(key exist)" 'OK (dbm-put! dbm "1" "uno"))
  (test* "dbm-get(key exist)" "uno" (dbm-get dbm "1"))
  (test* "dbm-exists?(key exist)" #t (dbm-exists? dbm "1"))
  (test* "dbm-delete!(key exist)" #t (dbm-delete! dbm "1"))
  (test* "dbm-delete!(not exist)" #f (dbm-delete! dbm "1"))
  (test* "dbm-delete!(result)" #f (dbm-exists? dbm "1"))
  (test* "dbm-fold(no keys)" '() (dbm-fold dbm acons '()))
  (for-each (cut apply dbm-put! dbm <>)
            '((1 "one") (2 "two") (3 "three") (4 "four") (5 "five")))
  (test* "dbm-fold"
         '(("1" . "one") ("2" . "two") ("3" . "three") ("4" . "four") ("5" . "five"))
         (sort! (dbm-fold dbm acons '())
                (lambda (a b) (string<? (car a) (car b)))))
  (test* "dbm-close" #t (dbm-close dbm))
  (test* "dbm-closed?" #t (dbm-closed? dbm)))

(test* "dbm-db-exists?" #t (dbm-db-exists? <redis> #`",|*bind-address*|:,|*bind-port*|"))
(test* "dbm-db-exists?" #f (dbm-db-exists? <redis> #`",|*bind-address*|:,(+ 1 *bind-port*)"))
(test* "dbm-db-remove" #t (dbm-db-remove <redis> #`",|*bind-address*|:,|*bind-port*|"))
(test* "dbm-db-remove" #f (dbm-db-remove <redis> #`",|*bind-address*|:,(+ 1 *bind-port*)"))

(redis-server-stop *redis-server*)

(test-end :exit-on-failure #t)
