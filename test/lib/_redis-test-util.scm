;;; -*- mode: scheme; coding: utf-8 -*-
(define-module _redis-test-util
  (use gauche.process)
  (export-all))
(select-module _redis-test-util)

(define-constant *config-format*
  "
port ~d
bind 127.0.0.1
logfile /dev/null
dir ./
")

(define (redis-server-start redis-server port)
  (let* ((p (run-process `(,redis-server "-") :redirects '((< 0 stdin))))
         (out (process-input p 'stdin)))
    (format out *config-format* port)
    (close-output-port out)
    p))

(define (redis-server-stop redis-server-process)
  (process-kill redis-server-process)
  (process-wait redis-server-process))
