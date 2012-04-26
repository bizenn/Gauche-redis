;;; -*- mode: scheme; coding: utf-8 -*-
;;; Test redis.protocol

(use gauche.test)

(test-start "redis.protocol")
(use redis.protocol)
(test-module 'redis.protocol)

(test-section "Request (only unified request)")
(test* "eod(End Of Data)"
       "\r\n"
       (call-with-output-string (cut eod <>)))
(test* "write-arg"
       "$5\r\nabcde\r\n"
       (call-with-output-string (cut write-arg <> "abcde")))
(test* "unified-request"
       "*3\r\n$3\r\nSET\r\n$1\r\n1\r\n$5\r\nabcde\r\n"
       (call-with-output-string (cut unified-request <> 'SET 1 "abcde")))

(test-section "Reply")
(test* "status-reply" 'OK (call-with-input-string "+OK\r\n" parse-reply))
(test* "error-reply" (test-error) (call-with-input-string "-ERR\r\n" parse-reply))
(test* "integer-reply" 12345 (call-with-input-string ":12345\r\n" parse-reply))
(test* "bulk-reply" "abcde" (call-with-input-string "$5\r\nabcde\r\n" parse-reply))
(test* "multi-bulk-reply"
       '#("abcde" #f "fghi")
       (call-with-input-string "*3\r\n$5\r\nabcde\r\n$-1\r\n$4\r\nfghi\r\n" parse-reply))

(test-end :exit-on-failure #t)

