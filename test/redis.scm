;;; -*- mode: scheme; coding: utf-8 -*-
;;; Test redis

(use gauche.test)
(use srfi-13)
(use _redis-test-util)

(test-start "redis")
(use redis)
(test-module 'redis)

(define-constant *redis-server-cmd* (car *argv*))
(define-constant *bind-address* "127.0.0.1")
(define-constant *bind-port* 16379)

(define *redis-server* (redis-server-start *redis-server-cmd* *bind-port*))
(sys-sleep 1)

(let1 redis (redis-open *bind-address* *bind-port*)
  (test-section "redis-command")
  (test* "redis-command:ping" 'PONG (redis-command redis 'ping))
  (test* "redis-command:error" (test-error) (redis-command redis 'bogus-command 1 2 3))
  (test-section "Redis each command")

  (test-section "append")
  (test* "redis-get(no value)" #f (redis-get redis 1))
  (test* "redis-append(first)" 4 (redis-append redis 1 "abcd"))
  (test* "redis-get(value exist)" "abcd" (redis-get redis 1))
  (test* "redis-append(second)" 8 (redis-append redis 1 "efgh"))
  (test* "redis-get(appended value)" "abcdefgh" (redis-get redis 1))
  (test* "redis-ping" 'PONG (redis-ping redis))

  (test-section "auth")
  ;; TODO

  (test-section "bgrewriteof")
  ;; TODO

  (test-section "bgsave")
  ;; TODO

  (test-section "blpop")
  ;; TODO

  (test-section "brpop")
  ;; TODO

  (test-section "brpoplpush")
  ;; TODO

  (test-section "config get")
  ;; TODO

  (test-section "config set")
  ;; TODO

  (test-section "dbsize")
  (test* "redis-dbsize" 1 (redis-dbsize redis))

  (test-section "debug object")
  ;; TODO

  (test-section "debug segfault")
  ;; TODO

  (test-section "decr")
  (test* "redis-decr(value is not integer)" (test-error) (redis-decr redis 1))
  (test* "redis-decr(new value)" -1 (redis-decr redis 2))
  (test* "redis-decr(next value)" -2 (redis-decr redis 2))

  (test-section "decrby")
  (test* "redis-decrby(value is not integer)" (test-error) (redis-decrby redis 1 2))
  (redis-del redis 2)
  (test* "redis-decrby(new value)" -2 (redis-decrby redis 2 2))
  (test* "redis-decrby(new value)" -6 (redis-decrby redis 2 4))

  (test-section "del")
  (test* "redis-del(not exist)" 0 (redis-del redis 0))
  (test* "redis-del(multi keys)" 2 (redis-del redis 1 2))

  (test-section "dump")
  ;; TODO

  (test-section "echo")
  (test* "redis-echo" "hello" (redis-echo redis "hello"))

  (test-section "eval")
  ;; TODO

  (test-section "exists")
  (test* "redis-exists(not exist)" 0 (redis-exists redis 0))
  (redis-set redis 0 "zero")
  (test* "redis-exists(exist)" 1 (redis-exists redis 0))

  (test-section "expire")
  (test* "redis-expire(not exist)" 0 (redis-expire redis 1 2))
  (redis-set redis 1 "one")
  (test* "redis-expire(exist)" 1 (redis-expire redis 1 2))
  (sys-sleep 1)
  (test* "redis-exists(after 1sec)" 1 (redis-exists redis 1))
  (sys-sleep 2)
  (test* "redis-exists(after 3secs)" 0 (redis-exists redis 1))

  (test-section "expireat")
  ;; TODO

  (test-section "flushall")
  (redis-mset redis 0 "zero" 1 "one" 2 "two" 3 "three")
  (test* "redis-keys(count)" 4 (vector-length (redis-keys redis "*")))
  (test* "redis-flushall" 'OK (redis-flushall redis))
  (test* "redis-keys(count)" 0 (vector-length (redis-keys redis "*")))

  (test-section "flushdb")
  (redis-mset redis 0 "zero" 1 "one" 2 "two" 3 "three")
  (test* "redis-keys(count)" 4 (vector-length (redis-keys redis "*")))
  (test* "redis-flushdb" 'OK (redis-flushdb redis))
  (test* "redis-keys(count)" 0 (vector-length (redis-keys redis "*")))

  (test-section "get")
  (test* "redis-get(not exist)" #f (redis-get redis 0))
  (redis-set redis 0 "zero")
  (test* "redis-get(exist)" "zero" (redis-get redis 0))

  (test-section "getbit")
  (test* "redis-getbit(new key)" 0 (redis-getbit redis "bitmap" 25))
  (redis-setbit redis "bitmap" 25 1)
  (test* "redis-getbit(set)" 1 (redis-getbit redis "bitmap" 25))
  (redis-setbit redis "bitmap" 25 0)
  (test* "redis-getbit(unset)" 0 (redis-getbit redis "bitmap" 25))

  (test-section "getrange")
  ;;

  (test-section "getset")
  (test* "redis-getset(not exist)" #f (redis-getset redis 1 "one"))
  (test* "redis-getset(exist)" "one" (redis-getset redis 1 "いち"))

  (test-section "hdel")
  ;; TODO

  (test-section "hexists")
  ;; TODO

  (test-section "hget")
  ;; TODO

  (test-section "hgetall")
  ;; TODO

  (test-section "hincrby")
  ;; TODO

  (test-section "hincrbyfloat")
  ;; TODO

  (test-section "hkeys")
  ;; TODO

  (test-section "hlen")
  ;; TODO

  (test-section "hmget")
  ;; TODO

  (test-section "hmset")
  ;; TODO

  (test-section "hset")
  ;; TODO

  (test-section "hsetnx")
  ;; TODO

  (test-section "hvals")
  ;; TODO

  (test-section "incr")
  (redis-set redis 0 "zero")
  (test* "redis-incr(value is not integer)" (test-error) (redis-incr redis 0))
  (redis-del redis 0)
  (test* "redis-incr(not exist)" 1 (redis-incr redis 0))
  (test* "redis-incr(exist)" 2 (redis-incr redis 0))

  (test-section "incrby")
  (redis-set redis 0 "zero")
  (test* "redis-incrby(value is not integer)" (test-error) (redis-incrby redis 0 2))
  (redis-del redis 0)
  (test* "redis-incrby(not exist)" 2 (redis-incrby redis 0 2))
  (test* "redis-incrby(exist)" 6 (redis-incrby redis 0 4))

  (test-section "incrbyfloat")
  ;; TODO

  (test-section "info")
  (test* "redis-info" #t (string? (redis-info redis)))

  (test-section "keys")
  (redis-flushall redis)
  (test* "redis-keys(no keys)" 0 (vector-length (redis-keys redis "*")))
  (redis-mset redis "a1" "one" "a2" "two" "b1" "イチ" "b2" "ニ" "b3" "サン")
  (test* "redis-keys" 2 (vector-length (redis-keys redis "a*")))
  (test* "redis-keys" 3 (vector-length (redis-keys redis "b*")))

  (test-section "lastsave")
  (test* "redis-lastsave" #t (integer? (redis-lastsave redis)))

  (test-section "lindex")
  (redis-lpush redis "l0" "1st elem.")
  (redis-lpush redis "l0" "2nd elem.")
  (test* "redis-lindex(positive)" "2nd elem." (redis-lindex redis "l0" 0))
  (test* "redis-lindex(positive)" "1st elem." (redis-lindex redis "l0" 1))
  (test* "redis-lindex(negative)" "1st elem." (redis-lindex redis "l0" -1))
  (test* "redis-lindex(negative)" "2nd elem." (redis-lindex redis "l0" -2))

  (test-section "linsert")
  (test* "redis-linsert(before)" 3 (redis-linsert redis "l0" 'before "2nd elem." "3rd elem."))
  (test* "redis-linsert(after)" 4 (redis-linsert redis "l0" 'after "1st elem." "4th elem."))
  (test* "redis-linsert(result)"
         #("3rd elem." "2nd elem." "1st elem." "4th elem.")
         (redis-lrange redis "l0" 0 -1))

  (test-section "llen")
  (test* "redis-llen" 4 (redis-llen redis "l0"))

  (test-section "lpop")
  (test* "redis-lpop" "3rd elem." (redis-lpop redis "l0"))
  (test* "redis-lpop" "2nd elem." (redis-lpop redis "l0"))
  (test* "redis-lpop" "1st elem." (redis-lpop redis "l0"))
  (test* "redis-lpop" "4th elem." (redis-lpop redis "l0"))
  (test* "redis-lpop" #f (redis-lpop redis "l0"))

  (test-section "lpush")
  (test* "redis-lpush(single value)" 1 (redis-lpush redis "l0" "1st"))
  (test* "redis-lpush(multi values)" 4 (redis-lpush redis "l0" "2nd" "3rd" "4th"))
  (test* "redis-lpush(result)"
         #("4th" "3rd" "2nd" "1st")
         (redis-lrange redis "l0" 0 -1))

  (test-section "lpushx")
  (test* "redis-lpushx(key not exist)" 0 (redis-lpushx redis "l1" "1st"))
  (test* "redis-lpushx(key exist)" 5 (redis-lpushx redis "l0" "5th"))

  (test-section "lrange")
  (test* "lrange(positive,positive)" #("4th" "3rd" "2nd") (redis-lrange redis "l0" 1 3))
  (test* "lrange(positive,negative)" #("3rd" "2nd") (redis-lrange redis "l0" 2 -2))
  (test* "lrange(negative,negative)" #("4th" "3rd" "2nd") (redis-lrange redis "l0" -4 -2))

  (test-section "lrem")
  (for-each (lambda (count result-count msg result)
              (redis-del redis "l1")
              (redis-rpush redis
                           "l1" "hello" "world" "hello" "world" "hello" "world" "hello")
              (test* (format "redis-lrem(~a)" msg)
                     result-count (redis-lrem redis "l1" count "hello"))
              (test* "redis-lrem(result)" result (redis-lrange redis "l1" 0 -1)))
            '(2 -2 0)
            '(2 2 4)
            '("from head to tail" "from tail to head" "all")
            '(#("world" "world" "hello" "world" "hello")
              #("hello" "world" "hello" "world" "world")
              #("world" "world" "world")))

  (test-section "lset")
  (redis-rpush redis "l2" "one" "two" "three")
  (test* "redis-lset(positive)" 'OK (redis-lset redis "l2" 1 "four"))
  (test* "redis-lset(negative)" 'OK (redis-lset redis "l2" -3 "five"))
  (test* "redis-lset(result)" '#("five" "four" "three") (redis-lrange redis "l2" 0 -1))

  (test-section "ltrim")
  (redis-rpush redis "l3" "one" "two" "three" "four")
  (test* "redis-ltrim" 'OK (redis-ltrim redis "l3" 1 -2))
  (test* "redis-ltrim(result)" '#("two" "three") (redis-lrange redis "l3" 0 -1))

  (test-section "mget")
  (redis-flushall redis)
  (for-each (cut apply redis-set redis <>) '((1 "one") (2 "two") (3 "three") (4 "four")))
  (test* "redis-mget" '#("two" "four" #f) (redis-mget redis 2 4 6))

  (test-section "migrate")
  ;; TODO

  (test-section "monitor")
  ;; TODO

  (test-section "move")
  ;; TODO

  (test-section "mset")
  (redis-flushall redis)
  (test* "redis-mset" 'OK (redis-mset redis 1 "one" 2 "two" 3 "three" 4 "four"))
  (test* "redis-mset(result)" '#(#f "one" "two" "three" "four" #f)
         (redis-mget redis 0 1 2 3 4 5))

  (test-section "msetnx")
  (redis-flushall redis)
  (test* "redis-msetnx(success)" 1 (redis-msetnx redis 1 "one" 2 "two"))
  (test* "redis-msetnx(failure)" 0 (redis-msetnx redis 2 "弐" 3 "参"))
  (test* "redis-msetnx(result)" '#(#f "one" "two" #f)
         (redis-mget redis 0 1 2 3))

  (test-section "object (refcount|encoding|idletime)")
  (redis-flushall redis)
  (redis-rpush redis "l0" "a" "b" "c" "d")
  (test* "redis-object-refcount" 1 (redis-object-refcount redis "l0"))
  (test* "redis-object-encoding" "ziplist" (redis-object-encoding redis "l0"))
  (test* "redis-object-idletime" #t (integer? (redis-object-idletime redis "l0")))

  (test-section "persist")
  (redis-flushall redis)
  (redis-set redis 1 "one")
  (test* "redis-persist(not expired)" 0 (redis-persist redis 1))
  (redis-expire redis 1 10)
  (test* "redis-persist(before)" 10 (redis-ttl redis 1))
  (test* "redis-persist" 1 (redis-persist redis 1))
  (test* "redis-persist(after)" -1 (redis-ttl redis 1))

  (test-section "pexpire")
  ;; TODO

  (test-section "pexpireat")
  ;; TODO

  (test-section "ping")
  (test* "redis-ping" 'PONG (redis-ping redis))

  (test-section "psetex")
  ;; TODO

  (test-section "psubscribe")
  ;; TODO

  (test-section "pttl")
  ;; TODO

  (test-section "publish")
  ;; TODO

  (test-section "punsubscribe")
  ;; TODO

  (test-section "quit")
  (test* "redis-quit" 'OK (redis-quit redis))
  (redis-close redis #f))

(let1 redis (redis-open *bind-address* *bind-port*)

  (test-section "randomkey")
  (test* "redis-randomkey" #f (string-null? (redis-randomkey redis)))
  (redis-flushall redis)
  (test* "redis-randomkey(empty database)" #f (redis-randomkey redis))

  (test-section "rename")
  (redis-set redis 1 "一")
  (test* "redis-rename(before)" "一" (redis-get redis 1))
  (test* "redis-rename(before)" #f (redis-get redis "one"))
  (test* "redis-rename" 'OK (redis-rename redis 1 "one"))
  (test* "redis-rename(after)" #f (redis-get redis 1))
  (test* "redis-rename(after)" "一" (redis-get redis "one"))
  (test* "redis-rename(same key)" (test-error) (redis-rename redis "one" "one"))
  (test* "redis-rename(not exist)" (test-error) (redis-rename redis "ひとつ" "one"))
  (redis-rpush redis "ひとつ" "一つ" "一")
  (test* "redis-rename(overwrite)" 'OK (redis-rename redis "ひとつ" "one"))
  (test* "redis-rename(overwritten)" '#("一つ" "一") (redis-lrange redis "one" 0 -1))

  (test-section "renamenx")
  (redis-flushall redis)
  (redis-set redis 1 "一")
  (test* "redis-renamenx(before)" "一" (redis-get redis 1))
  (test* "redis-renamenx(before)" #f (redis-get redis "one"))
  (test* "redis-renamenx" 1 (redis-renamenx redis 1 "one"))
  (test* "redis-renamenx(after)" #f (redis-get redis 1))
  (test* "redis-renamenx(after)" "一" (redis-get redis "one"))
  (test* "redis-renamenx(same key)" (test-error) (redis-renamenx redis "one" "one"))
  (test* "redis-renamenx(not exist)" (test-error) (redis-renamenx redis "ひとつ" "one"))
  (redis-rpush redis "ひとつ" "一つ" "一")
  (test* "redis-renamenx(exist)" 0 (redis-renamenx redis "ひとつ" "one"))
  (test* "redis-renamenx(not overwritten)" "一" (redis-get redis "one"))

  (test-section "restore")
  ;; TODO

  (test-section "rpop")
  (redis-lpush redis "r0" 1 2 3)
  (test* "redis-rpop" "1" (redis-rpop redis "r0"))
  (test* "redis-rpop(result)" '#("3" "2") (redis-lrange redis "r0" 0 -1))

  (test-section "rpoplpush")
  (redis-rpush redis "r1" "one" "two" "three" "four")
  (test* "redis-rpoplpush(new key)" "four" (redis-rpoplpush redis "r1" "r2"))
  (test* "redis-rpoplpush(key exist)" "three" (redis-rpoplpush redis "r1" "r2"))
  (test* "redis-rpoplpush(result)" '#("one" "two") (redis-lrange redis "r1" 0 -1))
  (test* "redis-rpoplpush(result)" '#("three" "four") (redis-lrange redis "r2" 0 -1))

  (test-section "rpush")
  (redis-flushall redis)
  (test* "redis-rpush(new key, single value)" 1 (redis-rpush redis "r1" "one"))
  (test* "redis-rpush(key exist, multi-values)" 3 (redis-rpush redis "r1" "two" "three"))
  (test* "redis-rpush(result)" '#("one" "two" "three") (redis-lrange redis "r1" 0 -1))
  (test* "redis-rpush(new key, multi-value)" 2 (redis-rpush redis "r2" "one" "two"))
  (test* "redis-rpush(key exist, single value)" 3 (redis-rpush redis "r2" "three"))
  (test* "redis-rpush(result)" '#("one" "two" "three") (redis-lrange redis "r2" 0 -1))

  (test-section "rpushx")
  (redis-flushall redis)
  (test* "redis-rpushx(new key, failure)" 0 (redis-rpushx redis "r1" "one"))
  (redis-rpush redis "r1" "one")
  (test* "redis-rpushx(key exist, success)" 2 (redis-rpushx redis "r1" "two"))
  (test* "redis-rpushx(result)" '#("one" "two") (redis-lrange redis "r1" 0 -1))

  (test-section "sadd")
  (test* "redis-sadd(first value)" 1 (redis-sadd redis "s1" "one"))
  (test* "redis-sadd(second value)" 1 (redis-sadd redis "s1" "two"))
  (test* "redis-sadd(duplicated value)" 0 (redis-sadd redis "s1" "one"))
  (test* "redis-sadd(result)" (sort '#("one" "two")) (sort (redis-smembers redis "s1")))
  (test* "redis-sadd(multi values)" 2 (redis-sadd redis "s1" "three" "four" "one" "two"))
  (test* "redis-sadd(result)" (sort '#("one" "two" "three" "four")) (sort (redis-smembers redis "s1")))

  (test-section "save")
  ;; TODO

  (test-section "scard")
  (redis-flushall redis)
  (test* "redis-scard(new key)" 0 (redis-scard redis "s1"))
  (redis-sadd redis "s1" "one" "two" "three" "two")
  (test* "redis-scard(key exist)" 3 (redis-scard redis "s1"))

  (test-section "script exists")
  ;; TODO

  (test-section "script flush")
  ;; TODO

  (test-section "script kill")
  ;; TODO

  (test-section "script load")
  ;; TODO

  (test-section "sdiff")
  (redis-flushall redis)
  (redis-sadd redis 's1 'a 'b 'c 'd 'e 'f 'g)
  (redis-sadd redis 's2 'b 'd 'f)
  (redis-sadd redis 's3 'c 'e 'g)
  (test* "redis-sdiff" '#("a") (redis-sdiff redis 's1 's2 's3))

  (test-section "sdiffstore")
  (test* "redis-sdiffstore" 1 (redis-sdiffstore redis 's0 's1 's2 's3))
  (test* "redis-sdiffstore(result)" '#("a") (redis-smembers redis 's0))

  (test-section "select")
  ;; TODO

  (test-section "set")
  (test* "redis-set(new key)" 'OK (redis-set redis 1 "one"))
  (test* "redis-set(result)" "one" (redis-get redis 1))
  (test* "redis-set(overwrite)" 'OK (redis-set redis 1 "uno"))
  (test* "redis-set(result)" "uno" (redis-get redis 1))

  (test-section "setbit")
  (test* "redis-setbit(set)" 0 (redis-setbit redis 2 7 1))
  (test* "redis-setbit(unset)" 1 (redis-setbit redis 2 7 0))
  (test* "redis-setbit(result)" 0 (redis-getbit redis 2 7))

  (test-section "setex")
  (test* "redis-setex(2s)" 'OK (redis-setex redis 3 2 "three"))
  (test* "redis-setex(not yet)" "three" (redis-get redis 3))
  (test* "redis-setex(TTL)" 2 (redis-ttl redis 3))
  (sys-sleep 3)
  (test* "redis-setex(already)" #f (redis-get redis 3))

  (test-section "setnx")
  (test* "redis-setnx(new key)" 1 (redis-setnx redis 4 "four"))
  (test* "redis-setnx(key exist)" 0 (redis-setnx redis 4 "quattro"))
  (test* "redis-setnx(result)" "four" (redis-get redis 4))

  (test-section "setrange")
  (test* "setrange(new key)" 11 (redis-setrange redis 5 6 "Redis"))
  (test* "setrange(result)" "\0\0\0\0\0\0Redis" (redis-get redis 5))
  (test* "setrange(key exist)" 11 (redis-setrange redis 5 0 "Hello "))
  (test* "setrange(result)" "Hello Redis" (redis-get redis 5))

  (test-section "shutdown")
  ;; TODO

  (test-section "sinter")
  (redis-flushall redis)
  (redis-sadd redis 1 'a 'b 'c 'd)
  (redis-sadd redis 2 'a 'c)
  (redis-sadd redis 3 'b 'c 'd)
  (test* "redis-sinter" '#("c") (redis-sinter redis 1 2 3))

  (test-section "sinterstore")
  (redis-flushall redis)
  (redis-sadd redis 1 'a 'b 'c 'd)
  (redis-sadd redis 2 'a 'c)
  (redis-sadd redis 3 'b 'c 'd)
  (test* "redis-sinterstore" 1 (redis-sinterstore redis 0 1 2 3))
  (test* "redis-sinterstore(result)" '#("c") (redis-smembers redis 0))

  (test-section "sismember")
  (redis-flushall redis)
  (redis-sadd redis 1 'a 'b 'c 'd 'e 'f)
  (test* "redis-sismember" 1 (redis-sismember redis 1 'c))
  (test* "redis-sismember" 0 (redis-sismember redis 1 'g))

  (test-section "slaveof")
  ;; TODO

  (test-section "slowlog get")
  ;; TODO

  (test-section "slowlog len")
  ;; TODO

  (test-section "slowlog reset")
  (test* "redis-slowlog-reset" 'OK (redis-slowlog-reset redis))

  (test-section "smembers")
  (redis-sadd redis 1 'a 'b 'c 'd 'e 'f)
  (test* "redis-smembers" '#("a" "b" "c" "d" "e" "f") (sort! (redis-smembers redis 1)))

  (test-section "smove")
  (test* "redis-smove(new key)" 1 (redis-smove redis 1 2 "d"))
  (test* "redis-smove(result)" '#("a" "b" "c" "e" "f") (sort! (redis-smembers redis 1)))
  (test* "redis-smove(result)" '#("d")  (sort! (redis-smembers redis 2)))
  (test* "redis-smove(key exist)" 1 (redis-smove redis 1 2 "b"))
  (test* "redis-smove(result)" '#("a" "c" "e" "f") (sort! (redis-smembers redis 1)))
  (test* "redis-smove(result)" '#("b" "d")  (sort! (redis-smembers redis 2)))

  (test-section "sort")
  ;; TODO

  (test-section "spop")
  ;; TODO

  (test-section "srandmember")
  ;; TODO

  (test-section "srem")
  (test* "redis-srem(exist)" 1 (redis-srem redis 1 "a"))
  (test* "redis-srem(not exist)" 0 (redis-srem redis 1 "g"))
  (test* "redis-srem(result)" '#("c" "e" "f") (sort! (redis-smembers redis 1)))

  (test-section "strlen")
  (redis-flushall redis)
  (redis-set redis 1 "Hello\0Redis!!\0")
  (test* "redis-strlen(exist)" 14 (redis-strlen redis 1))
  (test* "redis-strlen(not exist)" 0 (redis-strlen redis 2))

  (test-section "subscribe")
  ;; TODO

  (test-section "sunion")
  (redis-flushall redis)
  (redis-sadd redis 1 'a 'b 'c 'd 'e 'f)
  (redis-sadd redis 2 'd 'e 'f 'g 'h 'i 'j)
  (test* "redis-sunion" '#("a" "b" "c" "d" "e" "f" "g" "h" "i" "j")
         (sort! (redis-sunion redis 1 2 3)))

  (test-section "sunionstore")
  (redis-flushall redis)
  (redis-sadd redis 1 'a 'b 'c 'd 'e 'f)
  (redis-sadd redis 2 'd 'e 'f 'g 'h 'i 'j)
  (test* "redis-sunionstore" 10 (redis-sunionstore redis 0 1 2 3))
  (test* "redis-sunionstore(result)" '#("a" "b" "c" "d" "e" "f" "g" "h" "i" "j")
         (sort! (redis-smembers redis 0)))

  (test-section "sync")
  ;; TODO

  (test-section "time")
  ;; TODO

  (test-section "ttl")
  ;; TODO

  (test-section "type")
  (redis-flushall redis)
  (redis-set redis 1 "String")
  (redis-rpush redis 2 "L" "i" "s" "t")
  (redis-sadd redis 3 'a 'b 'c)
  (test* "redis-type" 'string (redis-type redis 1))
  (test* "redis-type" 'list (redis-type redis 2))
  (test* "redis-type" 'set (redis-type redis 3))

  (test-section "unsubscribe")
  ;; TODO

  (test-section "zadd")
  ;; TODO

  (test-section "zcard")
  ;; TODO

  (test-section "zcount")
  ;; TODO

  (test-section "zincrby")
  ;; TODO

  (test-section "zinterstore")
  ;; TODO

  (test-section "zrange")
  ;; TODO

  (test-section "zrangebyscore")
  ;; TODO

  (test-section "zrank")
  ;; TODO

  (test-section "zrem")
  ;; TODO

  (test-section "zremrangebyrank")
  ;; TODO

  (test-section "zremrangebyscore")
  ;; TODO

  (test-section "zrevrange")
  ;; TODO

  (test-section "zrevrangebyscore")
  ;; TODO

  (test-section "zrevrank")
  ;; TODO

  (test-section "zscore")
  ;; TODO

  (test-section "zunionstore")
  ;; TODO

  (redis-flushall redis)

  (test-section "Transaction(each command)")
  (test* "multi(empty transaction)" 'OK (redis-multi redis))
  (test* "exec(empty transaction)" '#() (redis-exec redis))
  (test* "exec(orphan)" (test-error) (redis-exec redis))
  (test* "multi(empty transaction)" 'OK (redis-multi redis))
  (test* "discard(empty transaction)" 'OK (redis-discard redis))
  (test* "discard(orphan)" (test-error) (redis-discard redis))
  (test* "watch" 'OK (redis-watch redis "1" "2" "3" "4"))
  (test* "unwatch" 'OK (redis-unwatch redis))

  (redis-flushall redis)

  (test-section "Transactional block")

  (redis-close redis))

(redis-server-stop *redis-server*)

(test-end :exit-on-failure #t)
