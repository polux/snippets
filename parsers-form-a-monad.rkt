; Copyright 2015 Google Inc. All Rights Reserved.
;
; Licensed under the Apache License, Version 2.0 (the "License");
; you may not use this file except in compliance with the License.
; You may obtain a copy of the License at
;
;     http://www.apache.org/licenses/LICENSE-2.0
;
; Unless required by applicable law or agreed to in writing, software
; distributed under the License is distributed on an "AS IS" BASIS,
; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
; See the License for the specific language governing permissions and
; limitations under the License.

#lang racket

(require "parsers.rkt")

; In this module we remark that parsers and some of their combinators satisfy
; some algebraic laws.

; Let us start by defining a variant of p-seq, p-compose, which is more
; amendable to algebraic reasoning. While p-seq combines a parser and a
; continuation, p-compose combines two continuations, or two "parser factories"
; if you prefer. That is, p-compose has the type:
;
;   (a -> Parser b) -> (b -> Parser c) -> (a -> Parser c)
;

(define (p-compose fun1 fun2)
  (λ (val1)
    (p-seq (fun1 val1) (λ (val2)
      (fun2 val2)))))

; In other words, writing ((p-compose f1 f2) a) is equivalent to writing
; (p-seq (f1 a) f2). We're just shuffling things around.


; Now let's start by remarking the following algebraic property of p-success
; and p-compose:
;
;   forall fun, (p-compose p-success fun) = fun
;
; In other words, p-success is the left-neutral of p-compose (similar to the
; way 0 is the left-neutral of +). Let's test it on a few instances.

(define (property1 fun val input)
  (assert (equal?
    (((p-compose p-success fun) val) input)
    ((fun val) input))))

(property1 p-char #\a "abc")
(property1 p-char #\a "bcd")
(property1 p-char #\a "")

; The second remarkable property is that p-success is also the right-neutral of
; p-compose:
;
;   forall fun, (p-compose fun p-success) = fun
;
; Let's test it on a few instances.

(define (property2 fun val input)
  (assert (equal?
    (((p-compose fun p-success) val) input)
    ((fun val) input))))

(property2 p-char #\a "abc")
(property2 p-char #\a "bcd")
(property2 p-char #\a "")

; The third remarkable propery is the following:
;
;   forall fun1 fun2 fun3,
;     (p-compose (p-compose fun1 fun2) fun3) =
;     ((p-compose fun1 (p-compose fun2 fun3)))
;
; In other words, p-compose is associative. You can also read it as "parsing (a
; then b) then c is the same as parsing a then (b then c)".
; Let's test it on a few instances.

(define (property3 fun1 fun2 fun3 val input)
  (assert (equal?
    (((p-compose (p-compose fun1 fun2) fun3) val) input)
    (((p-compose fun1 (p-compose fun2 fun3)) val) input))))

(define (p-add-to str)
  (p-let ((chr p-any-char))
    (str-cons chr str)))

(property3 p-add-to p-add-to p-add-to "a" "bcde")
(property3 p-add-to p-add-to p-add-to "" "bcde")
(property3 p-add-to p-add-to p-add-to "a" "")

; Because these laws hold, we say that parsers form a monad for p-success and
; p-compose (or for p-success and p-seq depending on which definition of monad
; you pick, they are equivalent).

; The fact that they form a monad buys us the following:
;   - We can reuse functions that are defined "for all monads" with parsers
;   - We can use theorems about monads when reasoning about parsers

