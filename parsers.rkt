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

(require srfi/1)

; == Utility functions ==

(define (empty-str? str) (= 0 (string-length str)))
(define (str-head str) (string-ref str 0))
(define (str-tail str) (substring str 1))
(define (str-cons chr str) (string-append (string chr) str))

(define (assert e) (if e (void) (raise "assertion failed")))

; == Base Combinators ==

; A parser is a function that takes a string and returns either:
;
;  (Failure)
;  (Success result rest)
;
; In (Success result rest), 'rest' is the part of the input string that hasn't
; been consumed yet. The type of 'result' depends on the parser. For instance,
; a parser for parsing integers will return (Succes 42 "aa") when applied to
; "42aa", but a parser for parsing many digits will return
; (Success (list 4 2) "aa") when applied to "42aa".

(struct Failure () #:transparent)
(struct Success (result rest) #:transparent)

; The simplest parser is the one that always fails.

(define (p-fail input) (Failure))

(assert (equal? (p-fail "abc") (Failure)))

; Another simple parser is the one that ignores its input and always returns
; the same value.

(define (p-success value)
  (λ (input) (Success value input)))

(assert (equal?
  ((p-success 42) "abc")
  (Success 42 "abc")))

; We define a parser that accepts any character sastifying a predicate
; and returns the character.

(define (p-satisfies predicate)
  (λ (input)
    (if (empty-str? input)
      (Failure)
      (let ((head (str-head input))
            (tail (str-tail input)))
        (if (predicate head)
          (Success head tail)
          (Failure))))))

; We can specialize it to accept a certain character only.

(define (p-char chr)
  (p-satisfies (λ (input-chr) (char=? input-chr chr))))

(assert (equal?
  ((p-char #\a) "abc")
  (Success #\a "bc")))

(assert (equal?
  ((p-char #\b) "abc")
  (Failure)))

; Or to accept any character.

(define p-any-char
  (p-satisfies (λ (_) #t)))

(assert (equal?
  (p-any-char "abc")
  (Success #\a "bc")))

(assert (equal?
  (p-any-char "bcd")
  (Success #\b "cd")))

; We define the alternative combinator: '(p-or parser1 parser2)' returns the
; result of 'parser1' if it succeeds, of 'parser2' otherwise.

(define (p-or parser1 parser2)
  (λ (input)
    (let ((result (parser1 input)))
      (if (Success? result)
        result
        (parser2 input)))))

(assert (equal?
  ((p-or (p-char #\a) (p-char #\b)) "abc")
  (Success #\a "bc")))

(assert (equal?
  ((p-or (p-char #\a) (p-char #\b)) "bcd")
  (Success #\b "cd")))

(assert (equal?
  ((p-or (p-char #\a) (p-char #\b)) "cde")
  (Failure)))

; Parsers can be chained: '(p-seq parser continuation)' executes 'parser' and
; feeds its result into 'continuation'. 'continuation' is expected to return a
; new parser that is in turn executed.

(define (p-seq parser continuation)
  (λ (input)
    (match (parser input)
      ((struct Success (value rest)) ((continuation value) rest))
      ((Failure) (Failure)))))

(assert (equal?
  ((p-seq p-fail (λ (_) (p-success 42))) "abc")
  (Failure)))

(assert (equal?
  ((p-seq p-any-char (λ (chr) (p-char chr))) "aab")
  (Success #\a "b")))

(assert (equal?
  ((p-seq p-any-char (λ (chr) (p-char chr))) "abb")
  (Failure)))

; Note that the grammar above (p-seq p-any-char (λ (chr) (p-char chr))) is
; context-sensitive: the result of the first parser is used for defining the
; second parser. p-seq allows for these sort of things. One could define weaker
; notions of sequencing which wouldn't allow for this, but would in exchange
; make it possible to build an AST of the grammar that is being defined and
; then pre-process it. This is a tradeoff between expressivity and
; performance.

; We can chain p-char parsers to build a parser that accepts a particular string
; and then returns it.

(define (p-str str)
  (if (empty-str? str)
    (p-success "")
    (let ((head (str-head str))
          (tail (str-tail str)))
      (p-seq (p-char head) (λ (_)
        (p-seq (p-str tail) (λ (_)
          (p-success (str-cons head tail)))))))))

(assert (equal?
  ((p-str "abc") "abcde")
  (Success "abc" "de")))

(assert (equal?
  ((p-str "abc") "bcde")
  (Failure)))

; We can now define a combinator that executes the same parser 0 or many times.
; It accumulates a list of the results.

(define (p-many parser)
  (p-or
    (p-seq parser (λ (val)
      (p-seq (p-many parser) (λ (vals)
        (p-success (cons val vals))))))
    (p-success '())))

(assert (equal?
  ((p-many (p-char #\a)) "aaabc")
  (Success (list #\a #\a #\a) "bc")))

(assert (equal?
  ((p-many (p-char #\a)) "bc")
  (Success '() "bc")))

; And its 1 or many times variant.

(define (p-many-1 parser)
  (p-seq parser (λ (val)
    (p-seq (p-many parser) (λ (vals)
      (p-success (cons val vals)))))))

(assert (equal?
  ((p-many-1 (p-char #\a)) "aaabc")
  (Success (list #\a #\a #\a) "bc")))

(assert (equal?
  ((p-many-1 (p-char #\a)) "bc")
  (Failure)))

; We have all the main combinators now. We can define some custom ones, like
; one that parses strings of the form "(a,b)" and returns the pair (cons a b).

(define (p-pair parser-left parser-right)
  (p-seq (p-char #\() (λ (_)
    (p-seq parser-left (λ (val-left)
      (p-seq (p-char #\,) (λ (_)
        (p-seq parser-right (λ (val-right)
          (p-seq (p-char #\)) (λ (_)
            (p-success (cons val-left val-right)))))))))))))

(assert (equal?
  ((p-pair p-any-char (p-str "foo")) "(a,foo)bcd")
  (Success (cons #\a "foo") "bcd")))

(assert (equal?
  ((p-pair p-any-char p-any-char) "a,b)de")
  (Failure)))

; Chaining parsers with λs is cumbersome, so we define a macro.

(define-syntax p-let
  (syntax-rules ()
    ((p-let () result) (p-success result))
    ((p-let ((x1 parser1) (x2 parser2) ...) result)
     (p-seq parser1 (λ (x1) (p-let ((x2 parser2) ...) result))))))

; Re-defining p-pair using the new notation is arguably more readable.

(define (p-pair2 parser-left parser-right)
  (p-let ((_         (p-char #\())
          (val-left  parser-left)
          (_         (p-char #\,))
          (val-right parser-right)
          (_         (p-char #\))))
    (cons val-left val-right)))

(assert (equal?
  ((p-pair2 p-any-char (p-str "foo")) "(a,foo)bcd")
  (Success (cons #\a "foo") "bcd")))

(assert (equal?
  ((p-pair2 p-any-char p-any-char) "a,b)de")
  (Failure)))

; == Some Useful Combinators ==

; Applies a function to the result of a parser.

(define (p-map fun parser)
  (p-seq parser (λ (val) (p-success (fun val)))))

(assert (equal?
  ((p-map string-upcase (p-str "abc")) "abcde")
  (Success "ABC" "de")))

(assert (equal?
  ((p-map string-upcase p-fail) "abcde")
  (Failure)))

; Chains two parsers, discards the result of the first one.

(define (p-then parser1 parser2)
  (p-seq parser1 (λ (_) parser2)))

(assert (equal?
  ((p-then (p-str "ab") (p-str "cd")) "abcde")
  (Success "cd" "e")))

(assert (equal?
  ((p-then p-fail (p-str "cd")) "abcde")
  (Failure)))

(assert (equal?
  ((p-then (p-str "ab") p-fail) "abcde")
  (Failure)))

; Parses one or more occurences of 'parser', separated by 'sep-parser'.

(define (p-sep-by-1 parser sep-parser)
  (p-let ((val  parser)
          (vals (p-many (p-then sep-parser parser))))
    (cons val vals)))

(assert (equal?
  ((p-sep-by-1 (p-str "ab") (p-str ",")) "ab,ab,abcd")
  (Success (list "ab" "ab" "ab") "cd")))

(assert (equal?
  ((p-sep-by-1 (p-str "ab") (p-str ",")) "cd")
  (Failure)))

; Same for zero or more occurences.

(define (p-sep-by parser sep-parser)
  (p-or (p-sep-by-1 parser sep-parser)
        (p-success '())))

(assert (equal?
  ((p-sep-by (p-str "ab") (p-str ",")) "ab,ab,abcd")
  (Success (list "ab" "ab" "ab") "cd")))

(assert (equal?
  ((p-sep-by (p-str "ab") (p-str ",")) "cd")
  (Success '() "cd")))

; Accepts 'parser' followed by many spaces and returns its result.

(define (p-token parser)
  (p-let ((val parser)
          (_   (p-many (p-char #\ ))))
    val))

(assert (equal?
  ((p-token (p-str "ab")) "ab  cde")
  (Success "ab" "cde")))

(assert (equal?
  ((p-token (p-str "ab")) "abcde")
  (Success "ab" "cde")))

(assert (equal?
  ((p-token (p-str "ab")) "bb cde")
  (Failure)))

; Eta-expands a parser, making it possible to write mutually-recursive
; parsers without looping when the parser gets evaluated.

(define (p-rec parser-maker)
  (λ (input)
    ((parser-maker) input)))

(assert (equal?
  ((p-rec (λ () (p-str "ab"))) "abcd")
  (Success "ab" "cd")))

; == Real Life Example: Arithmetic ==

; Data structures and helper functions.

(struct Plus (left right) #:transparent)
(struct Mult (left right) #:transparent)
(struct Lit (value) #:transparent)

(define (plus args)
  (reduce-right Plus (Lit 0) args))

(define (mult args)
  (reduce-right Mult (Lit 1) args))

; Turns a char into a token.

(define (char->token char)
  (p-token (p-char char)))

; Parses 'parser' between the '(' and ')' tokens.

(define (p-parens parser)
  (p-let ((_   (char->token #\())
          (val parser)
          (_   (char->token #\))))
    val))

; Converts a list of digits into a number.

(define (digits->number digits)
  (string->number (apply string digits)))

; Parses a digit.

(define p-digit
  (p-satisfies (λ (chr)
    (and (char>=? chr #\0) (char<=? chr #\9)))))

; Parses a number.

(define p-number
  (p-map digits->number (p-many-1 p-digit)))

; Parses a number literal.

(define p-lit
  (p-map Lit (p-token p-number)))

; Parses a sum of products.

(define (p-sum)
  (p-map plus (p-sep-by-1 (p-rec p-product) (char->token #\+))))

; Parses a product of atoms.

(define (p-product)
  (p-map mult (p-sep-by-1 (p-rec p-atom) (char->token #\*))))

; Parses either a literal or a sum inside parenthesis.

(define (p-atom)
  (p-or p-lit (p-parens (p-rec p-sum))))

; Demo

(print ((p-sum) "10 + 2 * (3+2) * 1"))
(newline (current-output-port))
