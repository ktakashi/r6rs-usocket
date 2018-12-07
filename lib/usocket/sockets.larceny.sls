;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; usocket/sockets.larceny.sls - Socket protocols (Larceny)
;;;
;;;   Copyright (c) 2018  Takashi Kato  <ktakashi@ymail.com>
;;;
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;


;; There are variety of restrictions on Larceny implementation.
;; - It can't create UDP socket
;; - Socket can't be shutdown
;; - No explicit socket close for server socket

#!r6rs
(library (usocket sockets)
    (export make-tcp-client-socket
	    make-tcp-server-socket
	    make-udp-client-socket
	    make-udp-server-socket)
    (import (rnrs)
	    (prefix (usocket types) usocket:)
	    (primitives r5rs:require get-service-by-name make-client-socket
			make-server-socket server-socket-accept
			socket-input-port socket-output-port))

(define (make-tcp-client-socket host service)
  (let ((s (make-client-socket host service)))
    (%make-socket s)))

(define (make-tcp-server-socket service)
  (let ((s (make-server-socket service)))
    (%make-server-socket s)))

(define (not-supported who)
  (raise (condition
	  (make-implementation-restriction-violation)
	  (make-who-condition who)
	  (make-message-condition "Not supported"))))
(define (make-udp-client-socket host service)
  (not-supported 'make-udp-client-socket))

(define (make-udp-server-socket service)
  (not-supported 'make-udp-server-socket))

(define (make-shutdowner s)
  ;; Larceny doesn't support shutdown... so do nothing
  (lambda (how) #f))
(define (make-closer in out)
  (lambda () (close-input-port in) (close-output-port out)))
(define (%make-socket s)
  (let ((in (socket-input-port s))
	(out (socket-output-port s)))
    (usocket:make-client-socket s (make-shutdowner s) (make-closer in out)
				in out)))

(define (%make-server-socket s)
  (usocket:make-server-socket s
   (make-shutdowner s)
   (lambda () #f) ;; we can't do anything...
   (lambda ()
     (let-values (((cs addr) (server-socket-accept s)))
       (%make-socket cs)))))

(r5rs:require 'socket)
)
