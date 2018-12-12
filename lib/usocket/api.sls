;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; usocket/api.sls - Socket protocols
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

#!r6rs
(library (usocket api)
    (export make-tcp-client-usocket
	    make-tcp-server-usocket
	    make-udp-client-usocket
	    make-udp-server-usocket)
    (import (rnrs)
	    (usocket types)
	    (prefix (usocket srfi) srfi:))

(define (make-tcp-client-usocket host service)
  (let ((s (srfi:make-client-socket host service)))
    (%make-socket s)))

(define (make-tcp-server-usocket service)
  (let ((s (srfi:make-server-socket service)))
    (%make-server-socket s)))

(define (make-udp-client-usocket host service)
  (let ((s (srfi:make-client-socket host service
				    srfi:*af-inet* srfi:*sock-dgram*)))
    (%make-socket s)))

(define (make-udp-server-usocket service)
  (let ((s (srfi:make-server-socket service srfi:*af-inet* srfi:*sock-dgram*)))
    (%make-server-socket s)))

(define (make-shutdowner s)
  (lambda (how)
    (case how
      ((read) (srfi:socket-shutdown s srfi:*shut-rd*))
      ((write) (srfi:socket-shutdown s srfi:*shut-wr*))
      ((read&write) (srfi:socket-shutdown s srfi:*shut-rdwr*))
      (else (assertion-violation 'socket-shutdown!
				 "Unknown shutdown method" how)))))
(define (make-closer s) (lambda () (srfi:socket-close s)))
(define (%make-socket s)
  (make-client-usocket s
	       (make-shutdowner s)
	       (make-closer s)
	       (srfi:socket-input-port s)
	       (srfi:socket-output-port s)))

(define (%make-server-socket s)
  (make-server-usocket s
		      (make-shutdowner s)
		      (make-closer s)
		      (lambda () (%make-socket (srfi:socket-accept s)))))
)
