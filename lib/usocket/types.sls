;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; usocket/types.sls - Socket records
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
(library (usocket types)
    (export socket?
	    socket-close!
	    socket-shutdown!

	    *usocket:shutdown-read*
	    *usocket:shutdown-write*
	    *usocket:shutdown-read&write*

	    client-socket?
	    client-socket-input-port
	    client-socket-output-port

	    server-socket?
	    server-socket-accept!
	    ;; for developer
	    make-client-socket
	    make-server-socket
	    socket-raw-socket)
    (import (rnrs))


(define-record-type socket
  (fields raw-socket shutdowner closer))
(define-record-type client-socket
  (parent socket)
  (fields input-port output-port))
(define-record-type server-socket
  (parent socket)
  (fields acceptor))

(define *usocket:shutdown-read* 'read)
(define *usocket:shutdown-write* 'write)
(define *usocket:shutdown-read&write* 'read&write)

(define (socket-close! socket) ((socket-closer socket)))
(define (socket-shutdown! socket how) ((socket-shutdowner socket) how))

(define (server-socket-accept! socket) ((server-socket-acceptor socket)))

)
