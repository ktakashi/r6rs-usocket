;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; usocket/srfi.sagittarius.sls - Portable layer of SRFI support
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
(library (usocket srfi)
    (export socket?
	    ;; the rest comes later
	    make-client-socket
	    make-server-socket

	    socket-input-port
	    socket-output-port

	    socket-accept ;; for server
	    socket-send
	    socket-recv
	    socket-close
	    socket-shutdown

	    call-with-socket

	    address-family address-info 
	    socket-domain ip-protocol
	    message-type shutdown-method

	    socket-merge-flags
	    socket-purge-flags
	    
	    *af-unspec* *af-inet* *af-inet6*
	    *sock-stream* *sock-dgram*
	    *ai-canonname* *ai-numerichost* *ai-v4mapped*
	    *ai-all* *ai-addrconfig*
	    *ipproto-ip* *ipproto-tcp* *ipproto-udp*
	    *msg-peek* *msg-oob* *msg-waitall*
	    *shut-rd* *shut-wr* *shut-rdwr*

	    socket-error? socket-error-socket)
    (import (srfi :106 socket)
	    (only (sagittarius socket) socket-error? socket-error-socket)))
