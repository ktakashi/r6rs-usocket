;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; usocket/srfi/pffi.sls - PFFI SRFI-106 implementation
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

;; SRFI-106 implementation of PFFI
(library (usocket srfi pffi)
    (export socket?
	    ;; the rest comes later
	    make-client-socket

	    socket-send
	    socket-recv
	    socket-close
	    socket-shutdown
	    (rename (usocket:AF_UNSPEC *af-unspec*)
		    (usocket:AF_INET *af-inet*)
		    (usocket:AF_INET6 *af-inet6*)
		    (usocket:SOCK_STREAM *sock-stream*)
		    (usocket:SOCK_DGRAM *sock-dgram*)
		    (usocket:AI_CANONNAME *ai-canonname*)
		    (usocket:AI_NUMERICHOST *ai-numerichost*)
		    (usocket:AI_V4MAPPED *ai-v4mapped*)
		    (usocket:AI_ALL *ai-all*)
		    (usocket:AI_ADDRCONFIG *ai-addrconfig*)
		    (usocket:IPPROTO_IP *ipproto-ip*)
		    (usocket:IPPROTO_TCP *ipproto-tcp*)
		    (usocket:IPPROTO_UDP *ipproto-udp*)
		    (usocket:MSG_PEEK *msg-peek*)
		    (usocket:MSG_OOB *msg-oob*)
		    (usocket:MSG_WAITALL *msg-waitall*)
		    (usocket:SHUT_RD *shut-rd*)
		    (usocket:SHUT_WR *shut-wr*)
		    (usocket:SHUT_RDWR *shut-rdwr*))
	    )
    (import (rnrs)
	    (pffi)
	    (psystem libc)
	    (psystem os)
	    (usocket consts))

;;; TODO Should this be somewhere else?
(define-record-type socket
  (fields socket
	  type
	  host
	  service
	  ;; input-port ;; later
	  ;; output-port ;; later
	  ))

(define-foreign-struct addrinfo
  (fields (int ai-flags)
	  (int ai-family)
	  (int ai-socktype)
	  (int ai-protocol)
	  (int ai-addrlen)
	  ;; the order of these 2 is os dependent (afaik)
	  ;; i.e. On Linux, ai_addr is before ai_canonname
	  ;;      On *BSD (incl. OSX) ai_addr is after ai_canonname
	  ;; who decide this fxxking incompatibility?
	  (pointer ai-maybe-canonname)
	  (pointer ai-maybe-addr)      
	  (pointer ai-next)))    ;; addrinfo

;; Fxxk Fxxk Fxxk!!!
(define (addrinfo-ai-addr addrinfo)
  (if (eq? *psystem:os-name* 'Linux)
      (addrinfo-ai-maybe-canonname addrinfo)
      (addrinfo-ai-maybe-addr addrinfo)))

;; int
;; getaddrinfo(const char *, const char *, const struct addrinfo *,
;;             struct addrinfo **)
(define c:getaddrinfo
  (foreign-procedure *psystem:libc*
		     int getaddrinfo (pointer pointer pointer pointer)))
(define c:freeaddrinfo
  (foreign-procedure *psystem:libc* void freeaddrinfo (pointer)))

(define c:socket
  (foreign-procedure *psystem:libc* int socket (int int int)))
;; int connect(int sockfd, const struct sockaddr *addr, socklen_t addrlen);
(define c:connect
  (foreign-procedure *psystem:libc* int connect (int pointer int)))

(define c:close
  (foreign-procedure *psystem:libc* int close (int)))
(define c:shutdown
  (foreign-procedure *psystem:libc* int shutdown (int int)))

;; ssize_t send(int sockfd, const void *buf, size_t len, int flags);
(define c:send
  (foreign-procedure *psystem:libc* int send (int pointer unsigned-int int)))
;; ssize_t recv(int sockfd, void *buf, size_t len, int flags);
(define c:recv
  (foreign-procedure *psystem:libc* int recv (int pointer unsigned-int int)))

(define null-pointer (integer->pointer 0))
(define (null-pointer? p) (zero? (pointer->integer p)))

(define-syntax get-optional
  (syntax-rules ()
    ((_ opt default kdr kdr* ...)
     (let ((l opt))
       (or (and (not (null? l)) (get-optional (kdr l) default kdr* ...))
	   default)))
    ((_ opt default)
     (let ((l opt))
       (or (and (not (null? l)) (car l)) default)))))

;; SRFI-106
(define *default-ai-flags*
  (bitwise-ior usocket:AI_V4MAPPED usocket:AI_ADDRCONFIG))
(define (make-client-socket host service . opts)
  (define family (get-optional opts usocket:AF_UNSPEC))
  (define socktype (get-optional opts usocket:SOCK_STREAM cdr))
  (define flags (get-optional opts *default-ai-flags* cdr cdr))
  (define protocol (get-optional opts usocket:IPPROTO_IP cdr cdr cdr))

  (define (free box)
    (c:freeaddrinfo (pointer-ref-c-pointer box 0))
    (psystem:free box))
  (define (free&error box)
    (free box)
    (error 'make-client-socket "Failed to create a socket" host service))
  (define (free&return box sock addr len)
    (let ((r (c:connect sock addr len)))
      (free box)
      (unless (zero? r)
	(error 'make-client-socket "Failed to connect" host service))
      (make-socket sock 'client host service)))
  
  (let ((box (psystem:malloc size-of-pointer))
	(hint (make-addrinfo 0 0 0 0 0 null-pointer null-pointer null-pointer)))
    (addrinfo-ai-flags-set! hint flags)
    (addrinfo-ai-family-set! hint family)
    (addrinfo-ai-socktype-set! hint socktype)
    (addrinfo-ai-protocol-set! hint protocol)
    (let ((r (c:getaddrinfo host service hint box)))
      (unless (zero? r)
	(error 'make-client-socket "Failed to call getaddrinfo" host service))
      (let loop ((result (pointer-ref-c-pointer box 0)))
	(if (null-pointer? result)
	    (free&error box)
	    (let* ((ai (pointer->bytevector result size-of-addrinfo))
		   (sock (c:socket (addrinfo-ai-family ai)
				 (addrinfo-ai-socktype ai)
				 (addrinfo-ai-protocol ai))))
	      (if (negative? sock)
		  (loop (addrinfo-ai-next ai))
		  (free&return box sock (addrinfo-ai-addr ai)
			       (addrinfo-ai-addrlen ai)))))))))

(define (socket-close sock) (c:close (socket-socket sock)))
(define (socket-shutdown sock how) (c:shutdown (socket-socket sock) how))

(define (socket-send socket bv . opt)
  (define flags (get-optional opt 0))
  (unless (bytevector? bv)
    (assertion-violation 'socket-send "Bytevector required" bv))
  (c:send (socket-socket socket)
	bv (bytevector-length bv)
	(bitwise-ior flags usocket:MSG_NOSIGNAL)))

(define (socket-recv socket size . opt)
  (define flags (bitwise-ior (get-optional opt 0) usocket:MSG_NOSIGNAL))
  (define buf (make-bytevector size))
  (define fd (socket-socket socket))
  ;; supporting implementations have proper bytevector->pointer
  ;; means it can share the buffer
  (define p (bytevector->pointer buf))
  (let ((c (c:recv fd p size flags)))
    (cond ((= c size) buf)
	  ((< c 0) (error 'socket-recv "Failed to receive"))
	  ((< c size)
	   (let ((r (make-bytevector c)))
	     (do ((i 0 (+ i 1)))
		 ((= i c) r)
	       (bytevector-u8-set! r i (bytevector-u8-ref buf i))))))))

)