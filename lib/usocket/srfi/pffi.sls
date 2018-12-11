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
#!r6rs
(library (usocket srfi pffi)
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
	    
	    (rename (bitwise-ior socket-merge-flags))
	    socket-purge-flags
	    
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
	  input-port
	  output-port
	  ;; pollfds we have 2 fds in case of multi thread env
	  read-poll
	  write-poll)
  (protocol (lambda (p)
	      (lambda (socket type host service)
		(let ((read-poll (make-pollfd socket usocket:POLLIN 0)))
		  (p socket type host service
		     (make-socket-input-port socket read-poll)
		     (make-socket-output-port socket)
		     read-poll
		     (make-pollfd socket usocket:POLLOUT 0)))))))

(define buffer-size 1024)
(define (make-socket-input-port fd pollfd)
  ;; internal buffer
  (define buffer (make-bytevector buffer-size))
  (define bufp (bytevector->pointer buffer))
  (define check-needed? #f)
  (define (read! bv start count)
    ;; if the last recv is less than expected or exactly the same
    ;; as buffer-size, then we need to check if there's next data
    ;; available or not.
    (if (and check-needed? (not (pollfd-readable? pollfd 0)))
	0
	(let* ((size (min count buffer-size))
	       (r (c:recv fd bufp size usocket:MSG_NOSIGNAL)))
	  (when (negative? r) (error 'socket-recv "Failed to receive"))
	  (set! check-needed?
		(or (not (= r size)) (and (= count buffer-size) (= r size))))
	  ;; let the custom port handle the remaining count
	  (do ((i 0 (+ i 1)))
	      ((= i r) r)
	    (bytevector-u8-set! bv (+ i start) (bytevector-u8-ref buffer i))))))
  (make-custom-binary-input-port "socket-input-port" read! #f #f #f))

(define (make-socket-output-port fd)
  ;; internal buffer
  (define buffer (make-bytevector buffer-size))
  (define bufp (bytevector->pointer buffer))
  (define (write! bv start count)
    (let loop ((rest count) (written 0))
      (if (= written count)
	  count
	  (let ((size (min rest buffer-size)))
	    (do ((i 0 (+ i 1)))
		((= i size))
	      (bytevector-u8-set! buffer i (bytevector-u8-ref bv (+ i start))))
	    (let ((r (c:send fd bufp size usocket:MSG_NOSIGNAL)))
	      (when (negative? r) (error 'socket-send "Failed to send"))
	      (loop (- rest r) (+ written r)))))))
  (make-custom-binary-output-port "socket-output-port" write! #f #f #F))

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

;; luckily pollfd has the same definition on both Linux and OSX :)
(define-foreign-struct pollfd
  (fields (int fd)
	  (short events)
	  (short revents)))

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

;; int bind(int sockfd, const struct sockaddr *addr, socklen_t addrlen);
(define c:bind
  (foreign-procedure *psystem:libc* int bind (int pointer int)))
;; int listen(int sockfd, int backlog);
(define c:listen
  (foreign-procedure *psystem:libc* int listen (int int)))
;; int accept(int sockfd, struct sockaddr *addr, socklen_t *addrlen);
(define c:accept
  (foreign-procedure *psystem:libc* int accept (int pointer pointer)))

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

(define (getaddrinfo host service flags family socktype protocol)
  (let ((box (psystem:malloc size-of-pointer))
	(hint (make-addrinfo 0 0 0 0 0 null-pointer null-pointer null-pointer)))
    (addrinfo-ai-flags-set! hint flags)
    (addrinfo-ai-family-set! hint family)
    (addrinfo-ai-socktype-set! hint socktype)
    (addrinfo-ai-protocol-set! hint protocol)
    (let ((r (c:getaddrinfo host service hint box)))
      (unless (zero? r)
	(psystem:free box)
	(error 'getaddrinfo "Failed to call getaddrinfo" host service))
      box)))
(define (freeaddrinfo box)
  (c:freeaddrinfo (pointer-ref-c-pointer box 0))
  (psystem:free box))

(define (get-socket addrinfo c:cont)
  (let loop ((result addrinfo))
    (and (not (null-pointer? result))
	 (let* ((ai (pointer->bytevector result size-of-addrinfo))
		(sock (c:socket (addrinfo-ai-family ai)
				(addrinfo-ai-socktype ai)
				(addrinfo-ai-protocol ai))))
	   (cond ((< sock 0) (loop (addrinfo-ai-next ai)))
		 ((zero? (c:cont sock (addrinfo-ai-addr ai)
				 (addrinfo-ai-addrlen ai))) sock)
		 (else (c:close sock) (loop (addrinfo-ai-next ai))))))))

(define (make-client-socket host service . opts)
  (define family (get-optional opts usocket:AF_UNSPEC))
  (define socktype (get-optional opts usocket:SOCK_STREAM cdr))
  (define flags (get-optional opts *default-ai-flags* cdr cdr))
  (define protocol (get-optional opts usocket:IPPROTO_IP cdr cdr cdr))
  (let* ((box (getaddrinfo host service flags family socktype protocol))
	 (sock (get-socket (pointer-ref-c-pointer box 0) c:connect)))
    (freeaddrinfo box)
    (unless sock (error 'make-client-socket "Failed to connect" host service))
    (make-socket sock 'client host service)))

(define (make-server-socket service . opts)
  (define family (get-optional opts usocket:AF_UNSPEC))
  (define socktype (get-optional opts usocket:SOCK_STREAM cdr))
  (define protocol (get-optional opts usocket:IPPROTO_IP cdr cdr))

  (let* ((box (getaddrinfo null-pointer service 0 family socktype protocol))
	 (sock (get-socket (pointer-ref-c-pointer box 0) c:bind)))
    (freeaddrinfo box)
    (unless sock (error 'make-server-socket "Failed to bind" service))
    (when (= socktype usocket:SOCK_STREAM)
      (unless (zero? (c:listen sock usocket:SOMAXCONN))
	(c:close sock)
	(error 'make-server-socket "Failed to listen" service)))
    (make-socket sock 'server #f service)))

(define (socket-close sock) (c:close (socket-socket sock)))
(define (socket-shutdown sock how) (c:shutdown (socket-socket sock) how))

(define (socket-accept sock)
  (define (free ss sl) (psystem:free ss) (psystem:free sl))
  (unless (socket? sock)
    (assertion-violation 'socket-accept "A socket required" sock))
  (let ((ss (psystem:malloc usocket:size-of-sockaddr-storage))
	(sl (psystem:malloc usocket:size-of-socklen_t)))
    (let ((fd (c:accept (socket-socket sock) ss sl)))
      (free ss sl)
      (when (= fd -1) (error 'socket-accept "Failed to accept"))
      (make-socket fd 'server #f #f))))

(define (socket-send socket bv . opt)
  (define flags (get-optional opt 0))
  (unless (bytevector? bv)
    (assertion-violation 'socket-send "Bytevector required" bv))
  (unless (socket-writeable? socket)
    (assertion-violation 'socket-send "Socket is not writable" bv))
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
  (unless (socket-readable? socket) (error 'socket-recv "Socket not available"))
  (let ((c (c:recv fd p size flags)))
    (cond ((= c size) buf)
	  ((< c 0) (error 'socket-recv "Failed to receive"))
	  ((< c size)
	   (let ((r (make-bytevector c)))
	     (do ((i 0 (+ i 1)))
		 ((= i c) r)
	       (bytevector-u8-set! r i (bytevector-u8-ref buf i))))))))

(define (call-with-socket socket proc)
  (let-values ((args (proc socket)))
    (socket-close socket)
    (apply values args)))

;; Copied from Sagittarius
(define-syntax define-one-flag-operation
  (syntax-rules ()
    ((_ ?name (?op ?flag) ...)
     (define-syntax ?name
       (lambda (x)
	 (define (lookup name)
	   (case (syntax->datum name)
	     ((?op)   #'?flag)
	     ...
	     (else (syntax-violation '?name "unknown flag" name))))
	 (syntax-case x ()
	   ((_ flag)
	    (lookup #'flag))))))))

(define-one-flag-operation address-family
  (inet   usocket:AF_INET)
  (inet6  usocket:AF_INET6)
  (unspec usocket:AF_UNSPEC))

(define-one-flag-operation ip-protocol
  (ip  usocket:IPPROTO_IP)
  (tcp usocket:IPPROTO_TCP)
  (udp usocket:IPPROTO_UDP))

(define-one-flag-operation socket-domain 
  (stream   usocket:SOCK_STREAM)
  (datagram usocket:SOCK_DGRAM))

(define-syntax define-flag-operation
  (syntax-rules ()
    ((_ ?name (?op ?flags) ...)
     (define-syntax ?name
       (lambda (x)
	 (define (lookup names flags)
	   (syntax-case names (?op ...)
	     (() flags)
	     ((?op rest (... ...)) 
	      (lookup #'(rest (... ...)) (bitwise-ior ?flags)))
	     ...
	     (_ (syntax-violation '?name "unknown flag" names))))
	 (syntax-case x ()
	   ((_ names (... ...))
	    (with-syntax ((flags (lookup #'(names (... ...)) 0)))
	      #'flags))))))))

(define-flag-operation address-info
  (canoname     usocket:AI_CANONNAME)
  (numerichost  usocket:AI_NUMERICHOST)
  (v4mapped     usocket:AI_V4MAPPED)
  (all          usocket:AI_ALL)
  (addrconfig   usocket:AI_ADDRCONFIG))

(define-flag-operation message-type
  (none 0)
  (peek usocket:MSG_PEEK)
  (oob  usocket:MSG_OOB)
  (wait-all usocket:MSG_WAITALL))

(define-syntax shutdown-method
  (lambda (x)
    (define (resolve names)
      (if (null? (cdr names))
	  (let ((name (car names)))
	    (case name
	      ((read)  #'usocket:SHUT_RD)
	      ((write) #'usocket:SHUT_WR)
	      (else (syntax-violation 'shutdown-method "unknown flag" name))))
	  ;; needs to be read and write
	  (or (and (null? (cddr names))
		   (memq 'write names)
		   (memq 'read names)
		   #'usocket:SHUT_RDWR)
	      (syntax-violation 'shutdown-method "invalid shutdown-method"
				names))))
    (syntax-case x ()
      ((_ names ...)
       (resolve (syntax->datum #'(names ...)))))))

(define (socket-purge-flags base . rest) 
  (let ((mask* (map (lambda (f) (bitwise-xor -1 f)) rest)))
    (apply bitwise-and base mask*)))

;; internal for now

;; int poll(struct pollfd *fds, nfds_t nfds, int timeout);
(define c:poll
  (foreign-procedure *psystem:libc* int poll (pointer unsigned-int int)))
(define (socket-readable? sock)
  (pollfd-readable? (socket-read-poll sock) -1))
(define (pollfd-readable? fd timeout)
  (let ((n (c:poll fd 1 timeout)))
    (and (= n 1)
	 (= (bitwise-and (pollfd-revents fd) usocket:POLLIN) usocket:POLLIN)
	 (zero? (bitwise-and (pollfd-revents fd) usocket:POLLHUP)))))

(define (socket-writeable? sock)
  (= (c:poll (socket-write-poll sock) 1 0) 1))

)
