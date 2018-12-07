#!r6rs
(import (rnrs)
	(usocket)
	;; FIXME chez?
	(srfi :18)
	(srfi :64))

(define service "10000")
(define echo-server-socket (make-tcp-server-socket service))

(define (socket-shutdown&close s)
  (socket-shutdown! s *usocket:shutdown-read&write*)
  (socket-close! s))

(define (echo-server-start! socket)
  (let loop ((s (server-socket-accept! socket)))
    (define in (transcoded-port (client-socket-input-port s)
				(native-transcoder)))
    (define out (transcoded-port (client-socket-output-port s)
				 (native-transcoder)))
    (let loop2 ()
      (let ((line (get-line in)))
	(cond ((or (eof-object? line) (string=? line "exit"))
	       (socket-shutdown&close s)
	       (socket-shutdown&close socket))
	      ((string=? line "nomore")
	       (socket-shutdown&close s)
	       (loop (server-socket-accept! socket)))
	      (else (put-string out line)
		    (flush-output-port out)
		    (loop2)))))))
  
(define thread
  (thread-start!
   (make-thread
    (lambda ()
      (guard (e (else (display e) (newline)))
	(echo-server-start! echo-server-socket))))))

(test-begin "TCP socket")

(let ((s (make-tcp-client-socket "localhost" service)))
  (test-assert "socket?" (socket? s))
  (test-assert "client-socket?" (client-socket? s))
  (let ((in (transcoded-port (client-socket-input-port s)
			     (native-transcoder)))
	(out (transcoded-port (client-socket-output-port s)
			      (native-transcoder))))
    (put-string out "hi\n")
    (test-equal "hi" (get-string-n in 2))
    (put-string out "exit")
    (socket-shutdown&close s)))

(thread-join! thread)

(test-end)
