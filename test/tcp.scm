#!r6rs
(import (rnrs)
	(usocket)
	(srfi :64))

(define service "10000")
(define (socket-shutdown&close s)
  (socket-shutdown! s *usocket:shutdown-read&write*)
  (socket-close! s))

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

(test-end)
