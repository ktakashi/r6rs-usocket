#!r6rs
(import (rnrs)
	(usocket)
	(srfi :64))

(define service "10000")
(define (socket-shutdown&close s)
  (usocket-shutdown! s *usocket:shutdown-read&write*)
  (usocket-close! s))

(test-begin "TCP socket")

(let ((s (make-tcp-client-usocket "localhost" service)))
  (test-assert "usocket?" (usocket? s))
  (test-assert "client-usocket?" (client-usocket? s))
  (let ((in (transcoded-port (client-usocket-input-port s)
			     (native-transcoder)))
	(out (transcoded-port (client-usocket-output-port s)
			      (native-transcoder))))
    (put-string out "hi\n")
    (test-equal "hi" (get-string-n in 2))
    (put-string out "exit")
    (socket-shutdown&close s)))

(test-end)
