#!read-macro=sagittarius/bv-string
(import (rnrs)
	(paella)
	(net server)
	(rfc http)
	(util bytevector)
	(sagittarius socket)
	(srfi :18)
	(srfi :64))

(define (async-handler req)
  ;;(thread-sleep! 1)
  #;(let-values (((s h b) (http-get "google.com" "/" :secure? #t)))
       (values 200 'text/html b))
  (http-request-start-async! req
   (lambda (req)
     (thread-sleep! 1)
     (let-values (((s h b) (http-get "google.com" "/" :secure? #t)))
       (values 200 'text/html b))))
  (values #f #f #f))

(define http-dispatcher
  (make-http-server-dispatcher
   (GET "/async" async-handler)))

(define config (make-http-server-config
		:max-thread 1
		:exception-handler (lambda (srv sock e)
				     (report-error e)
				     (socket-shutdown sock SHUT_RDWR)
				     (socket-close sock))))

(define server (make-simple-server "8080" (http-server-handler http-dispatcher)
				   :config config))

(test-begin "Async")
(server-start! server :background #t)

(define (simple-request)
  (define socket (make-client-socket "localhost" "8080"))
  (socket-send socket #*"GET /async HTTP/1.1\r\n")
  (socket-send socket #*"Host localhost\r\n\r\n")
  socket)

(define sockets
  (do ((i 0 (+ i 1))
       (sockets '() (cons (simple-request) sockets)))
      ((= i 5) sockets)
    (test-equal i (length sockets))))

(test-equal 5 (length sockets))

(define (simple-response socket)
  (test-assert (socket-recv socket 255))
  (socket-shutdown socket SHUT_RDWR)
  (socket-close socket))

(for-each simple-response sockets)

(server-stop! server)
(test-end)



