#!read-macro=sagittarius/regex
#!read-macro=sagittarius/bv-string

(add-load-path "../lib")
(import (rnrs)
	(net server)
	(rfc http)
	(sagittarius control)
	(sagittarius regex)
	(sagittarius socket)
	(paella))

(define (connect-handler req ignore)
  (define uri (http-request-uri req))
  (define-values (server port) 
    (cond ((#/:(\d+)$/ uri) => (lambda (m) (values (m 'before) (m 1))))
	  (else (values uri "80"))))
  (define client-socket (http-request-socket req))
  (define bufsiz 4096)
  (define buf0 (make-bytevector bufsiz))
  (define buf1 (make-bytevector bufsiz))
  (define count 2)

  (define (pipe buf in out)
    ;; set the socket non-blocking
    (socket-nonblocking! in)
    (let rec ()
      ;; read until it returns -1
      ;; NB: socket-recv! is not documented
      (let1 r (socket-recv! in buf 0 bufsiz)
	(cond ((zero? r) ;; nothing to read
	       (set! count (- count 1))
	       (socket-blocking! in)
	       (socket-shutdown out SHUT_WR)
	       #t)
	      ;; not ready yet, so go back
	      ((negative? r) (socket-blocking! in) #f)
	      ((= r bufsiz)
	       (socket-send out buf)
	       (rec))
	      (else
	       (socket-send out (bytevector-copy buf 0 r))
	       (rec))))))
  
  (define (tunnel-loop csock rsock)
    (define fdset (make-fdset))

    (fdset-set! fdset csock #t)
    (fdset-set! fdset rsock #t)

    (do () ((= count 0))
      (let-values (((n r w e) (socket-select fdset #f #f 1)))
	(dolist (sock (collect-sockets r))
	  (if (eq? sock csock)
	      (when (pipe buf0 sock rsock) (fdset-set! fdset sock #f))
	      (when (pipe buf1 sock csock) (fdset-set! fdset sock #f)))))))
  
  (let1 remote-socket (make-client-socket server port)
    (unwind-protect
     (begin
       (socket-send client-socket #*"HTTP/1.1 200 OK\r\n\r\n")
       (tunnel-loop client-socket remote-socket))
     (socket-close remote-socket))
    ;; return #f status and 'none mime type to let paella know 
    ;; that it doesn't have to do anything after this.
    (values #f 'none #f)))

;; Add CONNECT method
;; use '*' so that the handler can handle everything.
(define http-dispatcher
  (make-http-server-dispatcher
   (CONNECT * connect-handler)))

(define config (make-http-server-config :max-thread 5))
(define server 
  (make-simple-server "1080" (http-server-handler http-dispatcher)
                      :config config
		      :exception-handler print))

(server-start! server)

#|
to see how it goes:

$ curl -L -v -p -x localhost https://google.com
|#
