;; -*- mode:scheme; coding:utf-8 -*-
(import (rnrs)
	(paella)
	(net server))

(define (root-handler req)
  (values 200 'text/plain "OK"))

(define (root-filter req next-filter)
  (display "filter1") (newline)
  (next-filter req))
(define (root2-filter req next-filter)
  (display "filter2") (newline)
  (let-values (((s mime content . opt) (next-filter req)))
    (apply values s mime content '("extra" "value") opt)))

(define http-dispatcher
  (make-http-server-dispatcher
   (GET "/" root-handler)
   (http-filter "/" root-filter)
   (http-filter "/" root2-filter)))

(define config (make-http-server-config))

(define server (make-simple-server "8080" (http-server-handler http-dispatcher)
				   :config config))
(server-start! server)
