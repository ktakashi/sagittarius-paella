;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; repl.scm - Example for switching protocl on plato
;;;  
;;;   Copyright (c) 2016  Takashi Kato  <ktakashi@ymail.com>
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

(library (plato webapp repl)
  (export entry-point support-methods mount-paths)
  (import (rnrs)
	  (rnrs eval)
	  (clos user)
	  (sagittarius)
	  (sagittarius socket)
	  (paella)
	  (tapas)
	  (rfc websocket connection)
	  (rfc websocket messages))

(define (websocket-repl socket method path)
  (define connection (server-socket->websocket-connection socket))
  ;; If you want more, then add some more
  (define env (environment '(only (sagittarius)
				  import library define-library)))
  (define (eval-from-string s)
    (guard (e (else (call-with-string-output-port
		     (lambda (out)
		       (report-error e out)))))
      (let ((e (read (open-string-input-port s))))
	(format "~s" (eval e env)))))

  (guard (e (else (report-error e)
		  (socket-shutdown socket SHUT_RDWR)
		  (socket-close socket)))
    (websocket-connection-accept-handshake! connection '("repl"))
    (lambda (socket)
      (guard (e (else (websocket-connection-close! connection)))
	(let-values (((opcode data) (websocket-receive connection)))
	  (cond ((= opcode +websocket-close-frame+)
		 (websocket-connection-close! connection))
		((= opcode +websocket-binary-frame+)
		 (websocket-send-binary connection data))
		((= opcode +websocket-text-frame+)
		 (websocket-send-text connection (eval-from-string data)))
		(else 
		 (websocket-send-close connection
				       (websocket-compose-close-status 1002)
				       #f)
		 (websocket-connection-close! connection))))))))
      
(define (mount-paths)
  `(
    (UPGRADE "/eval" ,websocket-repl)
    ))
  
(define (support-methods) '(GET))

(define (main-handler req)
  (let ((page (call-with-input-file "main.html" html->tapas-component))
	(javascript (format "start_repl('~a')" (http-request-port req))))
    (slot-set! page 'body-attributes `((onload ,javascript)))
    page))
(define entry-point
  (tapas-request-handler main-handler))

)
