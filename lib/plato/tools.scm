;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; plato/tools.scm - Tools for plato
;;;  
;;;   Copyright (c) 2015  Takashi Kato  <ktakashi@ymail.com>
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

;; this doesn't have to be a library
(library (plato tools)
    (export plato-skelton
	    plato-add-webapp)
    (import (rnrs)
	    (sagittarius)
	    (util file)
	    (plato invoke)
	    (pp))

(define (plato-skelton root)
  (create-directory* root)
  (create-directory* (build-path root +plato-lib-dir+))
  (create-directory* (build-path root +plato-app-dir+))
  (let ((file (build-path root "run.scm")))
    (unless (file-exists? file)
      (call-with-output-file file
	(lambda (out)
	  (display ";; -*- mode:scheme; coding:utf-8 -*-" out) (newline out)
	  (pp '(import (rnrs)
		       (paella)
		       (plato)
		       (getopt)
		       (clos user)
		       (util concurrent)
		       (sagittarius)
		       (sagittarius io)
		       (sagittarius process)
		       (sagittarius socket)
		       (sagittarius threads)
		       (sagittarius remote-repl)) out)
	  (newline out) (newline out)
	  
	  (write '(define-constant +stop-commands+ '("stop" "restart"))
		   out)
	  (newline out) (newline out)
	  
	  (display ";; paella dispatcher" out) (newline out)
	  (write `(define http-dispatcher
		    (plato-make-dispatcher ,(absolute-path root)))
		 out)
	  (newline out) (newline out)

	  (pp '(define (make-shutdown-handler shared-queue)
		 (lambda (server socket)
		   (let ((command (utf8->string (socket-recv socket 255))))
		     (and (member command +stop-commands+)
			  (shared-queue-put! shared-queue command)))))
	      out)
	  (newline out) (newline out)

	  (display ";; create server config" out) (newline out)
	  (pp '(define (make-server-config shutdown-port max-thread)
		 (let* ((p (string->number shutdown-port))
			(shared-queue (make-shared-queue))
			(handler (make-shutdown-handler shared-queue)))
		   (values 
		    (make-http-server-config 
		     :max-thread max-thread
		     :shutdown-port (and p shutdown-port)
		     :shutdown-handler handler)
		    shared-queue)))
	      out)
	  (newline out) (newline out)

	  (display ";; run the server" out) (newline out)
	  (pp '(define (run-server port config remote-port shared-queue)
		 (define server-thread
		   (make-thread 
		    (lambda ()
		      (plato-run port config http-dispatcher))))
		 
		 (define (wait! socket)
		   (thread-join! server-thread)
		   (when socket
		     (socket-shutdown socket SHUT_RDWR)
		     (socket-close socket))
		   (unless (shared-queue-empty? shared-queue)
		     (let ((command (shared-queue-get! shared-queue)))
		       (when (equal? command "restart")
			 (run-server port config remote-port shared-queue)))))
		 
		 (thread-start! server-thread)
		 (if remote-port
		     (let-values (((repl socket)
				   (make-remote-repl remote-port)))
		       (thread-start! (make-thread repl))
		       (wait! socket))
		     (wait! #f)))
	      out)
	  (newline out) (newline out)

	  (pp '(define (send-command config command)
		 (let ((socket (make-client-socket 
				"localhost"
				(slot-ref config 'shutdown-port))))
		   (socket-send socket (string->utf8 command))))
	      out)
	  (newline out) (newline out)

	  (pp '(define (usage script)
		 (format #t "~a [OPTIONS]~%" script)
		 (format #t " OPTIONS~%")
		 (format #t "  -p<num>,--port <num>\tspecify HTTP port (default 8080)~%")
		 (format #t "  -r<num>,--remote-port <num>\tspecify remote repl port~%")
		 (format #t "  -s<num>,--shutdown-port <num>\tspecify shutdown port (default 8081)~%")
		 (format #t "  -c<command>,--command <command>\trun the command~%")
		 (format #t "    `run`     starts the server~%")
		 (format #t "    `restart` re-starts the server~%")
		 (format #t "    `stop`    stops the server~%")
		 (format #t "  -m<num>,--max-thread <num>\tspecify the number of server thread~%")
		 (format #t "  -h,--help\tshow this message~%"))
	      out)
	  (newline out) (newline out)

	  (display ";; entry point" out) (newline out)
	  (pp '(define (main args)
		 (with-args (cdr args)
		     ((port          (#\p "port") #t "8080")
		      (remote-port   (#\r "remote-port") #t #f)
		      (shutdown-port (#\s "shutdown-port") #t "8081")
		      (command       (#\c "command") #t "run")
		      (max-thread    (#\t "max-thread") #t "10")
		      (help          (#\h "help") #f #f))
		   (when help (usage (car args)) (exit 0))
		   (let-values (((config shared-queue)
				 (make-server-config shutdown-port
					     (string->number max-thread))))
		     (case (string->symbol command)
		       ((run) (run-server port config remote-port shared-queue))
		       (else  (send-command config command))))))
	      out)
	  (newline out))))))

(define (plato-add-webapp root name)
  (create-directory* (build-path* root +plato-app-dir+ name))
  (let ((file (build-path* root +plato-app-dir+ name +plato-handler-file+)))
    (unless (file-exists? file)
      (call-with-output-file file
	(lambda (out)
	  (display ";; auto generated stub" out) (newline out)
	  (pp `(library (plato webapp ,(string->symbol name))
		   (export entry-point
			   support-methods)
		   (import (rnrs)
			   (paella))
		 (define (support-methods) '(GET))
		 (define (entry-point req)
		   (values 200 'text/plain "OK")))
	      out))))))

)
