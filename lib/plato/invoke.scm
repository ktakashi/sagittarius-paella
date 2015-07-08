;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; plato/invoke.scm - Invoker for plato
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

#!read-macro=sagittarius/regex
(library (plato invoke)
    (export invoke-plato
	    plato-make-dispatcher
	    plato-run
	    plato-collect-handler
	    plato-load
	    ;; for tools
	    +plato-handler-file+
	    +plato-lib-dir+
	    +plato-app-dir+)
    (import (rnrs)
	    (rnrs eval)
	    (paella)
	    (net server)
	    (sagittarius)
	    (sagittarius control)
	    (sagittarius regex)
	    (util file)
	    (srfi :1 lists)
	    (srfi :39 parameters))

(define-constant +plato-handler-file+ "handler.scm")
(define-constant +plato-lib-dir+      "lib")
(define-constant +plato-app-dir+      "apps")

#|
Invoking a server takes 2 pass.
- Collects all web context
- Creates paella http server

the web context must be located like this
/$server-root
  - lib/
  - apps/
     - path1/
         handler.scm
     - path2/
         handler.scm
       :
      so on

The handler.scm must be a library whose name is (plato webapp $path)
where the $path is the name of the directory. The library must export
2 procedures: support-methods and entry-point.

support-methods must be a thunk and should return a list of HTTP methods.
e.g.) '(GET POST) these methods are registered to paella dispatcher.

entry-point must be a paella http handler.

The handler.scm is evaluated with library path $server-root/lib and
where its located.
|#
(define (plato-make-dispatcher server-root)
  (let* ((handlers (plato-collect-handler server-root))
	 ;; create empty dispatcher
	 (dispatcher (make-http-server-dispatcher)))
    (dolist (handler handlers)
      ;; evaluate the file
      (plato-load handler server-root dispatcher))
    dispatcher))
(define (plato-run port config dispatcher . opts)
  (let ((server (make-simple-server port (http-server-handler dispatcher)
				    :config config)))
    (apply server-start! server opts)
    (values server dispatcher)))

(define (invoke-plato server-root port config . opts)
  (let ((dispatcher (plato-make-dispatcher server-root)))
    (apply plato-run port config dispatcher opts)))

(define (plato-collect-handler root)
  (define (retrieve-path path)
    (cond ((#/apps(?:\/|\\)([^\/\\]+?)(?:\/|\\)handler.scm$/ path) => 
	   (lambda (m) (m 1)))
	  (else #f)))
  (let ((dir (build-path* root +plato-app-dir+)))
    (filter-map retrieve-path
		(find-files dir 
			    :pattern #/^handler.scm$/))))

(define (plato-load handler root dispatcher)
  (define current-load-path (load-path))
  (define handler-path (build-path* root +plato-app-dir+ handler))
  ;; allow r7rs style as well
  (define env (environment '(only (sagittarius) library define-library)))
  (parameterize ((load-path (cons* (build-path root +plato-lib-dir+)
				   handler-path
				   current-load-path)))
    ;; TODO should we wrap with guard to make this run anyway?
    (let ((file (build-path handler-path +plato-handler-file+))
	  ;; (plato webapp $handler) is the library name
	  (lib (list 'plato 'webapp (string->symbol handler))))
      (call-with-input-file file
	(lambda (in)
	  (let loop ((e (read/ss in)))
	    (unless (eof-object? e)
	      (eval e env)
	      (loop (read/ss in))))))
      ;; we load the library
      (let* ((e (environment lib))
	     (methods (eval '(support-methods) e))
	     (proc (eval 'entry-point e))
	     (path (string-append "/" handler)))
	(dolist (m methods)
	  (http-add-dispatcher! 
	   dispatcher m path 
	   (lambda (req)
	     ;; TODO add context parameters
	     (parameterize ((current-directory handler-path))
	       (proc req)))))))))

)
