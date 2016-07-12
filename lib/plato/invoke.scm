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
	    plato-add-root
	    plato-run
	    plato-collect-handler
	    plato-load

	    ;; context
	    *plato-current-context*
	    plato-context-root
	    plato-current-path
	    plato-work-path
	    plato-parent-context
	    plato-application-name
	    plato-context-lock
	    with-plato-context-lock

	    ;; for tools
	    +plato-handler-file+
	    +plato-meta-file+
	    +plato-lib-dir+
	    +plato-app-dir+
	    make-plato-webapp-name)
    (import (rnrs)
	    (rnrs eval)
	    (paella)
	    (net server)
	    (sagittarius)
	    (sagittarius control)
	    (sagittarius regex)
	    (clos user)
	    (match)
	    (util file)
	    (rfc uri)
	    (srfi :1 lists)
	    (srfi :18 multithreading)
	    (srfi :39 parameters))

(define-constant +plato-handler-file+ "handler.scm")
(define-constant +plato-meta-file+    "meta.scm")
(define-constant +plato-lib-dir+      "lib")
(define-constant +plato-app-dir+      "apps")
(define-constant +plato-work-dir+     "work")

(define *plato-current-context* (make-parameter #f))

(define-record-type (<plato-context> make-plato-context plato-context?)
  (fields (immutable context-root plato-context-root) ;; should we?
	  (immutable current-path plato-current-path)
	  ;; temporary path for this context
	  (immutable work-path    plato-work-path)
	  (immutable parent       plato-parent-context)
	  (immutable app-name     plato-application-name)
	  ;; if user/framework needs lock per context
	  (immutable lock         plato-context-lock))
  (protocol (lambda (n)
	      (lambda (root path work parent name)
		(n root path work parent name (make-mutex))))))

(define-syntax with-plato-context-lock
  (syntax-rules ()
    ((_ context expr ...)
     (let ((lock (plato-context-lock context)))
       (dynamic-wind
	   (lambda () (mutex-lock! lock))
	   (lambda () expr ...)
	   (lambda () (mutex-unlock! lock)))))))

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

The library can also provide sub contexts via mount-paths procedure.
If this is provided, then it's all added to paths.
e.g.) if (mount-paths) reuturns (((GET) "/bar" #<closure>))
      then the mount path and handler is like the following:
       supporting HTTP method: GET
       mount-path: /handler/bar
       handler: #<closure>
In this case, the plato-context contains parent context field which contains
the context of parents.

|#
(define (plato-make-dispatcher server-root)
  (plato-add-root (make-http-server-dispatcher) server-root))

(define (plato-add-root dispatcher server-root)
  (let ((handlers (plato-collect-handler server-root)))
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

;; FIXME: maybe we should only get directories under app directory
;;        and let plato-load handle if the given directory contains
;;         application or not.
(define (plato-collect-handler root)
  (define (retrieve-path path)
    (cond ((#/apps(?:\/|\\)([^\/\\]+?)(?:\/|\\)(?:meta|handler).scm$/ path) => ;; |
	   (lambda (m) (m 1)))
	  (else #f)))
  (let ((dir (build-path* root +plato-app-dir+)))
    (filter-map retrieve-path
		(find-files dir 
			    :pattern #/^(meta|handler).scm$/ ;; |
					     ))))

(define (make-plato-webapp-name handler)
  (list 'plato 'webapp (string->symbol handler)))

#|
A meta.scm must contain a alist. The key is
 - handler:   handler file      (default 'handler.scm')
 - load-path: library directory (default '.')
 - ... (not decided yet, so later)
|#
(define (read-meta-file file)
  (if (file-exists? file)
      (call-with-input-file file read)
      '()))
(define (meta-ref meta key default)
  (cond ((assq key meta) => cadr)
	(else default)))

(define (plato-load handler root dispatcher)
  (define current-load-path (load-path))
  (define handler-path (build-path* root +plato-app-dir+ handler))
  (define meta-path (build-path handler-path +plato-meta-file+))

  (define work-path (build-path* root +plato-work-dir+ handler))
  ;; allow r7rs style as well
  (define env (environment '(only (sagittarius) library define-library)))
  ;; context will be unique in handler so we don't have to 
  ;; make extra storage for this to make sure the uniqueness.
  (define context (make-plato-context
		   ;; should we make this absolute path, explicitly?
		   root
		   handler-path
		   work-path
		   #f
		   handler))
  (define (create-plato-handler proc context)
    (lambda (req)
      (parameterize ((current-directory (plato-current-path context))
		     (*plato-current-context* context))
	(proc req))))
  (define (create-plato-sub-handler path proc)
    (define (ensure-path path)
      (define (create-if-needed path)
	(let ((p (build-path* handler-path path)))
	  (unless (file-exists? p) (create-directory* p))
	  p))
      (let ((p (regex-replace-all #/\\/ path "/")))
	(cond ((#/^\/+(.+)/ p) => 
	       (lambda (m) (create-if-needed (m 1))))
	      (else (create-if-needed p)))))

    (if (string? path)
	(create-plato-handler 
	 proc
	 (make-plato-context handler-path
			     (ensure-path path)
			     work-path ;; TODO should we separate?
			     context
			     handler))
	;; unfortunately, we can't determine now.
	;; so it'll be runtime
	(lambda (req)
	  ;; at least at this point, it's matched
	  (let ((uri (http-request-uri req)))
	    (let-values (((scheme ui hs p path q f) (uri-parse uri)))
	      ;; path must be there
	      (let ((this-path (build-path* root +plato-app-dir+ path)))
		(unless (file-exists? this-path) (create-directory* this-path))
		(parameterize ((current-directory this-path)
			       (*plato-current-context* (make-plato-context
							 handler-path
							 this-path
							 work-path
							 context
							 handler)))
		  (proc req))))))))
  
  (define (sub-context parent env)
    ;; adding parent context
    ;; if the given path contains '/' in front then remove it.
    (define (ensure-root-context path)
      (if (string? path)
	  (cond ((#/^\/+(.+)/ path) => 
		 (lambda (m) (string-append parent "/" (m 1))))
		(else  (string-append parent "/" path)))
	  (let ((pat (regex-pattern path)))
	    ;; bit too naive but better than nothing
	    (regex
	     (cond ((#/^\/+(.+)/ pat) => 
		    (lambda (m) (string-append "^" parent "/" (m 1))))
		   (else  (string-append "^" parent "/" pat)))))))

    (let ((sub-paths (guard (e (else '())) (eval '(mount-paths) env))))
      (for-each (match-lambda
		 (((methods ...) path handler)
		  (dolist (m methods)
		    (http-add-dispatcher!
		     dispatcher m (ensure-root-context path)
		     (create-plato-sub-handler path handler)))))
		sub-paths)))

  (define meta (read-meta-file meta-path))
  (define handler-file (meta-ref meta 'handler +plato-handler-file+))
  (define loading-path (build-path handler-path (meta-ref meta 'load-path ".")))

  (unless (file-exists? work-path) (create-directory* work-path))
  (parameterize ((load-path (cons* (build-path root +plato-lib-dir+)
				   loading-path
				   current-load-path))
		 ;; make sure loading time can also find resource
		 (current-directory handler-path))
    ;; TODO should we wrap with guard to make this run anyway?
    (let ((file (build-path handler-path handler-file))
	  ;; (plato webapp $handler) is the library name
	  (lib (make-plato-webapp-name handler)))
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
	   (create-plato-handler proc context)))
	(sub-context path e)))))

)
