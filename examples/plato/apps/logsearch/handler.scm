;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; Simple logsearch app
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
(library (plato webapp logsearch)
    (export entry-point support-methods mount-paths)
    (import (rnrs) 
	    (paella)
	    (tapas)
	    (plato)
	    (cuberteria)
	    (util file)
	    (util concurrent)
	    (util bytevector)
	    (rename (binary io) (get-line binary:get-line))
	    (text json)
	    (srfi :1 lists)
	    (srfi :18 multithreading)
	    (srfi :39 parameters)
	    (clos user)
	    (clos core)
	    (sagittarius)
	    (sagittarius io)
	    (sagittarius time)
	    (sagittarius control)
	    (sagittarius regex))

  (define-constant +config+ "config.scm")
  (define-constant +default-search-threads+ 10)
  (define-constant +default-timeout+       3000) ;; 3000ms = 3s

  (define (bytevector->json bv)
    (parameterize ((*json-map-type* 'alist))
      (json-read (open-string-input-port (utf8->string bv)))))
  (define-class <json-request> (<converter-mixin>) 
    ((json :converter bytevector->json)))

  (define (json->response json)
    (cuberteria-map-object! (make <json-response>) json
			    (lambda (p) (string->symbol (car p)))
			    cdr))
  (define-class <json-response> (<converter-mixin>)
    (thread-id
     files
     regexp
     query
     done?
     next
     wait
     (next-query :converter json->response :json #t)))

  ;; FIXME: using global thread pool isn't a good idea
  (define search-thread-pool #f)
  (define search-thread-data #f)
  (define search-thread-timeout #f)
  (define %raw-timeout #f)

  (define (init-globals context)    
    (unless search-thread-pool
      (let* ((pwd (plato-current-path context))
	     (config (call-with-input-file (build-path pwd +config+) read))
	     (count (cond ((assq 'threads config) => cdr)
			  (else +default-search-threads+)))
	     (timeout (cond ((assq 'timeout config) => cdr)
			    (else +default-timeout+))))
	(set! search-thread-pool (make-thread-pool count))
	(set! search-thread-data (make-vector count #f))
	(set! %raw-timeout timeout)
	(set! search-thread-timeout (make-time time-duration 
					       ;; milli -> nano
					       (* timeout 1000000)
					       0)))))

  (define (logsearch-handler req)
    ;; load configuration into session
    (define context (*plato-current-context*))
    (define pwd (plato-current-path context))
    ;; if it's not there let it fail
    ;; TODO better error handling?
    (define config (call-with-input-file (build-path pwd +config+) read))
    (and-let* ((log-glob (assq 'log-glob config))
	       (session (*plato-current-session*)))
      (plato-session-set! session "log-glob" (cdr log-glob)))
    (init-globals context)
    (call-with-input-file "main.html" html->tapas-component))
  
  (define script-loader (cuberteria-resource-loader 'text/javascript "."))
  (define style-loader (cuberteria-resource-loader 'text/css "."))

  (define (list-file-handler req)
    (or (and-let* ((session (*plato-current-session*))
		   (log-glob (plato-session-ref session "log-glob")))
	  (values 200 
		  'application/json
		  (call-with-string-output-port
		   (lambda (out)
		     (json-write (glob log-glob) out)))))
	(values 200 'application/json "[]")))

  (define (make-task json sq)
    (define context (plato-parent-context (*plato-current-context*)))
    (define pwd (plato-current-path context))
    (define config (call-with-input-file (build-path* pwd +config+) read))
    (define head (regex (string-append "^" (cdr (assq 'log-head config)))))
    (define-values (buffer get-buffer) (open-bytevector-output-port))

    (define (read-block in)
      ;; we read 2 log blocks to make sure we handle a log line properly
      (let loop ((line (binary:get-line in)))
	(cond ((eof-object? line) line)
	      ((head line)
	       (put-bytevector buffer line)
	       (let loop2 ((pos (port-position in)))
		 (let ((line2 (binary:get-line in)))
		   (cond ((eof-object? line2) (get-buffer))
			 ((head line2) (set-port-position! in pos) (get-buffer))
			 (else (put-bytevector buffer line2) 
			       (put-u8 buffer 10)
			       (loop2 (port-position in)))))))
	      ;; I don't know the format so ignore.
	      (else (loop (binary:get-line in))))))
    (define (search files pred)
      ;; reuse buffer
      (define buffer (make-bytevector 8096))
      (dolist (file files)
	(let ((in #;(open-file-input-port file (file-options no-fail)
					(buffer-mode block))
		  (buffered-port
		   (open-file-input-port file (file-options no-fail)
					 (buffer-mode none))
		   (buffer-mode block)
		   :buffer buffer)))
	  (let loop ()
	    (let ((block (read-block in)))
	      (unless (eof-object? block)
		(when (pred block) 
		  (let ((line (if (null? (cdr files))
				  (utf8->string block)
				  (string-append file ":"
						 (utf8->string block)))))
		    (shared-queue-put! sq line)))
		(loop))))
	  (close-input-port in)))
      (shared-queue-put! sq #t))

    (lambda ()
      (let ((files (vector->list (slot-ref json 'files)))
	    ;; TODO handle other encoding
	    (text  (slot-ref json 'query)))
	(if (slot-ref json 'regexp)
	    (let ((p (regex text)))
	      (search files (lambda (block) (p block))))
	    (let ((text (string->utf8 text)))
	      (search files (lambda (block) 
			      (bytevector-contains block text))))))))

  (define (search-handler req)
    (define uri (http-request-uri req))
    (define session (*plato-current-session*))
    (define (json->string json)
      (call-with-string-output-port
       (lambda (out)
	 (json-write json out))))
    (define (retrieve-results id sq)
      ;; emulates max timeout period
      (define timeout (add-duration (current-time) search-thread-timeout))
      (if (or (not id) (thread-pool-thread-task-running? search-thread-pool id))
	  (let loop ((r '()))
	    (let ((q (shared-queue-get! sq timeout)))
	      (cond (q (loop (cons q r)))
		    (else r))))
	  '(#t)))
    (define (response-it results next-query)
      ;; if the results contains #t in the first element then it's ended
      (let* ((done? (and (not (null? results)) (eqv? (car results) #t)))
	     (texts (if (and done? (not (null? results))) 
			(cdr results) 
			results)))
	(parameterize ((*json-map-type* 'alist))
	  (values 200 'application/json
	    (json->string `((result . ,(list->vector texts))
			    (done   . ,done?)
			    (next   . ,uri)
			    (wait   . ,%raw-timeout)
			    (next-query  . ,(cuberteria-object->json next-query))))))))

    (define (terminate-previous-process session)
      (and-let* ((id (plato-session-ref session "thread-id"))
		 (thread (thread-pool-thread search-thread-pool id)))
	(when (equal? (thread-specific thread) (plato-session-name session))
	  (thread-pool-thread-terminate! search-thread-pool id))))

    (init-globals (plato-parent-context (*plato-current-context*)))
    (or (and-let* ((json-request 
		    (cuberteria-map-http-request! (make <json-request>) req))
		   ( (slot-bound? json-request 'json) ) ;; in case
		   (json (json->response (slot-ref json-request 'json))))
	  (cond ((slot-bound? json 'thread-id)
		 (let* ((id (slot-ref json 'thread-id))
			(sq (vector-ref search-thread-data id)))
		   (response-it (retrieve-results id sq) json)))
		(else
		 ;; terminate if there's already a session
		 (terminate-previous-process session)
		 (let* ((sq (make-shared-queue))
			(id (thread-pool-push-task! search-thread-pool
						    (make-task json sq)))
			(t (thread-pool-thread search-thread-pool id)))
		   (thread-specific-set! t (plato-session-name session))
		   (plato-session-set! session "thread-id" id)
		   (vector-set! search-thread-data id sq)
		   (slot-set! json 'thread-id id)
		   (response-it (retrieve-results #f sq) json)))))
	(values 500 'text/plain "Invalid parameter")))
	

  (define (mount-paths)
    `( 
      ((GET)  #/scripts/ ,script-loader)
      ((GET)  #/styles/  ,style-loader)
      ;; need session to find files
      ((GET)  "/list_file" ,(plato-session-handler list-file-handler))
      ((POST) "/search"    ,(plato-session-handler search-handler))
       
      ))
  (define (support-methods) '(GET))
  (define entry-point
    (plato-session-handler (tapas-request-handler logsearch-handler)))

)
