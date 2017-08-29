;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; paella.scm - Simple HTTP server for Sagittarius
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


#!read-macro=sagittarius/bv-string
#!read-macro=sagittarius/regex
(library (paella)
    (export http-server-handler
	    make-http-server-config
	    *http-not-found-handler*
	    make-http-server-dispatcher
	    http-add-dispatcher!
	    http-add-protocol-handler!
	    http-add-filter! http-filter

	    ;; records
	    ;; request
	    http-request?
	    http-request-method
	    http-request-path
	    http-request-uri
	    http-request-headers
	    http-request-parameters
	    http-request-cookies
	    http-request-socket
	    http-request-source
	    http-request-port
	    http-request-remote-address
	    http-request-remote-port
	    http-request-server-context
	    ;; parameter
	    http-parameter?
	    http-parameter-name
	    http-parameter-value
	    http-parameter-headers

	    ;; default handler
	    http-file-handler
	    http-registered-path-handler

	    ;; for convenience
	    http-mapped-path->alist

	    ;; parameters
	    *http-server-name*
	    *http-log-directory*
	    ;; for testing
	    make-http-request
	    )
    (import (rnrs)
	    (rnrs mutable-pairs)
	    (sagittarius)
	    (sagittarius regex)
	    (sagittarius socket)
	    (net server)
	    (rfc mime) 
	    (rfc :5322)
	    (rfc uri)
	    (rfc cookie)
	    (rfc gzip)
	    (rfc tls)
	    (clos user)
	    (srfi :1 lists)
	    (srfi :18 multithreading)
	    (srfi :39 parameters)
	    (srfi :117 list-queues)
	    (prefix (binary io) binary:) 
	    (text sxml serializer)
	    (text sxml html-parser)
	    (util file)
	    (util port)
	    (util logging)
	    (pp))

(define *http-log-directory* 
  (make-parameter (build-path (current-directory) "logs")))

(define (default-not-found-handler req)
  (values 404 'text/plain "Not Found"))

(define *http-not-found-handler* (make-parameter default-not-found-handler))
(define *http-server-name* (make-parameter "Sagittarius Paella"))

(define (http-make-path-entry method path)
  (string-append method ":" path))

(define (make-http-server-config . opt)
  (apply make-server-config :non-blocking? #t opt))

;; TODO regex mapping?
(define (http-mapped-path->alist dispatcher)
  (append 
   (map (lambda (p) (string-split p ":"))
	(hashtable-keys-list (http-dispatcher-table dispatcher)))
   (map (lambda (p) (list (format "REGEX-~a" (cdar p)) (regex-pattern (caar p))))
	(http-dispatcher-patterns dispatcher))))

(define-record-type (<http-dispatcher> make-http-dispatcher http-dispatcher?)
  (fields (immutable table http-dispatcher-table)
	  (mutable patterns http-dispatcher-patterns 
		   http-dispatcher-patterns-set!)
	  ;; for HTTP 101
	  (immutable protocol-handlers
		     http-dispatcher-protocol-handlers)
	  (immutable upgraded-sockets
		     http-dispatcher-upgraded-sockets)
	  (immutable filters http-dispatcher-filters ))
  (protocol (lambda (p)
	      (lambda ()
		(p (make-string-hashtable) '()
		   (make-string-hashtable) (make-eq-hashtable)
		   (list-queue))))))

(define-syntax http-filter (syntax-rules ()))
(define-syntax make-http-server-dispatcher
  (syntax-rules (* http-filter)
    ((_ "dispatch" table ((http-filter path handler) next ...))
     (let ((p handler))
       (http-add-filter! table path p)
       (make-http-server-dispatcher "dispatch" table (next ...))))
    ((_ "dispatch" table (((method method* ...) path handler) next ...))
     (begin
       (make-http-server-dispatcher "dispatch" table ((method path handler)))
       (make-http-server-dispatcher "dispatch" 
				    table (((method* ...) path handler)
						 next ...))))
    ((_ "dispatch" table ((() path handler) next ...))
     (make-http-server-dispatcher "dispatch" table (next ...)))
    ((_ "dispatch" table ((method * handler) next ...))
     (let ((p handler))
       (http-add-dispatcher! table 'method "*"
			     (lambda (req)
			       (p req (http-mapped-path->alist table))))
       (make-http-server-dispatcher "dispatch" table (next ...))))
    ((_ "dispatch" table ((method path handler) next ...))
     (let ((p handler))
       (http-add-dispatcher! table 'method path p)
       (make-http-server-dispatcher "dispatch" table (next ...))))
    ;; done
    ((_ "dispatch" table ()) (begin))
    ;; entry point
    ((_ specs ...)
     (let ((r (make-http-dispatcher)))
       (make-http-server-dispatcher "dispatch" r (specs ...))
       r))))

(define (http-add-dispatcher! dispatcher method path handler)
  (define (find-regex-slot pattern method patterns)
    (define (pattern=? a p)
      (let ((b (car p)))
	(and (string=? method (cdr p))
	     (or (string=? (regex-pattern a) (regex-pattern b)) ;; faster
		 (equal? (regex-ast a) (regex-ast b)))
	     (= (regex-flags a) (regex-flags b)))))
    (assoc pattern patterns pattern=?))

  (cond ((string? path)
	 (hashtable-set! (http-dispatcher-table dispatcher)
			 (http-make-path-entry (symbol->string method) path)
			 handler))
	((regex-pattern? path)
	 (let ((p (http-dispatcher-patterns dispatcher))
	       (m (symbol->string method)))
	   (cond ((find-regex-slot path m p) =>
		  (lambda (s) (set-cdr! s handler)))
		 (else
		  (http-dispatcher-patterns-set! dispatcher 
		    (acons (cons path m) handler p))))))
	(else
	 (assertion-violation 'http-add-dispatcher!
			      "string or regex pattern required" path))))

(define (http-add-protocol-handler! dispatcher path handler)
  (let ((protocols (http-dispatcher-protocol-handlers dispatcher)))
    (hashtable-set! protocols path handler)
    dispatcher))

(define (http-add-filter! dispatcher path filter)
  (list-queue-add-back! (http-dispatcher-filters dispatcher) (cons path filter))
  dispatcher)

(define-record-type (<http-request> make-http-request http-request?)
  (fields (immutable method  http-request-method)
	  (immutable path    http-request-path)
	  (immutable uri     http-request-uri) ;; original request
	  (immutable headers http-request-headers)
	  ;; alist of query name and http-parameter object.
	  (immutable parameters http-request-parameters)
	  ;; cookies
	  (immutable cookies http-request-cookies)
	  (immutable socket  http-request-socket)
	  ;; raw POST?
	  (immutable source  http-request-source)
	  (immutable port    http-request-port) ;; host port
	  (immutable remote-addr http-request-remote-address)
	  (immutable remote-port http-request-remote-port)
	  (immutable server-context http-request-server-context)))
(define-record-type (<http-parameter> make-http-parameter http-parameter?)
  (fields (immutable name    http-parameter-name)
	  (immutable value   http-parameter-value)
	  ;; if it's mime
	  (immutable headers http-parameter-headers)))

(define (http-emit-response out request-headers status mime content headers)
  (define (get-content mime content gzip?)
    (define (do-compress content)
      (if gzip?
	  (let-values (((out extract) (open-bytevector-output-port)))
	    (let ((gout (open-gzip-output-port out)))
	      (put-bytevector gout content)
	      (close-output-port gout)
	      (extract)))
	  content))
    (case mime
      ((shtml) 
       ;; TODO UTF-8?
       (let ((bv (do-compress (string->utf8 (shtml->html content)))))
	 (values "text/html" (bytevector-length bv)
		 (open-bytevector-input-port bv)
		 gzip?)))
      ((sxml)
       (let ((bv (do-compress (string->utf8 (srl:sxml->xml content)))))
	 (values "text/html" (bytevector-length bv)
		 (open-bytevector-input-port bv)
		 gzip?)))
      ((file)
       ;; we don't want neither sending with chunk nor compressing
       ;; in case of huge file. so get size and just dump it
       ;; I don't care about network trafic for this!
       (let ((size (file-size-in-bytes content)))
	 (values "application/octet-stream" size
		 (open-file-input-port content (file-options no-fail))
		 #f)))
      ;; TODO 
      (else
       (cond ((string? content)
	      (let ((bv (do-compress (string->utf8 content))))
		(values mime (bytevector-length bv)
			(open-bytevector-input-port bv) gzip?)))
	     ((bytevector? content)
	      (let ((bv (do-compress content)))
		(values mime (bytevector-length bv)
			(open-bytevector-input-port bv) gzip?)))
	     ((and (binary-port? content) (input-port? content))
	      ;; size is unknown thus chunk
	      (values mime #f content gzip?))
	     (else
	      (error 'http-emit-response "unknown type" content))))))
  (define get-header rfc5322-header-ref)
  ;; slightly memory efficient...
  (define (gzipped-port ocontent)
    (define buf (binary:open-chunked-binary-input/output-port))
    (define gout (open-gzip-output-port buf))
    (copy-binary-port gout ocontent)
    (close-output-port gout)
    (set-port-position! buf 0)
    buf)
  (define (send-chunked-data out ocontent gzip?)
    (define size 1024)
    (define buf (make-bytevector size))
    (define content (if gzip?
			(gzipped-port ocontent)
			ocontent))
    (define (finish)
      (send-chunk out 0 #vu8())
      (flush-output-port out))
    (define (send-chunk out n buf)
      (put-bytevector out (string->utf8 (number->string n 16)))
      (put-bytevector out #*"\r\n")
      (put-bytevector out buf 0 n)
      (put-bytevector out #*"\r\n"))
    (let loop ((n (get-bytevector-n! content buf 0 size)))
      (cond ((eof-object? n) (finish))
	    ((< n size) 
	     (send-chunk out n buf)
	     (finish))
	    (else (send-chunk out n buf)
		  (loop (get-bytevector-n! content buf 0 size))))))
  (define gzip?
    (and-let* ((accept-encoding (get-header request-headers "accept-encoding")))
      (and accept-encoding (#/gzip/ accept-encoding))))
  
  (let-values (((mime size content compressed?)
		(get-content mime content gzip?)))
    (let ((content-type (or (get-header headers "content-type") mime))
	  (content-length (or (get-header headers "content-length") size))
	  (headers (remp (lambda (slot)
			   (or (string-ci=? (car slot) "content-length")
			       (string-ci=? (car slot) "content-type")
			       (string-ci=? (car slot) "server")))
			 headers)))
      (put-bytevector out #*"HTTP/1.1 ")
      (put-bytevector out (string->utf8 
			   (format "~a ~a\r\n" (car status) (cadr status))))
      (put-bytevector out (string->utf8
			   (format "Content-Type: ~a\r\n" content-type)))
      (when compressed?
	(put-bytevector out #*"Vary: Accept-Encoding \r\n")
	(put-bytevector out #*"Content-Encoding: gzip\r\n"))
      (if content-length
	  (put-bytevector out
			  (string->utf8
			   (format "Content-Length: ~a\r\n"
				   content-length)))
	  (put-bytevector out #*"Transfer-Encoding: chunked\r\n"))
      ;; server header
      (put-bytevector out (string->utf8
			   (format "Server: ~a\r\n" (*http-server-name*))))
      (for-each (lambda (slot)
		  (put-bytevector 
		   out 
		   (string->utf8
		    (format "~a: ~a\r\n" (car slot) (cadr slot)))))
		headers)
      (put-bytevector out #*"\r\n")
      (if content-length ;; we don't care if this is compressed or not
	  (copy-binary-port out content)
	  ;; passed content was binary input-port
	  ;; and we don't compress it ahead. so do it here
	  (send-chunked-data out content gzip?))
      (close-port content))))

;; TODO
(define (http-internal-server-error out e header?)
  (define (format-condition e)
    (let-values (((out extract) (open-string-output-port)))
      (report-error e out)
      (extract)))
  (define (format-header header?)
    (if header?
	(let-values (((out extract) (open-string-output-port)))
	  (pp header? out)
	  (extract))
	header?))

  (guard (e (else #f))
    (let* ((content 
	    (format "Server Error\r\nheaders:\r\n~a\r\ncondition:\r\n~a\r\n"
		    (format-header header?) (format-condition e)))
	   (bv (string->utf8 content)))
      (put-bytevector out #*"HTTP/1.1 500 Internal Server Error\r\n")
      (put-bytevector out #*"Content-Length: ")
      (put-bytevector out 
		      (string->utf8 (number->string (bytevector-length bv))))
      (put-bytevector out #*"\r\n")
      (put-bytevector out #*"Content-Type: text/plain\r\n\r\n")
      (put-bytevector out bv))))


;; From RFC 7231 section 6
;;  see https://tools.ietf.org/html/rfc7231#section-6
(define-constant +http-status-message+
  '((100 "Continue")
    (101 "Switching Protocols")
    (200 "OK")
    (201 "Created")
    (202 "Accepted")
    (203 "Non-Authoritative Information")
    (204 "No Content")
    (205 "Reset Content")
    (206 "Partial Content")
    (300 "Multiple Choices")
    (301 "Moved Permanently")
    (302 "Found")
    (303 "See Other")
    (304 "Not Modified")
    (305 "Use Proxy")
    (307 "Temporary Redirect")
    (400 "Bad Request")
    (401 "Unauthorized")
    (402 "Payment Required")
    (403 "Forbidden")
    (404 "Not Found")
    (405 "Method Not Allowed")
    (406 "Not Acceptable")
    (407 "Proxy Authentication Required")
    (408 "Request Timeout")
    (409 "Conflict")
    (410 "Gone")
    (411 "Length Required")
    (412 "Precondition Failed")
    (413 "Payload Too Large")
    (414 "URI Too Long")
    (415 "Unsupported Media Type")
    (416 "Range Not Satisfiable")
    (417 "Expectation Failed")
    (426 "Upgrade Required")
    (500 "Internal Server Error")
    (501 "Not Implemented")
    (502 "Bad Gateway")
    (503 "Service Unavailable")
    (504 "Gateway Timeout")
    (505 "HTTP Version Not Supported")))

(define (read-chunk in)
  (let-values (((out extract) (open-bytevector-output-port)))
    (let loop ((chunk-size #f))
      ;; remove CRLF of previous chunk
      (when chunk-size (binary:get-line in))
      (let ((line (binary:get-line in)))
	(when (eof-object? line)
	  (error 'read-chunk "chunk body ended prematurely"))
	(cond ((#/^([0-9a-fA-F]+)/ (utf8->string line)) =>
	       (lambda (m)
		 (let ((digits (string->number (m 1) 16)))
		   (if (zero? digits)
		       (do ((line (binary:get-line in) (binary:get-line in)))
			   ((or (eof-object? line)
				(zero? (bytevector-length line)))
			    (extract)))
		       (begin
			 (copy-binary-port out in :size digits)
			 (loop digits))))))
	      (else (error 'read-chunk "bad line in chunked data" line)))))))
    
(define (read-form-data headers in)
  (cond ((rfc5322-header-ref headers "content-length") =>
	 (lambda (v) (get-bytevector-n in (string->number v))))
	((equal? (rfc5322-header-ref headers "transfer-encoding") "chunked")
	 (read-chunk in))
	((not (port-ready? in)) #vu8())
	(else
	 ;; ok we can't do much
	 (let-values (((out extract) (open-bytevector-output-port)))
	   (let loop ((first #t))
	     (let ((u8 (get-u8 in)))
	       (if (eof-object? u8)
		   (if first u8 (extract))
		   (begin
		     (put-u8 out u8)
		     (loop #f)))))))))

(define (http-server-handler dispatcher)
  (define log-dir (*http-log-directory*))
  (define logger
    (and log-dir
	 (create-directory* log-dir)
	 (make-async-logger +info-level+
	  ;; TODO console log?
	  (make-daily-rolling-file-appender "~w5 ~m"
	   (build-path log-dir "access-log.log")))))
  (define (write-log method path ip)
    (and logger
	 (info-log logger (format "[~a] ~a ~a" ip method path))))
  (lambda (server socket)
    ;; we can't use buffered-port here since swtiching protocol
    ;; may require buffer values.
    (define raw-in (socket-input-port socket))    
    (define peer (socket-peer socket))
    (define (fixup-status status)
      (if (pair? status)
	  status
	  (cond ((assv status +http-status-message+))
		(else (list status "Unknown status")))))
    ;; lazy
    (define (mime-handler packet port)
      (get-bytevector-all port))

    (define (%uri-decode bv)
      (call-with-bytevector-output-port
       (lambda (out)
	 (uri-decode (open-bytevector-input-port bv) out
		     :cgi-decode #t))))
    (define (query-string->alist qs)
      (let ((kv-pairs (string-split qs "&")))
	(map (lambda (kv-pair)
	       (let ((r (string-split kv-pair "=")))
		 (if (null? (cdr r))
		     (cons (car r) (make-http-parameter (car r) #vu8() '()))
		     (cons (car r) 
			   (make-http-parameter
			    (car r)
			    (%uri-decode (string->utf8 (cadr r)))
			    '())))))
	     kv-pairs)))

    (define (parse-mime headers in)
      (define (get-name params)
	(let loop ((params params))
	  (cond ((null? params) #f)
		((and (pair? (car params)) (string=? "name" (caar params)))
		 (cdar params))
		(else (loop (cdr params))))))
      (define (get-names&contents body)
	(define (get-name&content mime)
	  (let* ((headers (mime-part-headers mime))
		 (field (rfc5322-header-ref headers "content-disposition"))
		 (params (mime-parse-content-disposition field))
		 (name (get-name params)))
	    (cons name
		  (make-http-parameter name (mime-part-content mime)
				       (mime-part-headers mime)))))
	(map get-name&content (mime-part-content body)))
      (or (and-let* ((content-type (rfc5322-header-ref headers "content-type"))
		     (parsed (mime-parse-content-type content-type)))
	    (or (and-let* (( (string=? (car parsed) "multipart") )
			   ;; should we check subpart?
			   ( (string=? (cadr parsed) "form-data") )
			   (body (mime-parse-message in headers mime-handler)))
		  (get-names&contents body))
		(and-let* (( (string=? (car parsed) "application") )
			   ( (string=? (cadr parsed) "x-www-form-urlencoded") )
			   (data (read-form-data headers in))
			   ( (not (eof-object? data)) ))
		  (query-string->alist (utf8->string data)))))
	  '()))
    (define (parse-path path)
      (let-values (((s ui h p path qs frag) (uri-parse path)))
	(values path (if qs (query-string->alist qs) '()) frag)))
    (define (lookup-handler method opath)
      (define (match? path patterns)
	(find (lambda (pattern) 
		(and ((caar pattern) path) (string=? method (cdar pattern))))
		patterns))
      (let ((table (http-dispatcher-table dispatcher))
	    (pattern (http-dispatcher-patterns dispatcher))
	    (path (http-make-path-entry method opath)))
	(cond ((hashtable-ref table path #f))
	      ((match? opath pattern) => cdr)
	      ((hashtable-ref table (http-make-path-entry method "*")
			      (*http-not-found-handler*))))))

    (define (parse-cookie headers)
      (append-map (lambda (header)
		    ;; we know the header name is downcased but
		    ;; use string-ci=?
		    (if (string-ci=? "cookie" (car header))
			(parse-cookies-string (cadr header))
			'())) headers))

    (define (finish in out)
      (flush-output-port out)
      ;; close the socket
      ;; if something is still there, discard it.
      (unless (null? (socket-read-select 0 socket))
	(get-bytevector-all in))
      (close-port in)
      (close-port out)
      (socket-shutdown socket SHUT_RDWR)
      (socket-close socket))

    ;; let the protocol handler handle everything
    ;; since it's not an HTTP anymore
    (define (invoke-protocol proc method path)
      ;; process must return
      (let ((r (proc socket method path)))
	(if (socket-closed? socket)
	    (hashtable-delete! (http-dispatcher-upgraded-sockets dispatcher)
			       socket)
	    (hashtable-set! (http-dispatcher-upgraded-sockets dispatcher)
			    socket r))))

    (define (invoke-handler dispatcher handler path http-request)
      (define (path-match? template&filter)
	(let ((template (car template&filter))
	      (filter (cdr template&filter)))
	  (cond ((string? template) (and (string=? path template) filter))
		((regex-pattern? template) (and (template path) filter))
		(else #f))))
      (define filters
	(filter-map path-match?
		    (list-queue-list (http-dispatcher-filters dispatcher))))
      (let ((invoke
	     (fold-right (lambda (filter acc) (lambda (req) (filter req acc)))
			 (lambda (req) (handler req))
			 filters)))
	(invoke http-request)))
    
    (define (handle-http method opath path qs frg)
      (define in (buffered-port raw-in (buffer-mode line)))
      (define out (buffered-port (socket-output-port socket)
				 (buffer-mode block)))
      (define headers (rfc5322-read-headers in))
      
      (let ((handler (lookup-handler method path))
	    (params (append qs (parse-mime headers in)))
	    (cookies (parse-cookie headers)))
	(guard (e ((uncaught-exception? e)
		   (http-internal-server-error out
		     (uncaught-exception-reason e) headers)
		   (finish in out))
		  (else
		   (http-internal-server-error out e headers)
		   (finish in out)))
	  (let-values (((status mime content . response-headers)
			;; TODO proper http request
			(invoke-handler dispatcher handler path
					(make-http-request
					 method path opath 
					 headers params cookies 
					 socket in
					 (slot-ref server 'port)
					 (ip-address->string 
					  (slot-ref peer 'ip-address))
					 (slot-ref peer 'port)
					 (server-context server)))))
	    (when (and status (not (eq? mime 'none)))
	      (http-emit-response out headers
				  (fixup-status status)
				  mime content
				  response-headers))
	    (finish in out)))))
    (define (handle-unexpected)
      (define in (buffered-port raw-in (buffer-mode line)))
      (define out (buffered-port (socket-output-port socket)
				 (buffer-mode block)))
      
      (let ((data (get-bytevector-all in)))
	(http-internal-server-error out #f
	  (if (eof-object? data) "no data" (utf8->string data)))
	(finish in out)))
    
    (define upgraded-sockets (http-dispatcher-upgraded-sockets dispatcher))

    (cond ((hashtable-ref upgraded-sockets socket) =>
	   (lambda (next)
	     (next socket)
	     (when (socket-closed? socket)
	       (hashtable-delete! (http-dispatcher-upgraded-sockets dispatcher)
				  socket))))
	  (else
	   (let ((first (binary:get-line raw-in)))
	     (cond ((eof-object? first)) ;; why
		   ((#/(\w+)\s+([^\s]+)\s+HTTP\/([\d\.]+)/ first) =>
		    (lambda (m)
		      (let ((method (utf8->string (m 1)))
			    (opath  (utf8->string (m 2)))
			    (prot   (m 3))
			    (protocols (http-dispatcher-protocol-handlers
					dispatcher))
			    (ip-address (ip-address->string 
					 (slot-ref peer 'ip-address))))
			(write-log method opath ip-address)
			(let-values (((path qs frg) (parse-path opath)))
			  (cond ((hashtable-ref protocols path) =>
				 (lambda (proc)
				   (invoke-protocol proc method opath)))
				(else
				 (handle-http method opath path qs frg)))))))
		   (else (handle-unexpected))))))))

(define (http-file-handler file mime)
  (lambda (req)
    (let ((size (file-size-in-bytes file)))
      (values 200 'file file
	      (list "content-length" size)
	      (list "content-type" mime)))))

;; for convenience
(define (http-registered-path-handler req path-map)
  (values 200 'shtml
	  `(html
	    (head (title "Registered Paths"))
	    (body
	     (ul
	      ,@(map (lambda (path)
		       (if (string=? "GET" (car path))
			   `(li (a (@ (href ,(cadr path)))
				    ,(string-append "GET:" (cadr path))))
			   `(li ,(apply http-make-path-entry path))))
		     path-map))))))

)
