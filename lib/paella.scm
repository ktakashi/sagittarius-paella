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

	    ;; records
	    ;; request
	    http-request?
	    http-request-method
	    http-request-path
	    http-request-uri
	    http-request-headers
	    http-request-parameters
	    http-request-cookies
	    http-request-source
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
	    )
    (import (rnrs)
	    (net server)
	    (rfc mime) 
	    (rfc :5322)
	    (rfc uri)
	    (rfc cookie)
	    (rfc gzip)
	    (sagittarius)
	    (sagittarius regex)
	    (sagittarius socket)
	    (sagittarius conditions)
	    (srfi :39 parameters)
	    (util port)
	    (prefix (binary io) binary:) 
	    (text sxml serializer)
	    (text sxml html-parser)
	    (pp))

(define (default-not-found-handler req)
  (values 404 'text/plain "Not Found"))

(define *http-not-found-handler* (make-parameter default-not-found-handler))
(define *http-server-name* (make-parameter "Sagittarius Paella"))

(define (http-make-path-entry method path)
  (string-append method ":" path))

(define (make-http-server-config . opt)
  (apply make-server-config :non-blocking? #t opt))

(define (http-mapped-path->alist dispatcher)
  (map (lambda (p)
	 (string-split p ":"))
       (hashtable-keys-list dispatcher)))

(define-syntax make-http-server-dispatcher
  (syntax-rules (*)
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
     (let ((r (make-string-hashtable)))
       (make-http-server-dispatcher "dispatch" r (specs ...))
       r))))

(define (http-add-dispatcher! dispacher method path handler)
  (hashtable-set! dispacher (http-make-path-entry (symbol->string method) path)
		  handler))

(define-record-type (<http-request> make-http-request http-request?)
  (fields (immutable method  http-request-method)
	  (immutable path    http-request-path)
	  (immutable uri     http-request-uri) ;; original request
	  (immutable headers http-request-headers)
	  ;; alist of query name and http-parameter object.
	  (immutable parameters http-request-parameters)
	  ;; cookies
	  (immutable cookies http-request-cookies)
	  ;; raw POST?
	  (immutable source  http-request-source)))
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
      (and accept-encoding (#/gzip.+?deflate/ accept-encoding))))
  
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
      (report-error e out) ;; very first one
      (when (stack-trace-condition? e)
	(let loop ((e (condition-cause e)))
	  (display "Nested " out)
	  (format-stack-trace (condition-stack-trace e) out)
	  (when (and (stack-trace-condition? e) (condition-cause e))
	    (loop (condition-cause e)))))
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

(define (http-server-handler dispatcher)
  (lambda (server socket)
    (define in (socket-input-port socket))
    (define out (socket-output-port socket))
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
			   (data (get-bytevector-all in))
			   ( (not (eof-object? data)) ))
		  (query-string->alist (utf8->string data)))))
	  '()))
    (define (parse-path path)
      (let-values (((s ui h p path qs frag) (uri-parse path)))
	(values path (if qs (query-string->alist qs) '()) frag)))
    (define (lookup-handler method path)
      (let ((path (http-make-path-entry method path)))
	(cond ((hashtable-ref dispatcher path #f))
	      ((hashtable-ref dispatcher (http-make-path-entry method "*")
			      (*http-not-found-handler*))))))

    (define (parse-cookie headers)
      (fold-left (lambda (acc header)
		   ;; we know the header name is downcased but
		   ;; use string-ci=?
		   (if (string-ci=? "cookie" (car header))
		       (cons (parse-cookie-string (cadr header)) acc)
		       acc)) '() headers))

    (let ((first (binary:get-line in)))
      (cond ((#/(\w+)\s+([^\s]+)\s+HTTP\/([\d\.]+)/ first) =>
	     (lambda (m)
	       (let* ((method (utf8->string (m 1)))
		      (opath   (utf8->string (m 2)))
		      (prot   (m 3))
		      (headers (rfc5322-read-headers in)))
		 (let-values (((path qs frg) (parse-path opath)))
		   (let ((handler (lookup-handler method path))
			 (params (append qs (parse-mime headers in)))
			 (cookies (parse-cookie headers)))
		     (guard (e (else
				(http-internal-server-error out e headers)))
		       (let-values (((status mime content . response-headers)
				     ;; TODO proper http request
				     (handler (make-http-request
					       method path opath 
					       headers params cookies in))))
			 (http-emit-response out headers
					     (fixup-status status)
					     mime content
					     response-headers))))))))
	    (else
	     (let ((data (get-bytevector-all in)))
	       (http-internal-server-error out #f
		(if (eof-object? data) "no data" (utf8->string data)))))))
    ;; close the socket
    ;; if something is still there, discard it.
    (unless (null? (socket-read-select 0 socket))
      (get-bytevector-all in))
    (socket-close socket)))


(define (http-file-handler file mime)
  (lambda (req)
    (let ((size (file-size-in-bytes file)))
      (values 200 mime (open-file-input-port file (file-options no-fail))
	      (list "content-length" size)))))

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
