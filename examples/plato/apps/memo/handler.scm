;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; Simple memo app
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
#!read-macro=sagittarius/bv-string
(library (plato webapp memo)
  (export entry-point support-methods mount-paths)
  (import (rnrs)
	  (tapas)
	  (paella)
	  (plato)
	  (util file)
	  (text markdown)
	  (sagittarius)
	  (sagittarius regex)
	  (text json))

  (define-constant +memos+ "memos")
  (define (generate-file-name name)
    (let ((s (string-downcase name)))
      (regex-replace-all #/[^a-zA-Z0-9_-]/ s "_")))

  (define (meta-file dir) (build-path dir "memo-lists.scm"))

  (define (save-memo dir name tags content)
    (define meta (meta-file dir))
    (let* ((name (utf8->string (http-parameter-value name)))
	   (tags (utf8->string (http-parameter-value tags)))
	   (content (http-parameter-value content))
	   (meta-info (if (file-exists? meta)
			  (filter (lambda (m)
				    (not (string=? name (car m))))
				  (call-with-input-file meta read))
			  '())))
      (let ((filename (build-path +memos+ (generate-file-name name))))
	(when (file-exists? filename) (delete-file filename))
	(call-with-output-file filename
	  (lambda (out)
	    (put-bytevector out content))
	  :transcoder #f)
	(let ((out (open-file-output-port meta (file-options no-fail)
					  (buffer-mode block)
					  (native-transcoder))))
	  (write (cons (list name tags filename) meta-info) out)
	  (close-output-port out)))))

  (define (memo-handler req)
    (define page (call-with-input-file "loader.html" html->tapas-component))
    (define context (*plato-current-context*))

    (let ((param (http-request-parameters req))
	  (dir (build-path (plato-current-path context) +memos+)))
      (and-let* ((name (assoc "name" param))
		 (tags (assoc "tags" param))
		 (content (assoc "content" param)))
	(unless (file-exists? dir) (create-directory* dir))
	(save-memo dir (cdr name) (cdr tags) (cdr content))))
    page)

  ;; for ajax
  (define (list-memo-handler req)
    (define context (plato-parent-context (*plato-current-context*)))
    (define meta (meta-file (build-path (plato-current-path context) +memos+)))
    (define meta-info (if (file-exists? meta)
			  (call-with-input-file meta read)
			  '()))

    (define (meta->json meta)
      (vector (cons "title" (car meta))
	      (cons "file" (caddr meta))))
    ;; dummy
    (values 200 'application/json
	    (call-with-string-output-port
	     (lambda (out)
	       (json-write (map meta->json meta-info) out)))))

  (define (script-loader req)
    (define context (plato-parent-context (*plato-current-context*)))
    (let ((uri (http-request-uri req)))
      (let-values (((dir file ext) (decompose-path uri)))
	(let ((in (open-file-input-port 
		   (build-path* (plato-current-path context)
				(string-append file "." ext))
		   (file-options no-fail)
		   (buffer-mode block))))
	  (values 200 'text/javascript in)))))

  ;; /memo/load
  (define (load-memo-handler req)
    (define context (plato-parent-context (*plato-current-context*)))
    (define dir (plato-current-path context))
    (define meta (meta-file (build-path dir +memos+)))
    (define meta-info (call-with-input-file meta read))
    (define params (http-request-parameters req))

    (or (and-let* ((pname (assoc "name" params))
		   (p (utf8->string (http-parameter-value (cdr pname))))
		   (info (assoc p meta-info string=?))
		   (name (car info))
		   (tags (cadr info))
		   (file (caddr info)))
	  (let ((markdown? (cond ((assoc "markdown" params) =>
				  (lambda (s)
				    (bytevector=? (http-parameter-value(cdr s))
						  #*"true")))
				 (else #f))))
	    (values 200 
		    'application/json
		    (call-with-string-output-port
		     (lambda (out)
		       (json-write
			`#(("name" . ,name)
			   ("tags" . ,tags)
			   ("content" . 
			    ,(call-with-input-file (build-path dir file)
			       (lambda (in) 
				 (if markdown?
				     (markdown-read in :as 'html)
				     (get-string-all in))))))
			out))))))
	(values 404 'text/plain "Not found")))

  (define (remove-memo-handler req)
    (define context (plato-parent-context (*plato-current-context*)))
    (define current-path (plato-current-path context))
    (define dir (build-path current-path +memos+))
    (define mfile (meta-file dir))
    (define meta-info (call-with-input-file mfile read))

    (define (try-remove m p)
      (and-let* (( (string=? (car m) p))
		 (file (caddr m)))
	(delete-file (build-path current-path file))))

    (or (and-let* ((title (assoc "title" (http-request-parameters req)))
		   (p (utf8->string (http-parameter-value (cdr title))))
		   (new-meta (filter (lambda (m) (not (try-remove m p)))
				     meta-info))
		   (out (open-file-output-port mfile (file-options no-fail)
					       (buffer-mode block)
					       (native-transcoder))))
	  (write new-meta out)
	  (close-output-port out)
	  (values 200 'text/plain (format "~a is removed" p)))
	(values 404 'text/plain "Not found")))

  (define (mount-paths)
    `( ((GET) "/list" ,list-memo-handler)
       ((GET) #/scripts/ ,script-loader)
       ((GET) "/load" ,load-memo-handler)
       ((GET) "/remove" ,remove-memo-handler)))

  (define (support-methods) '(GET POST))

  (define entry-point (tapas-request-handler memo-handler))
)
