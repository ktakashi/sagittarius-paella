;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; cuberteria.scm - cutler for plato
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

(library (cuberteria)
    (export cuberteria-resource-loader
	    cuberteria-map-http-request!
	    cuberteria-map-json-string!
	    cuberteria-map-json-object!
	    cuberteria-map-object!
	    cuberteria-object->json
	    ;; for convenience
	    <converter-meta> <converter-mixin>
	    )
    (import (rnrs)
	    (paella)
	    (plato)
	    (util file)
	    (rfc uri)
	    (clos core)
	    (clos user)
	    (text json)
	    (srfi :1 lists)
	    (srfi :39 parameters))

  (define (cuberteria-resource-loader mime base)
    (lambda (req)
      (define context (plato-parent-context (*plato-current-context*)))
      (let ((uri (http-request-uri req)))
	(let-values (((dir file ext) (decompose-path uri)))
	  (let ((in (open-file-input-port 
		     (build-path* (plato-current-path context)
				  base
				  (string-append file "." ext))
		     (file-options no-fail)
		     (buffer-mode block))))
	    (values 200 mime in))))))

  ;; <validator-mixin> can also be used but this is more convenient if
  ;; only conversion is needed.
  (define-class <converter-meta> (<class>) ())
  (define-method compute-getter-and-setter ((class <converter-meta>) slot)
    (let ((r    (call-next-method))
	  (conv  (slot-definition-option slot :converter #f))
	  (name (slot-definition-name slot)))
      (if conv
	  (let ((setter (or (cadr r)
			    (lambda (o v)
			      (slot-set-using-class! class o name v)))))
	    (list (car r)
		  (lambda (o v)
			(setter o (conv v)))
		  (caddr r)))
	  r)))
  (define-class <converter-mixin> () () :metaclass <converter-meta>)

  ;; this may be convenient with combination of <validator-mixin>
  (define (cuberteria-map-http-request! obj request)
    (define params (http-request-parameters request))
    (cuberteria-map-object! obj params 
			    (lambda (p) (string->symbol (car p)))
			    (lambda (s p) (http-parameter-value (cdr p)))))
  
  ;; if you are using JSON then this may be convenient
  (define (cuberteria-map-json-string! obj json-string)
    (define (string->json json-string)
      (parameterize ((*json-map-type* 'alist))
	(json-read (open-string-input-port json-string))))
    (let ((json (string->json json-string)))
      (cuberteria-map-json-object! obj json)))

  ;; TODO handling *json-map-type*
  (define (cuberteria-map-json-object! obj json)
    (define slot-definitions (class-slots (class-of obj)))
    (define (find-slot-definition slot)
      (let loop ((def slot-definitions))
	(cond ((null? def) #f)
	      ((eq? slot (slot-definition-name (car def))) (car def))
	      (else (loop (cdr def))))))

    (define (->json-slot param)
      (let ((slot (car param)))
	(let loop ((defs slot-definitions))
	  (cond ((null? defs) (string->symbol slot))
		((string=? slot
			   (slot-definition-option (car defs) 
						   :json-element-name ""))
		 (slot-definition-name (car defs)))
		(else (loop (cdr defs)))))))

    (define (->json-value slot param)
      (let ((v (cdr param))
	    (s (find-slot-definition slot)))
	(cond ((slot-definition-option s :json #f) =>
	       (lambda (class)
		 ;; :json must have <class> as its value
		 (if (is-a? class <class>)
		     (let ((obj (make class)))
		       (cuberteria-map-json-object! obj v))
		     v)))
	      (else v))))
    (cuberteria-map-object! obj json ->json-slot ->json-value))

  ;; general mapper
  (define (cuberteria-map-object! obj source slot-retriever value-retriever)
    (for-each (lambda (param)
		(let ((maybe-slot (slot-retriever param)))
		  (when (slot-exists? obj maybe-slot)
		    (slot-set! obj maybe-slot 
			       (value-retriever maybe-slot param)))))
	      source)
    obj)

  (define (cuberteria-object->json obj :optional (map-type (*json-map-type*)))
    (define (find-name slot)
      (cond ((slot-definition-option slot :json-element-name #f))
	    (else (symbol->string (slot-definition-name slot)))))
    (define (find-converter slot)
      (cond ((slot-definition-option slot :->json #f))
	    (else values)))
    (define (convert-rec obj handle-array)
      (filter-map (lambda (slot)
		    (let ((slot-name (slot-definition-name slot))
			  (json-name (find-name slot))
			  (conv (find-converter slot))
			  ;; if slot definition :json then the value
			  ;; is recursively converted
			  (json? (slot-definition-option slot :json #f)))
		      (and (slot-bound? obj slot-name)
			   (cons json-name 
				 (handle-array
				  (if json?
				      (cuberteria-object->json
				       (slot-ref obj slot-name) map-type)
				      (conv (slot-ref obj slot-name))))))))
		  (class-slots (class-of obj))))
    (case map-type
      ((alist) (convert-rec obj values))
      ((vector)
       (list->vector (convert-rec obj vector->list)))))
	

)
