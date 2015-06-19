;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; tapas/base.scm - Base framework for tapas
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

(library (tapas base)
    (export &tapas tapas-error? make-tapas-error ;; conditions
	    ;; base components
	    tapas-render-component
	    <tapas-component>
	    <tapas-container>
	    <tapas-page>

	    tapas-set-attribute!
	    tapas-add-components!

	    tapas-page-add-headers!
	    ;; TODO remove attribute

	    ;; utilities
	    tapas-find-component
	    tapas-visit-component
	    )
    (import (rnrs)
	    (rnrs mutable-pairs)
	    (tapas handler)
	    (clos user)
	    (sagittarius)
	    (sagittarius object)
	    (text sxml tools)
	    (srfi :26 cut))

(define-condition-type &tapas &error
  make-tapas-error tapas-error?)

(define-class <tapas-component> ()
  ((id :init-keyword :id :init-value #f)
   ;; put initial value as invalid sxml tag name
   ;; so that extended component can have options where
   ;; they want to put tag name.
   (tag-name :init-keyword :tag-name :init-value #f) ;; tag name
   (class :init-keyword :class :init-value #f)
   (attributes :init-keyword :attributes :init-value '()) ;; alist
   (content :init-keyword :content :init-value #f)))

(define-class <tapas-container> (<tapas-component>)
  ((components :init-keyword :components :init-value '())))
(define-method initialize ((o <tapas-container>) initargs)
  (call-next-method)
  (unless (~ o 'tag-name) (set! (~ o 'tag-name) 'div)) ;; default div
  o)

(define-class <tapas-page> (<tapas-container>)
  ((headers :init-keyword :headers :init-value '())))
(define-method initialize ((o <tapas-page>) initargs)
  (call-next-method)
  (set! (~ o 'id) #f)
  (set! (~ o 'tag-name) 'html)
  o)

;; in SXML, text element is the same as string
(define-method tapas-render-component ((text <string>)) text)
;; raw SXML
(define-method tapas-render-component ((sxml <pair>)) sxml)

(define (safe-set-attr! elem component slot)
  (when (and (slot-bound? component slot) (~ component slot))
    (sxml:set-attr! elem (list slot (~ component slot)))))

(define-method tapas-render-component ((comp <tapas-component>))
  (let ((r `(,(~ comp 'tag-name) (@ ,@(~ comp 'attributes))
	     ,@(or (and-let* ((content (~ comp 'content)))
		     ;; if this is a string, then just return
		     ;; this makes a bit convenient.
		     (if (string? content)
			 (list content)
			 (tapas-render-component (~ comp 'content))))
		   '()))))
    (safe-set-attr! r comp 'id)
    (safe-set-attr! r comp 'class)
    r))

(define-method tapas-render-component ((comp <tapas-container>))
  (let ((this (call-next-method)))
    (sxml:change-content! this 
			  (map tapas-render-component (~ comp 'components)))
    this))

(define-method tapas-render-component ((comp <tapas-page>))
  (let* ((this (call-next-method))
	 (content (sxml:content this))
	 (headers (map tapas-render-component (~ comp 'headers))))
    (sxml:change-content! this `((head ,@headers) (body ,@content)))
    this))

;; convenient method
(define (tapas-set-attribute! component name value)
  ;; this replace attr
  (let ((attr (~ component 'attributes)))
    (cond ((assq name attr) => (lambda (slot) (set-cdr! slot (list value))))
	  (else (set! (~ component 'attributes) 
		      (acons name (list value) attr))))
    component))

(define (tapas-add-components! container . components)
  (let ((comps (~ container 'components)))
    (set! (~ container 'components) (append! comps components))
    container))

(define (tapas-page-add-headers! page . headers)
  ;; TODO check header components
  (let ((hdrs (~ page 'headers)))
    (set! (~ page 'headers) (append! hdrs headers))
    page))

(define (tapas-find-component proc component)
  (call/cc (lambda (c)
	     (tapas-visit-component component
				    (lambda (comp)
				      (when (proc comp)
					(c comp))))
	     #f)))

;; string is text element
(define-method tapas-visit-component ((o <string>) proc)
  (proc o))

(define-method tapas-visit-component ((o <tapas-component>) proc)
  (proc o))

(define-method tapas-visit-component ((o <tapas-container>) proc)
  (proc o)
  (for-each (cut tapas-visit-component <> proc) (~ o 'components)))

(define-method tapas-visit-component ((o <tapas-page>) proc)
  (proc o)
  (for-each (cut tapas-visit-component <> proc) (~ o 'components))
  (for-each (cut tapas-visit-component <> proc) (~ o 'headers)))


  )
