;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; tapas/tags.scm - Basic tag components for tapas
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

(library (tapas tags)
    (export <tapas-form>
	    <tapas-input>
	    <tapas-textarea>

	    <tapas-link>
	    <tapas-br>
	    ;; utilities
	    tapas:br
	    make-tapas-simple-tag)
    (import (rnrs)
	    (clos user)
	    (sagittarius object)
	    (tapas handler)
	    (tapas base)
	    (text sxml tools))

;;; form
(define-class <tapas-form> (<tapas-container>) 
  ((method :init-keyword :method :init-value "GET")
   (action :init-keyword :action :init-value #f)))

(define-method tapas-render-component ((form <tapas-form>))
  (let ((this (call-next-method)))
    (sxml:change-name! this 'form)
    (sxml:set-attr! this (list 'action (~ form 'action)))
    (sxml:set-attr! this (list 'method (~ form 'method)))
    this))

(define-class <tapas-input> (<tapas-component>) 
  ((type :init-keyword :type :init-value "text")
   (name :init-keyword :name :init-value #f)
   (value :init-keyword :value :init-value #f)))

(define-method tapas-render-component ((form <tapas-input>))
  (let ((this (call-next-method)))
    (sxml:change-name! this 'input)
    (sxml:set-attr! this (list 'type (~ form 'type)))
    (when (~ form 'name)
      (sxml:set-attr! this (list 'name (~ form 'name))))
    (when (~ form 'value)
      (sxml:set-attr! this (list 'value (~ form 'value))))
    this))

(define-class <tapas-textarea> (<tapas-input>) 
  ((rows :init-keyword :rows)
   (cols :init-keyword :cols)))

(define-method tapas-render-component ((form <tapas-textarea>))
  (let ((this (call-next-method)))
    (sxml:change-name! this 'textarea)
    (when (slot-bound? form 'rows)
      (sxml:add-attr! this (list 'rows (~ form 'rows))))
    (when (slot-bound? form 'cols)
      (sxml:add-attr! this (list 'cols (~ form 'cols))))
    ;; remove type
    (sxml:change-attrlist! this
     (remp (lambda (slot)
	     (eq? (car slot) 'type)) (sxml:attr-list this)))
    this))


;;; link
(define-class <tapas-link> (<tapas-component>) 
  ((href :init-keyword :href :init-value "javascript:void(0)")))

(define-method tapas-render-component ((link <tapas-link>))
  (let ((this (call-next-method)))
    (sxml:change-name! this 'a)
    (sxml:set-attr! this (list 'href (~ link 'href)))
    this))

;;; br
(define-class <tapas-br> (<tapas-component>) ())
(define-method initialize ((o <tapas-br>) initargs)
  (call-next-method)
  (set! (~ o 'tag-name) 'br)
  o)

;;; utility
(define tapas:br (make <tapas-br>))

(define (make-tapas-simple-tag tag-name :optional (content #f))
  (make <tapas-component> :tag-name tag-name :content content))
)
