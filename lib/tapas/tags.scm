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

#!read-macro=sagittarius/regex
(library (tapas tags)
    (export <tapas-form>
	    <tapas-input>
	    <tapas-textarea>
	    <tapas-select>
	    <tapas-option>

	    <tapas-link>
	    <tapas-br>

	    <tapas-script>
	    ;; utilities
	    tapas:br
	    make-tapas-simple-tag

	    &tapas-tag tapas-tag-error? tapas-tag-element
	    )
    (import (rnrs)
	    (clos user)
	    (clos core)
	    (sagittarius)
	    (sagittarius regex)
	    (sagittarius object)
	    (tapas handler)
	    (tapas base)
	    (text sxml tools))

(define-condition-type &tapas-tag &tapas
  make-tapas-tag-error tapas-tag-error?
  (element tapas-tag-element))

(define (tapas-tag-error elem who msg . irr)
  (raise (condition
	  (make-tapas-tag-error elem)
	  (make-who-condition (or who (class-name (class-of elem))))
	  (make-message-condition msg)
	  (make-irritants-condition irr))))

;; the same as (tapas base)
(define (safe-set-attr! elem component slot)
  (when (and (slot-bound? component slot) (~ component slot))
    (sxml:set-attr! elem (list slot (~ component slot)))))

;;; form
(define-class <tapas-form> (<tapas-container>) 
  ((method :init-keyword :method :init-value "GET")
   (action :init-keyword :action :init-value #f)
   (enctype :init-keyword :enctype :init-value #f)))

(define-method tapas-render-component ((form <tapas-form>))
  (unless (and (slot-bound? form 'action) (~ form 'action))
    (tapas-tag-error form #f "action is mandatory" 'action))
  (let ((this (call-next-method)))
    (sxml:change-name! this 'form)
    (sxml:set-attr! this (list 'action (~ form 'action)))
    (sxml:set-attr! this (list 'method (~ form 'method)))
    (safe-set-attr! this form 'enctype)
    this))

(define-class <tapas-input> (<tapas-component>) 
  ((type :init-keyword :type :init-value "text")
   (name :init-keyword :name :init-value #f)
   (value :init-keyword :value :init-value #f)))

(define-method tapas-render-component ((form <tapas-input>))
  (let ((this (call-next-method)))
    (sxml:change-name! this 'input)
    (safe-set-attr! this form 'type)
    (safe-set-attr! this form 'name)
    (safe-set-attr! this form 'value)
    this))

(define-class <tapas-textarea> (<tapas-input>) 
  ((rows :init-keyword :rows)
   (cols :init-keyword :cols)))

(define-method tapas-render-component ((form <tapas-textarea>))
  (let ((this (call-next-method)))
    (sxml:change-name! this 'textarea)
    (safe-set-attr! this form 'rows)
    (safe-set-attr! this form 'cols)
    ;; remove type
    (sxml:change-attrlist! this
     (remp (lambda (slot)
	     (eq? (car slot) 'type)) (sxml:attr-list this)))
    this))

;; TODO should this be a subclass of <tapas-input>?
(define-class <tapas-select> (<tapas-container>) 
  ((name :init-keyword :name :init-value #f)))
(define-method tapas-render-component ((s <tapas-select>))
  (let ((this (call-next-method)))
    (sxml:change-name! this 'select)
    (safe-set-attr! this s 'name)
    this))

(define-class <tapas-option> (<tapas-input>) 
  ((selected :init-keyword :selected :init-value #f)))
(define-method tapas-render-component ((o <tapas-option>))
  (let ((this (call-next-method)))
    (sxml:change-name! this 'option)
    (when (~ o 'selected) (sxml:set-attr! this (list 'selected "true")))
    this))

;;; link
(define-class <tapas-link> (<tapas-component>) 
  ((href :init-keyword :href :init-value "javascript:void(0)")))

(define-method tapas-render-component ((link <tapas-link>))
  (let ((this (call-next-method)))
    (sxml:change-name! this 'a)
    (safe-set-attr! this link 'href)
    this))

;;; br
(define-class <tapas-br> (<tapas-component>) ())
(define-method initialize ((o <tapas-br>) initargs)
  (call-next-method)
  (set! (~ o 'tag-name) 'br)
  o)

(define-class <tapas-script> (<tapas-component>)
  ((type :init-keyword :type :init-value #f)
   (src :init-keyword :src :init-value #f)
   (charset :init-keyword :charset :init-value #f)
   (async :init-keyword :async :init-value #f)
   (defer :init-keyword :defer :init-value #f)))
  
(define-method tapas-render-component ((s <tapas-script>))
  (let ((this (call-next-method)))
    (sxml:change-name! this 'script)
    (safe-set-attr! this s 'type)
    (safe-set-attr! this s 'src)
    (safe-set-attr! this s 'charset)
    (safe-set-attr! this s 'async)
    (safe-set-attr! this s 'defer)
    ;; wrap content with *COMMENT* to avoid escaping < >
    ;; this must be after set-attr! to avoid sxml:content
    ;; NB: sxml:content doesn't return *COMMENT* element 
    ;;     means if the result content is modified by that,
    ;;     then it'd get screwed.
    (and-let* ((content (sxml:content this))
	       ( (not (null? content)) )
	       ( (not (eq? (car content) '*COMMENT*)) ))
      ;; we assume all elements are already strings
      (let* ((c (string-concatenate content))
	     (new-c (regex-replace-all #/-->/
				       (regex-replace-all #/<!--/ c "")
				       "")))
	(sxml:change-content! this (list (list '*COMMENT* new-c)))))
    this))

;;; utility
(define tapas:br (make <tapas-br>))

(define (make-tapas-simple-tag tag-name :optional (content #f))
  (make <tapas-component> :tag-name tag-name :content content))
)
