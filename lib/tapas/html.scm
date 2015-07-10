;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; tapas/html.scm - HTML utility for tapas
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

(library (tapas html)
    (export html->tapas-component
	    shtml->tapas-component

	    *tapas-html-context*)
    (import (rnrs)
	    (tapas base)
	    (tapas tags)
	    (clos core)
	    (sagittarius object)
	    (text sxml html-parser)
	    (text sxml tools)
	    (text sxml sxpath)
	    (srfi :39 parameters))

(define (html->tapas-component input . opt)
  (let ((shtml (html->shtml input)))
    (apply shtml->tapas-component shtml opt)))

(define (shtml->tapas-component shtml :optional (context *tapas-html-context*))
  (define (strip-top shtml)
    (if (eq? (car shtml) shtml-top-symbol)
	(cadr shtml)
	shtml))
  (define ctx (context))
  ;; this is pain in the ass task
  ;; we need to traverse the shtml tree.
  (define (traverse shtml)
    (define (set-attr obj class attr)
      (let loop ((attr attr) (rest '()))
	(cond ((null? attr) (set! (~ obj 'attributes) rest))
	      ((slot-exists-using-class? class obj (caar attr))
	       (set! (~ obj (caar attr)) (cadar attr))
	       (loop (cdr attr) rest))
	      (else
	       (loop (cdr attr) (cons (car attr) rest))))))
    (if (string? shtml)
	shtml ;; text element
	(let ((tag (sxml:name shtml))
	      (attr (sxml:attr-list shtml))
	      (content (sxml:content shtml)))
	  (cond ((hashtable-ref ctx tag #f) =>
		 (lambda (class/procedure)
		   (if (procedure? class/procedure)
		       ;; let must return component
		       (let ((obj (class/procedure traverse shtml)))
			 (set-attr obj (class-of obj) attr)
			 obj)
		       (let ((obj (make class/procedure)))
			 (set! (~ obj 'tag-name) tag)
			 (if (is-a? obj <tapas-container>)
			     (set! (~ obj 'components) (map traverse content))
			     ;; put raw SXML for component
			     (unless (null? content)
			       (set! (~ obj 'content) content)))
			 (set-attr obj class/procedure attr)
			 obj))))
		;; TODO should we handle known tags?
		(else
		 (if (or (null? content) (null? (cdr content)))
		     (let ((obj (make <tapas-component>)))
		       (set! (~ obj 'tag-name) tag)
		       (unless (null? content)
			 (set! (~ obj 'content) content))
		       (set-attr obj <tapas-component> attr)
		       obj)
		     (let ((obj (make <tapas-container>)))
		       (set! (~ obj 'tag-name) tag)
		       (set! (~ obj 'components) (map traverse content))
		       (set-attr obj <tapas-container> attr)
		       obj)))))))
  (traverse (strip-top shtml)))

(define (page-converter traverse shtml)
  (define (handle-header obj shtml)
    (let ((header ((if-car-sxpath "/html/head") shtml))
	  (body ((if-car-sxpath "/html/body") shtml)))
      ;; head tag shouldn't have any attribute so ignore.
      (when header (set! (~ obj 'headers) (map traverse (sxml:content header))))
      (if body
	  (begin
	    (set! (~ obj 'body-attributes) (sxml:attr-list body))
	    (sxml:content body))
	  '())))
  (let ((tag (sxml:name shtml))
	(attr (sxml:attr-list shtml))
	(obj (make <tapas-page>)))
    ;; we know this doesn't have *TOP*
    (let ((body (handle-header obj (list shtml-top-symbol shtml))))
      (set! (~ obj 'components) (map traverse body))
      obj)))

(define default-context
  (let ((ht (make-eq-hashtable)))
    (hashtable-set! ht 'div <tapas-container>)
    (hashtable-set! ht 'form <tapas-form>)
    (hashtable-set! ht 'input <tapas-input>)
    (hashtable-set! ht 'textarea <tapas-textarea>)
    (hashtable-set! ht 'a <tapas-link>)
    (hashtable-set! ht 'br <tapas-br>)
    (hashtable-set! ht 'select <tapas-select>)
    (hashtable-set! ht 'option <tapas-option>)
    (hashtable-set! ht 'script <tapas-script>)
    (hashtable-set! ht 'html page-converter)
    ht))

(define *tapas-html-context* 
  (make-parameter default-context
		  (lambda (x)
		    (if (hashtable? x)
			x
			(assertion-violation '*tapas-html-context*
					     "must be a hashtable")))))

)
