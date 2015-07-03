;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; plato/tools.scm - Tools for plato
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

;; this doesn't have to be a library
(library (plato tools)
    (export plato-skelton
	    plato-add-webapp)
    (import (rnrs)
	    (sagittarius)
	    (util file)
	    (plato invoke)
	    (pp))

(define (plato-skelton root)
  (create-directory* root)
  (create-directory* (build-path root +plato-lib-dir+))
  (create-directory* (build-path root +plato-app-dir+))
  (let ((file (build-path root "run.scm")))
    (unless (file-exists? file)
      (call-with-output-file file
	(lambda (out)
	  (display ";; -*- mode:scheme; coding:utf-8 -*-" out) (newline out)
	  (display '(import (rnrs) (paella) (plato)) out) (newline out)
	  (newline out)
	  
	  (display ";; server config" out) (newline out)
	  (pp '(define config (make-http-server-config :max-thread 10)) out)
	  (pp `(invoke-plato ,(absolute-path root) "8080" config) out))))))

(define (plato-add-webapp root name)
  (create-directory* (build-path* root +plato-app-dir+ name))
  (let ((file (build-path* root +plato-app-dir+ name +plato-handler-file+)))
    (unless (file-exists? file)
      (call-with-output-file file
	(lambda (out)
	  (display ";; auto generated stub" out) (newline out)
	  (pp `(library (plato webapp ,(string->symbol name))
		   (export entry-point
			   support-methods)
		   (import (rnrs)
			   (paella))
		 (define (support-methods) '(GET))
		 (define (entry-point req)
		   (values 200 'text/plain "OK")))
	      out))))))

)
