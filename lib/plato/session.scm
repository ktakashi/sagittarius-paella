;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; plato/session.scm - Session manager
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

(library (plato session)
    (export plato-session-handler
	    plato-session-values
	    plato-session-name
	    plato-session-set!
	    plato-session-ref
	    *plato-session-id*
	    *plato-current-session*
	    *plato-session-duration*)
    (import (rnrs)
	    (rnrs mutable-pairs)
	    (sagittarius)
	    (sagittarius control)
	    (clos user)
	    (clos core)
	    (srfi :18)
	    (srfi :19 time)
	    (srfi :39 parameters)
	    (util timer)
	    (util file)
	    (paella)
	    (plato invoke)
	    (rfc uuid)
	    (rfc cookie)
	    (math))

(define-constant +session+ "session")

;; the name of session in the cookie
(define *plato-session-id* (make-parameter "plato-session-id"))
(define *plato-current-session* (make-parameter #f))
;; default 10 mins (must be in second)
(define *plato-session-duration* (make-parameter (* 60 10)))

(define-class <plato-session> ()
  ((name :init-keyword :name :reader plato-session-name)
   (app  :init-keyword :app)
   (created :init-keyword :created)
   ;; alist of session data
   (data :init-keyword :data :init-value '()
	 :reader plato-session-values)
   (timer-id :init-keyword :timer-id)
   ))

(define (plato-session-set! session name value)
  (let ((data (plato-session-values session)))
    (cond ((assoc name data) =>
	   (lambda (slot) (set-cdr! slot value)))
	  (else
	   (slot-set! session 'data (acons name value data))))))
(define (plato-session-ref session name :optional (fallback #f))
  (let ((data (plato-session-values session)))
    (cond ((assoc name data) => cdr)
	  (else fallback))))

;;; Sessions are files for now. this may not be secure but for now it's ok
(define (plato-session-handler proc)
  (lambda (req)
    (define plato-context (*plato-current-context*))
    ;; application name
    (define app (plato-application-name plato-context))
    (define work (let loop ((root plato-context))
		   (if (plato-parent-context root)
		       (loop (plato-parent-context root))
		       (plato-work-path root))))
    ;; session
    (define session-id (*plato-session-id*))
    (define session (retrieve-session plato-context req session-id app work))
    ;; don't let handler change
    (define duration (*plato-session-duration*))
    (parameterize ((*plato-current-session* session))
      (unwind-protect 
       (let-values (((status mime content . rest) (proc req)))
	 (let* ((when (add-duration (current-time)
				    (make-time time-duration 0 duration)))
		(cookie (make-cookie session-id
				     (slot-ref session 'name)
				     ;; forcing offset to be 0
				     :expires (time-utc->date when 0)
				     :http-only #t
				     :path (string-append "/" app))))
	   (apply values status mime content 
		  (cons (list "set-cookie" (cookie->string cookie)) rest))))
       (with-plato-context-lock plato-context
	 (save-session work (*plato-current-session*)))))))

;;; private stuff
(define (retrieve-session ctx req session-id app work)
  (define cookies (http-request-cookies req))
  (let ((session-name (or (and-let* ((c (find (lambda (c) 
						(string=? (cookie-name c)
							  session-id))
					      cookies)))
			    (cookie-value c))
			  (generate-session-id req))))
    (with-plato-context-lock ctx
      (load/create-session ctx req app session-name work))))

(define (generate-session-id req)
  (let* ((h (hash-algorithm SHA-1))
	 (bv (make-bytevector (hash-size h))))
    (hash-init! h)
    (hash-process! h (string->utf8 (http-request-remote-address req)))
    (hash-process! h (string->utf8
		      (number->string (http-request-remote-port req))))
    (hash-done! h bv)
    (number->string (bytevector->integer bv) 32)))

(define (make-session session) 
  (apply make <plato-session> (apply append session)))

(define (save-session dir session)
  (define (session->raw-session session)
    (define (get-init-keyword slot)
      (slot-definition-option slot :init-keyword))
    (let ((slots (class-slots <plato-session>)))
      (map (lambda (s) 
	     (list (get-init-keyword s)
		   (slot-ref session (slot-definition-name s)))) slots)))
  (let* ((session-dir (build-path* dir +session+))
	 (file (build-path* session-dir (slot-ref session 'name))))
    (unless (file-exists? session-dir) (create-directory* session-dir))
    (when (file-exists? file) (delete-file file))
    (call-with-output-file file
      (lambda (out) 
	(write (session->raw-session session) out) (newline out)
	;; return session for convenience
	session))))

(define (load/create-session ctx req app name dir)
  (define (check timer session name)
    (and (timer-exists? timer (slot-ref session 'timer-id))
	 (string=? (slot-ref session 'name) (generate-session-id req))))

  (let ((file (build-path* dir +session+ name))
	(timer (retrieve-timer app)))
    (if (file-exists? file)
	(let* ((raw-session (call-with-input-file file read))
	       (session (make-session 
			 `(,@(remp (lambda (i) (eq? :created (car i))) 
				   raw-session)
			   (:created ,(time-second (current-time)))))))
	  (if (check timer session name)
	      (begin
		(timer-reschedule! timer
				   (slot-ref session 'timer-id)
				   (make-expire-task ctx file))
		session)
	      (let ((id (timer-schedule! timer (make-expire-task ctx file)
				   ;; to milliseconds
				   (* (*plato-session-duration*) 1000))))
		(slot-set! session 'timer-id id)
		;; update id
		(save-session dir session))))
	(let ((id (timer-schedule! timer (make-expire-task ctx file)
				   ;; to milliseconds
				   (* (*plato-session-duration*) 1000))))
	  (save-session dir
	   (make-session `((:name ,name)
			   (:app  ,app)
			   (:created ,(time-second (current-time)))
			   (:timer-id ,id))))))))

;; expiring task
(define retrieve-timer 
  (let ((timers (make-string-hashtable)))
    (lambda (name)
      (cond ((hashtable-ref timers name #f))
	    (else 
	     (let ((timer (timer-start! (make-timer))))
	       (hashtable-set! timers name timer)
	       timer))))))

;; removing session file
(define (make-expire-task ctx file)
  (lambda () 
    (when (file-exists? file) 
      (with-plato-context-lock ctx
	(when (file-exists? file) 
	  (delete-file file))))))
    
)
