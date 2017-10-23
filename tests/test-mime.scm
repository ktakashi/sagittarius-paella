;; -*- mode:scheme; coding:utf-8 -*-
(import (rnrs)
	(paella)
	(net server)
	(rfc http-connections)
	(rfc base64)
	(rfc mime)
	(util bytevector)
	(srfi :64))

(define (mime-handler req)
  (define parameters (http-request-parameters req))
  (define file (cond ((assoc "file" parameters) => cdr)
		     (else #f)))
  (or (and file
	   (input-port? (http-parameter-value file))
	   (values 200 'text/plain
		   (utf8->string
		    (get-bytevector-all
		     (open-base64-encode-input-port
		      (http-parameter-value file)
		      :owner? #t)))))
      (values 500 'text/plain "Something was wrong")))

(define http-dispatcher
  (make-http-server-dispatcher
   (POST "/mime" mime-handler)))

(define config (make-http-server-config
		:exception-handler (lambda (srv sock e) (report-error e))))

(define server (make-simple-server "8080" (http-server-handler http-dispatcher)
				   :config config))

(server-start! server :background #t)

(test-begin "Test mime")

(define conn (make-http1-connection "localhost:8080" #f))

(open-http-connection! conn)

(define data (call-with-input-file "data/img.png" get-bytevector-all
				   :transcoder #f))
(for-each
 (lambda (slice)
   (define (make-content-disposision name)
     `(("content-disposition" ("form-data" ("name" . ,name)))))

   (let ((part (make-mime-part :content slice
			       :type "applciation"
			       :subtype "octet-stream"
			       :transfer-encoding "binary"
			       :headers (make-content-disposision "file"))))
     (let-values (((s h b)
		   (http-request conn 'POST "/mime"
				 :sender (http-multipart-sender conn (list part)))))
       (test-equal slice (base64-decode b)))))
 (bytevector-slices data (* 1024 10)))

(test-end)

(close-http-connection! conn)
(server-stop! server)
