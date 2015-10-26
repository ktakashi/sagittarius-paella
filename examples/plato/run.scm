(add-load-path (absolute-path "../../lib"))
(import (rnrs)
	(paella)
	(plato)
	(plato tools)
	(getopt)
	(clos user)
	(util concurrent)
	(sagittarius)
	(sagittarius io)
	(sagittarius socket)
	(sagittarius threads)
	(sagittarius remote-repl)
	(util file)
	(srfi :13))


(define-constant +stop-commands+ '("stop" "restart"))

(define +root+
  (let ((script (absolute-path (car (command-line)))))
    (let-values (((dir b e) (decompose-path script)))
      dir)))

;; paella dispatcher
(define http-dispatcher (plato-make-dispatcher +root+))

;; Creating frame to show all example applications
(http-add-dispatcher! http-dispatcher 'GET "/"
  (lambda (req)
    (let ((alist (filter (lambda (s)
			   (and (string=? "GET" (car s))
				(not (string=? "*" (cadr s)))
				(not (string=? "/" (cadr s)))
				;; strip sub path
				(zero? (string-index-right (cadr s) #\/))))
			 (http-mapped-path->alist http-dispatcher))))
      (values 200 'sxml 
	      `(html
		(head (title "Tools"))
		(body 
		 (div (@ (style "float: left; width: 15%; margin: 0px"))
		   ,@(map (lambda (path)
			    `(div (a (@ (href ,path)
					(target "content-frame"))
				     ,path)))
			  (list-sort string<=? (map cadr alist))))
		 (div (@ (style "float: right; width: 85%; margin: 0x; padding: 0px"))
		   (iframe (@ (name "content-frame")
			      (style "width: 99%; height: 100%; border: 0px; border-left: 1px solid #cacaca;"))))))))))

(define (make-shutdown-handler shared-queue)
  (lambda (server socket)
    (let ((command (utf8->string (socket-recv socket 255))))
      (and (member command +stop-commands+)
           (shared-queue-put! shared-queue command)))))

;; create server config
(define (make-server-config shutdown-port max-thread)
  (let* ((p (string->number shutdown-port))
         (shared-queue (make-shared-queue))
         (handler (make-shutdown-handler shared-queue)))
    (values (make-http-server-config
	     :max-thread max-thread
	     :shutdown-port (and p shutdown-port)
	     :shutdown-handler handler)
	    shared-queue)))

;; run the server
(define (run-server port config remote-port shared-queue)
  (define server-thread
    (make-thread
     (lambda ()
       (plato-run port config http-dispatcher))))
  (define (wait! socket)
    (thread-join! server-thread)
    (when socket
      (socket-shutdown socket SHUT_RDWR)
      (socket-close socket))
    (unless (shared-queue-empty? shared-queue)
      (let ((command (shared-queue-get! shared-queue)))
        (when (equal? command "restart")
	  (run-server port config remote-port shared-queue)))))
  (thread-start! server-thread)
  (if remote-port
      (let-values (((repl socket) (make-remote-repl remote-port)))
	(thread-start! (make-thread repl))
	(wait! socket))
      (wait! #f)))

(define (send-command config command)
  (let ((socket (make-client-socket "localhost"
				    (slot-ref config 'shutdown-port))))
    (socket-send socket (string->utf8 command))))

;; For interactive development
(define (reload-webapp name)
  (plato-reload name +root+ http-dispatcher))

(define (usage script)
  (format #t "~a [OPTIONS]~%" script)
  (format #t " OPTIONS~%")
  (format #t "  -p<num>,--port <num>\tspecify HTTP port (default 8080)~%")
  (format #t "  -r<num>,--remote-port <num>\tspecify remote repl port~%")
  (format #t "  -s<num>,--shutdown-port <num>\tspecify shutdown port (default 8081)~%")
  (format #t "  -c<command>,--command <command>\trun the command~%")
  (format #t "    `run`     starts the server~%")
  (format #t "    `restart` re-starts the server~%")
  (format #t "    `stop`    stops the server~%")
  (format #t "  -m<num>,--max-thread <num>\tspecify the number of server thread~%")
  (format #t "  -h,--help\tshow this message~%"))


;; entry point
(define (main args)
  (with-args (cdr args)
      ((port (#\p "port") #t "8080")
       (remote-port (#\r "remote-port") #t #f)
       (shutdown-port (#\s "shutdown-port") #t  "8081")
       (command (#\c "command") #t "run")
       (max-thread (#\t "max-thread") #t "10")
       (help (#\h "help") #f #f))
    (when help (usage (car args)) (exit 0))
    (let-values (((config shared-queue) (make-server-config
					 shutdown-port
					 (string->number max-thread))))
      (case (string->symbol command)
        ((run) (run-server port config remote-port shared-queue))
        (else  (send-command config command))))))

