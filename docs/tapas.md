Reference manual for Tapas
==========================

Example
-------

```scheme
(import (rnrs)
	(net server)
	(paella)
	(tapas)
	(clos user))

(define config (make-http-server-config :max-thread 5))

(define page (make <tapas-page>))
(define form (make <tapas-form> :action "base64"))

(tapas-add-components! form (make <tapas-input> :name "base64"))
(tapas-add-components! form (make <tapas-textarea> :name "plain"
			       :content "hola"))
(tapas-add-components! page form)

(define http-dispatcher
  (make-http-server-dispatcher
   (GET "/" (http-file-handler "index.html" "text/html"))
   (GET "/tapas" (tapas-request-handler page))))

(define server 
  (make-simple-server "8080" (http-server-handler http-dispatcher)
                      :config config))



(server-start! server)
```

Document TBD
