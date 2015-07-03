Paella - Simple HTTP server framework
=====================================

Paella is a simple HTTP server framework for Sagittarius Scheme. The
server itself is based on `(net server)` library.

This repository contains `(tapas)` which is a simple CLOS based HTTP
component framework and `(ploto)` which is a simple web application
framework.


Example
-------

```scheme
(import (rnrs)
	(net server)
	(paella))

(define config (make-http-server-config :max-thread 5))

(define http-dispatcher
  (make-http-server-dispatcher
    (GET "/" (http-file-handler "index.html" "text/html"))))

(define server 
  (make-simple-server "8080" (http-server-handler http-dispatcher)
                      :config config))

(server-start! server)
```

Documents
---------

- Paella: [Paella reference manual](docs/paella.md)
- Tapas: [Tapas reference manual](docs/tapas.md)
- Ploto: [Ploto reference manual](docs/ploto.md)

Supporting version
------------------

This library requries Sagittarius Scheme 0.6.5 or later.
