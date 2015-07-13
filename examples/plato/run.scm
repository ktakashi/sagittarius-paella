(add-load-path (absolute-path "../../lib"))
(import (rnrs) (paella) (plato))

(define config (make-http-server-config :max-thread 10))

(invoke-plato (absolute-path "./") "8080" config)
