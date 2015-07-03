(import (rnrs)
	(tapas)
	(clos user)
	(srfi :64 testing))

(test-begin "tapas")

;; TODO write more valuable tests...
(test-assert "shtml->tapas-component"
	     (is-a? (shtml->tapas-component
		     '(div (@ (style "foo"))
			   (form (@ (name "bar"))
				 "label"
				 (input (@ (type "text") 
					   (name "bzz") 
					   (value "value"))))))
		    <tapas-container>))

(test-end)
