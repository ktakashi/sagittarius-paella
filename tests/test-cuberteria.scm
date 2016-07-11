(import (rnrs)
	(cuberteria)
	(clos user)
	(text json)
	(sagittarius object)
	(srfi :39)
	(srfi :64 testing)
	(pp))

(test-begin "cuberteria")

(define-class <address> ()
  ((city :init-keyword :city)
   (province :init-keyword :province :json-element-name "state")))

(define-class <person> ()
  ((first-names :init-keyword :first-names :json-element-name "firstNames")
   (last-name :init-keyword :last-name :json-element-name "lastName")
   (sex  :init-keyword :sex)
   (age  :init-keyword :age)
   (address :init-keyword :address :json <address>)))

(define *json* '(("firstNames" . #("Takashi"))
		 ("lastName" . "Kato")
		 ("sex"      . "Male")
		 ("age"      . "forever 18")
		 ("address"  . (("city" . "Leiden")
				("state" . "South Holland")))))
		 
(define *json-string* (call-with-string-output-port
		       (lambda (out) 
			 (parameterize ((*json-map-type* 'alist))
			   (json-write *json* out)))))

(test-equal "json->object"
	    '(#("Takashi") "Kato" "Male" "forever 18" "Leiden" "South Holland")
	    (let ((p (cuberteria-map-json-string! (make <person>) 
						  *json-string*)))
	      (list (~ p 'first-names)
		    (~ p 'last-name)
		    (~ p 'sex)
		    (~ p 'age)
		    (~ p 'address 'city)
		    (~ p 'address 'province))))

(test-equal "object->json" *json*
	    (let* ((addr (make <address> :city "Leiden" 
			       :province "South Holland"))
		   (p (make <person> :first-names '#("Takashi")
			    :last-name "Kato"
			    :sex "Male"
			    :age "forever 18"
			    :address addr)))
	      (cuberteria-object->json p 'alist)))

(test-end)