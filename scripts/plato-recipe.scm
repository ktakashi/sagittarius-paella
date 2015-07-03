#!/bin/sh
#| -*- mode:scheme; coding: utf-8 -*-
this_path=`dirname "$0"`
exec sagittarius -L${this_path}/../lib $0 "$@"
|#
(import (rnrs)
	(plato tools)
	(getopt))

(define (usage)
  (print "plato-recipe.scm [OPTIONS] path")
  (print " OPTIONS:")
  (print "  -i,--init               initialises container named <name>")
  (print "  -a<name>,--add=<name>   adds webapp named <name>")
  (exit -1))


(define (main args)
  (with-args (cdr args)
      ((init (#\i "init") #f #f)
       (add (#\a "add") #t #f)
       . rest)
    (unless (or init add) (usage))
    (let ((path (car rest)))
      (when init
	(format #t "initialising '~a'... " path)
	(plato-skelton path)
	(print " done"))
      (when add 
	(format #t "adding '~a'... " add)
	(plato-add-webapp path add)
	(print " done")
	))))
      
