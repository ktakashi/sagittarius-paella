Reference manual for Plato
==========================

Plato is a web application framework based on Paella.

Demo
----

As an example, there is an example application bundled. To run this,

    $ cd example/plato
    $ sash run.scm

Then you can access http://localhost:8080/memo


Simple web application
----------------------

```shell
$ scripts/plato-recipe.scm -i sample
$ scripts/plato-recipe.scm -a hello sample

$ cd sample
$ sagittarius ../lib run.scm
```


Structure
---------

Plato script creates the following structure:

```
+ $root
    - lib/
    - apps/
       - app1/
          - handler.scm
```

`lib` directory may  contain common library among  the web application
such as constant  variables or parameters. This directory  is added to
Sagittarius' load path.

`apps`  directory contains  web  application. The  web application  is
directory base.  Thus, `app1` is the  name of application and  this is
mapped to `/app1` on the  server. `handler.scm` is loaded during start
up.   This   file  must   contains   Scheme   library.  If   you   use
`plato-recipe.scm`   to   create   a  web   application,   then   stub
implementation of the `handler.scm` is created.

Whenever `app1` is accessed by client,  then the handler is invoked in
the   same  directory   as   `app1/handler.scm`   is  located.   Means
`(current-directory)` procedure returns `$root/apps/app1`.


Handler library
---------------

Each `handler.scm` must contains a library named `(plato webapp $app)`
where `$app` is the name of the web application.

The   library  must   export  2   procedures:  `support-methods`   and
`entry-point`.  The first procedure must return a list of HTTP request
method which the application should support. The second procedure must
be a Paella HTTP request handler.

If  you want  to  use Tapas  with  this, you  can  simple define  your
`entry-point` procedure like this:

```scheme
(define entry-point (tapas-request-handler $your-http-handler))
```

Plato  also  supports  sub  context  path.  If  the  library  exports,
`mount-paths` procedure which  must be a thunk, then  during the load,
it  calls the  procedure  and  mount the  path  under the  application
path.  The  procedure  must  return  a list  which  structure  is  the
following:

- `((http-methods ...) path request-handler)`

The `http-method` must be a symbol which indicated proper HTTP method,
such as `GET` or `POST`.

The `path must be  a string or regular expression which  will be a sub
context path.

The `request-handler` must be a Paella HTTP request handler.

Each sub  context path also has  own directory allocated. So  when the
request handler is invoked, then the sub context can use own directory
as its resource location. For example,  an application `app` has a sub
context  `sub1`   and  `sub2`,   then  `sub1`'s  request   handler  is
invoked.  During  the  invokaction  of the  request  handler,  current
directory is set to `$root/apps/app/sub1`.


Session management
------------------

Plato also has a simple session management mechanism. Managing session
is done by a HTTP handler calles `plato-session-handler`. This handler
takes one argument which is a HTTP handler and returns a HTTP handler.
The simple example is the following:

```scheme
(define entry-point
  (plato-session-handler (tapas-request-handler your-handler)))
```

The session variables can be refered by `plato-session-ref` procedure.

### `(plato-session-handler _handler_)` - Procedure

Creates a HTTP handler which sets `*plato-current-session*` variable.

### `(plato-session-ref _session_ _name_)` - Procedure

Retrieves  session value  associated  with _name_  from given  session
_session_. _name_ is compared in sense of `equal?`.

### `(plato-session-set! _session_ _name_ _value_)` - Procedure

Set/update   session  value   associated   with   _name_  to   session
_session_. _name_  and _value_  must be Scheme  objects which  must be
read-write invariance.

### `(plato-session-values _session_)` - Procedure

Returns an alist  of session names and values.   Behaviour of changing
the returning value is undefined.

### `*plato-current-session*` - Variable

A parameter which is set by `plato-session-handler`. This parameter
returns current session.

### `*plato-session-id*` - Variable

A parameter which is used to determine session name.

### `*plato-session-duration*` - Variable

A parameter which represents the validity period of session.


TODO
----

- document for context

