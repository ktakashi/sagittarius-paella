Reference manual for Paella
===========================

APIs
----

### `(make-http-server-dispatcher _spec_ ...)` - Macro

Creates a string hashtable which contains mapping of path and handler.
The _spec_ must be one of the followings:

- `(_method_ _path_ _handler_)`
- `((_method1_ _method*_ ...) _path_ _handler_)`
- `(_method_ * _handler_)`

The  first one  registers the  _handler_ to  HTTP method  _method_ and
_path_. The _path_ must not contain query string or fragment.

The second  one registers  the _handler_ to  HTTP method  _method_ and
_method*_.  This is  convenient if you want to treat  2 different HTTP
method with the same way.

Using to forms required _handler_ to  accept one argument. Which is an
HTTP request object.

The third one  registers the fallback path for the  HTTP _method_. The
_handler_ must accept 2 arguments,  HTTP registers object and alist of
path.


### `(http-server-handler _dispatcher_)` - Procedure

Returns  a  procedure  which  can  be  used  for  `make-simple-server`
procedure.   The   _dispatcher_  must   be  a  hashtable   created  by
`make-http-server-dispatcher`.

### `(make-http-server-config . _opt_)` - Procedure

Creates  a server  config  object.  This is  a  very  thin wrapper  of
`make-server-config`.  You  can  make  this  passing  `:non-blocking?`
keyword argument with value `#t`.


### `*http-not-found-handler*` - Parameter

HTTP status 404  handler. If the dispatcher doesn't  have any matching
path, then the value of this parameter is used.

### `(http-file-handler _file_ _mime_)` - Procedure

A builtin  HTTP handler  generator. _file_ must  be an  existing file.
_mime_  must be  a  valid mime  type such  as  `text/html`. Returns  a
handler procedure.

### `http-registered-path-handler` - HTTP handler

A builtin HTTP handler. This generates a link list of registerd paths.


HTTP request object
-------------------

### `(http-request? o)` - Procedure

Returns `#t` if the given _o_ is an HTTP request object, otherwise `#f`.

### `(http-request-method request)` - Procedure

Retrieves HTTP request method of this request.

### `(http-request-path request)` - Procedure

Retrieves requested path of this request.

### `(http-request-uri request)` - Procedure

Retrieves original requested  path of this request.  This may contains
query string or fragment.

### `(http-request-headers request)` - Procedure

Retrieves raw request header of this request as an alist.

### `(http-request-parameters request)` - Procedure

Retrieves request  parameters of this request.  The handled parameters
are the followings:

- Query string
- Multipart form data
- Form URL encoded

The parameters returned by this procedure  is a list of HTTP parameter
object.

Other  types of  request data  will remain  int the  `source` of  this
request object.

### `(http-request-cookies request)` - Procedure

Retrieves a list of cookie of this request.

### `(http-request-source request)` - Procedure

Retrieves source of  this request as binary input  port. The returning
value may or may not contain data.

### `(http-request-remote-address request)` - Procedure

Retrieves remote address of this request.

### `(http-request-remote-port request)` - Procedure

Retrieves remote port of this request.

HTTP parameter object
---------------------

### `(http-parameter? o)` - Procedure

Returns `#t` if  the given _o_ is an HTTP  parameter object, otherwise
`#f`.

### `(http-parameter-name parameter)` - Procedure

Returns the name of given HTTP parameter.

### `(http-parameter-value parameter)` - Procedure

Returns the  value of given HTTP  parameter. The returning value  is a
bytevector.

### `(http-parameter-headers parameter)` - Procedure

Returns the header  of given HTTP parameter. This is  only relevant if
the request  is multpart  form data.  Other request  parameter returns
`()`.


Creating a handler
------------------

A handler is a procedure which accept either 1 or 2 argument depending
on  the context,  see  `make-http-server-dispatcher` description,  and
returns at least 3 values, status, mime and content, respectively. The
procedure can return more than 3 values and the rest of the values are
treated as HTTP headers.

For  example, you  want to  make a  POST handler  which retrieves  all
request data  (and discards,  in this example),  then send  back extra
headers. The handler would look like this.

```scheme
(import (rnrs) (paella))

(define (post-handler req)
  (get-bytevector-all (http-request-source req))
  (values 200 'text/plain "Done"
          '("x-my-header" "header-value")))

```

The first  returning value can  be a list  of integer and  string. The
integer is the HTTP status and string is its description. So the above
handler can also be like this:

```scheme
(import (rnrs) (paella))

(define (post-handler req)
  (get-bytevector-all (http-request-source req))
  (values '(200 "OK OK") 'text/plain "Done"
          '("x-my-header" "header-value")))

```
