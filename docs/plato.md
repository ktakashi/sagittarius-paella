Reference manual for Plato
==========================

Plato is a web application framework based on Paella.

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

`lib` directory may contain common library among the web application such
as constant variables or parameters. This directory is added to Sagittarius'
load path.

`apps` directory contains web application. The web application is directory
base. Thus, `app1` is the name of application and this is mapped to `/app1`
on the server. `handler.scm` is loaded during start up. This file must
contains Scheme library. If you use `plato-recipe.scm` to create a web
application, then stub implementation of the `handler.scm` is created.

Whenever `app1` is accessed by client, then the handler is invoked in the
same directory as `app1/handler.scm` is located. Means `(current-directory)`
procedure returns `$root/apps/app1`.

Handler library
---------------

Each `handler.scm` must contains a library named `(ploto webapp $app)` where
`$app` is the name of the web application.

The library must export 2 procedures: `support-methods` and `entry-point`.
The first procedure must return a list of HTTP request method which the
application should support. The second procedure must be a Paella HTTP
request handler.

If you want to use Tapas with this, you can simple define your `entry-point`
procedure like this:

```scheme
(define entry-point (tapas-request-handler $your-http-handler))
```


TODO
----

- session management
- context path


