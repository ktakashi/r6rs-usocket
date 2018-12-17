`(usocket)`: Portable R6RS socket library
=========================================

`(usocket)` will be a portable socket library for R6RS implementations.
This library will provide wrapper layer of the implementation specific
socket APIs. Which means, it may not full support for some of the
functionalities from a particular implementation.

APIs
====

The library provides the following APIs.

- `(usocket? o)`:
  Returns `#t` if the given *o* is an socket object, otherwise `#f`
- `(usocket-close! socket)`:
  Closes the given *socket*.
- `(usocket-shutdown! socket how)`:
  Shutdowns the given *socket*, the *how* argument must be the followings:
  - `*usocket:shutdown-read*`:
	Shutdowns read channel of the given *socket*.
  - `*usocket:shutdown-write*`:
  	Shutdowns write channel of the given *socket*.
  - `*usocket:shutdown-read&write*`:
  	Shutdowns read and write channels of the given *socket*.
- `(client-usocket? o)`:
  Return `#t` if the given *o* is a client socket object, otherwise `#f`.
- `(client-usocket-input-port client-socket)`:
  Return an binrary input port of the given *client-socket*.
- `(client-usocket-output-port client-socket)`:
  Return an binrary output port of the given *client-socket*.
- `(server-usocket? o)`:
  Return `#t` if the given *o* is a server socket object, otherwise `#f`.
- `(server-usocket-accept! server-socket)`:
  Waits until the given *server-socket* receives input and returns
  a client socket.
- `(make-tcp-client-usocket host service)`:
  Create a TCP client socket object which connects to the givne *host* on the
  *service*. The *service* can be an integer or string.
- `(make-tcp-server-usocket service)`:
  Create a TCP server socket object which accepts an input on the *service*.
  The *service* can be an integer or string.
- `(make-udp-client-usocket host service)`:
  Create a UDP client socket object which connects to the givne *host* on the
  *service*. The *service* can be an integer or string.
- `(make-udp-server-usocket service)`:
  Create a UDP server socket object which accepts an input on the *service*.
  The *service* can be an integer or string.

Bonus
=====

This library provides portable SRFI 106 implementation with the name
of `(usocket srfi pffi)`. This library provides all the interface
defined in SRFI 106 and extra `socket-error?` and `socket-error-socket`
procedures.

Dependencies
============

This library depends on the following libraries

- [`(pffi)`](https://github.com/ktakashi/r6rs-pffi/)
- [`(psystem libc)`](https://github.com/ktakashi/r6rs-psystem/)

Supported implementations
=========================

Currently the following implemnetations are supported.

- Sagittarius Scheme (0.9.4 or later)
- Chez Scheme (v9.5)
- Larceny (1.3)

How to add implemnetations
==========================

The easiest way would be using the portable SRFI 106 implemnetations, which
requires to be supported by the `(psystem libc)` library. See the dependencies
section and read the instruction of the dependency.

If the implemnetation supports socket, it's most of the case better
to use it. In that case, there are following 2 scenarios:

1. Implement `(pffi srfi)`: This requires entire SRFI 106 procedures.
2. Implement `(pffi api)`: This requires implementing the following 4 procedures
   - `make-tcp-client-usocket`
   - `make-tcp-server-usocket`
   - `make-udp-client-usocket`
   - `make-udp-server-usocket`
   See default `lib/usocket/api.sls` as an example.

Copyright and lincence
======================

Copyright 2018 Takashi Kato. Code released under the BSD-style
license. See [COPYING](COPYING).
