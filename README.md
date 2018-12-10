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

Supported implementations
=========================

Currently the following implemnetations are supported.

- Sagittarius Scheme (0.9.4 or later)

Copyright and lincence
======================

Copyright 2018 Takashi Kato. Code released under the BSD-style
license. See [COPYING](COPYING).
