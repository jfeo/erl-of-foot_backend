Erl of Foot (backend)
=====================

A badly named project, aiming to implement the backend of an online multiplayer
game.

The backend is built with Erlang/OTP, and is implemented as a websocket server.


Build
-----

So far, no build system has been setup. You can compile the modules by:

```
erlc src/*.erl
```


Run
---

Run the server by

```
eof_ws_server:init({port, PORT}).
```

Enjoy!
