Erl of Foot (backend)
=====================

A badly named project, aiming to implement the backend of an online multiplayer
game.

The backend is built with Erlang/OTP, and is implemented as a websocket server.


Build
-----

Build the project using the `rebar3` build system

    $ rebar3 compile

Development
-----------

EUnit tests are placed in the `./test`-directory, and can be run with

    $ rebar eunit

Run
---

Run the server in an erlang shell with

    $ rebar shell

Enjoy!
