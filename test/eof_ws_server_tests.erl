-module(eof_ws_server_tests).
-include_lib("eunit/include/eunit.hrl").

start_link_handshake_test() ->
    {ok, Pid} = eof_ws_server:start_link({port, 5000}),
    {ok, Socket} = gen_tcp:connect({127, 0, 0, 1}, 5000, []),
    gen_tcp:send(Socket, <<"GET /chat HTTP/1.1", 13, 10,
                           "Sec-WebSocket-Protocol: chat", 13, 10,
                           "Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==", 13, 10,
                           13, 10>>),
    Response = receive
        {tcp, Socket, Data} -> Data
    after
        1000 -> error
    end,
    ?assertEqual(lists:flatten(["HTTP/1.1 101 Switching Protocols", 13, 10,
                  "Upgrade: websocket", 13, 10,
                  "Connection: Upgrade", 13, 10,
                  "Sec-WebSocket-Accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=", 13, 10, 13, 10]), Response),
    gen_tcp:close(Socket).
