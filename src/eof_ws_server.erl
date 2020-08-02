-module(eof_ws_server).
-behavior(supervisor_bridge).
-export([init/1, terminate/2]).

%%=============================================================================
%% OTP callbacks
%%=============================================================================

init({port, Port}) ->
    Pid = spawn_link(fun() ->
        {ok, ListenSocket} = gen_tcp:listen(Port, [binary, {active, false}]),
        accept_loop(ListenSocket)
    end),
    {ok, Pid, undefined}.

terminate(_Reason, _State) -> ok.

%%=============================================================================
%% Private implementation
%%=============================================================================

accept_loop(ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->
            spawn(fun() -> accept_loop(ListenSocket) end),
            http_loop(Socket);
        {error, Reason} -> {error, Reason}
    end.

handle_http(Socket, {"GET", _RequestURI, _HTTPVersion, Headers}) ->
    try eof_ws:handshake(Headers) of
        {ok, AcceptHash, _Protocols} ->
            ResponseMsg = eof_http:generate(
                "101",
                "Switching Protocols",
                [{"Upgrade", "websocket"}, {"Connection", "Upgrade"}, {"Sec-Websocket-Accept", AcceptHash}]
            ),
            gen_tcp:send(Socket, ResponseMsg),
            io:format("starting ws_loop()~n"),
            ws_loop(Socket);
        {error, _} -> http_loop(Socket)
    catch
        _ -> error
    end.

handle_ws(Socket, Fin, OPCode, Payload) ->
    io:format("Server: Fin=~p OPCode=~p Payload=~p~n", [Fin, OPCode, Payload]),
    ws_loop(Socket).

http_loop(Socket) ->
    inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, Msg} ->
            io:format("http_loop() recv~n"),
            try eof_http:parse(Msg) of
                {ok, Request} ->
                    handle_http(Socket, Request)
            catch
                _ ->
                    gen_tcp:send(Socket, eof_http:generate(
                        "500",
                        "Internal Server Error",
                        []
                    ))
            end;
        _ -> gen_tcp:close(Socket)
    end.

ws_loop(Socket) ->
    io:format("ws_loop~n"),
    inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, Msg} ->
            io:format("ws_loop() recv~n"),
            try eof_ws:parse(Msg) of
                {ok, Fin, OPCode, Payload} -> handle_ws(Socket, Fin, OPCode, Payload);
                _ -> gen_tcp:close(Socket)
            catch
                _ -> gen_tcp:close(Socket)
            end;
        _ -> gen_tcp:close(Socket)
    end,
    io:format("ws_loop ended~n").
