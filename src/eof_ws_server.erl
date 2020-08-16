-module(eof_ws_server).
-behavior(supervisor_bridge).
-export([start_link/1]).
-export([init/1, terminate/2]).

start_link({port, Port}) ->
    supervisor_bridge:start_link({local, eof_ws_server}, eof_ws_server, {port, Port}).

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
                Request ->
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
    inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, Msg} ->
            try eof_ws:parse(Msg) of
                {ok, Fin, OPCode, Length, Payload} ->
                    io:format("ok~n"),
                    handle_ws(Socket, Fin, OPCode, Payload);
                {more_plain, Fin, OPCode, RemLen, Payload} ->
                    ws_continue_plain(Socket, Fin, OPCode, RemLen, Payload);
                {more_masked, Fin, OPCode, Mask, RemLen, Payload} ->
                    ws_continue_masked(Socket, Fin, OPCode, Mask, RemLen, Payload);
                W -> io:format("closing~n~p~n", [W]),
                    gen_tcp:close(Socket)
            catch
                _ -> gen_tcp:close(Socket)
            end;
        _ -> gen_tcp:close(Socket)
    end.

ws_continue_plain(Socket, Fin, OPCode, RemLen, Payload) ->
    inet:setopts(Socket, [{active, once}]),
    receive
       {tcp, Socket, Msg} ->
           case eof_ws:dump(RemLen, Msg) of
               {ok, RestPayload} ->
                   handle_ws(Socket, Fin, OPCode, <<Payload, RestPayload>>);
               {more_plain, NewRemLen, MorePayload} ->
                   ws_continue_plain(Socket, Fin, OPCode, NewRemLen, <<Payload, MorePayload>>);
               _ -> gen_tcp:close(Socket)
           end;
       _ -> gen_tcp:close(Socket)
    end.

ws_continue_masked(Socket, Fin, OPCode, {M1, M2, M3, M4}, RemLen, Payload) ->
    inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, Msg} ->
            try eof_ws:decode(RemLen,M1, M2, M3, M4, Msg) of
                {ok, Decoded} ->
                    handle_ws(Socket, Fin, OPCode, <<Payload/binary, Decoded/binary>>);
                {more_masked,Mask, NewRemLen, Decoded} ->
                    ws_continue_masked(Socket, Fin, OPCode, Mask, NewRemLen, <<Payload/binary, Decoded/binary>>);
                _ -> gen_tcp:close(Socket)
            catch
                _ -> gen_tcp:close(Socket)
            end;
        _ -> gen_tcp:close(Socket)
    end.
