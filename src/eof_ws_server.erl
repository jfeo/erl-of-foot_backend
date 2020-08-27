-module(eof_ws_server).
-behavior(gen_server).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).
-record(state, {phase, socket}).

start_link(ListenSocket) ->
    gen_server:start_link(?MODULE, ListenSocket, []).

%%=============================================================================
%% OTP callbacks
%%=============================================================================

init(ListenSocket) ->
    io:format("initializing~n"),
    gen_server:cast(self(), accept),
    {ok, #state{phase=accept, socket=ListenSocket}}.

handle_cast(accept, S = #state{phase=accept, socket=ListenSocket}) ->
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
    ok = inet:setopts(AcceptSocket, [{active, once}]),
    io:format("accepting~n"),
    eof_backend_sup:start_socket(),
    {noreply, S#state{socket=AcceptSocket, phase=handshake}}.

handle_info({tcp, _Socket, Msg}, S = #state{socket=Socket, phase=handshake}) ->
    ok = inet:setopts(Socket, [{active, once}]),
    io:format("parsing http message~n"),
    try eof_http:parse(Msg) of
        Request -> handle_http(S, Socket, Request)
    catch
        _ ->
            gen_tcp:send(Socket, eof_http:generate(
                "500",
                "Internal Server Error",
                []
            )),
            {noreply, S}
    end;
 
handle_info({tcp, _Socket, Msg}, S = #state{socket=Socket, phase=ws}) ->
    ok = inet:setopts(Socket, [{active, once}]),
    io:format("parsing new websocket message~n"),
    try eof_ws:parse(Msg) of
        {ok, Fin, OPCode, _Length, Payload} ->
            handle_message(S, Fin, OPCode, Payload);
        {more_plain, Fin, OPCode, RemLen, Payload} ->
            Phase = {more_plain, Fin, OPCode, RemLen, Payload},
            {noreply, S#state{phase=Phase}};
        {more_masked, Fin, OPCode, Mask, RemLen, Payload} ->
            Phase = {more_masked, Fin, OPCode, Mask, RemLen, Payload},
            {noreply, S#state{phase=Phase}}
    catch
        _ -> {noreply, S}
    end;

handle_info({tcp, _Socket, Msg}, S = #state{socket=Socket, phase={more_plain, Fin, OPCode, RemLen, Payload}}) ->
    ok = inet:setopts(Socket, [{active, once}]),
    io:format("parsing new websocket message~n"),
    case eof_ws:dump(RemLen, Msg) of
        {ok, RestPayload} ->
            handle_message(S#state{phase=ws}, Fin, OPCode, <<Payload, RestPayload>>);
        {more_plain, NewRemLen, MorePayload} ->
            Phase = {more_plain, Fin, OPCode, NewRemLen, <<Payload, MorePayload>>},
            {noreply, S#state{phase=Phase}}
    end;

handle_info({tcp, _Socket, Msg}, S = #state{socket=Socket, phase={more_masked, Fin, OPCode, Mask, RemLen, Payload}}) ->
    ok = inet:setopts(Socket, [{active, once}]),
    io:format("parsing continued websocket message~n"),
    {M1, M2, M3, M4} = Mask,
    try eof_ws:decode(RemLen, M1, M2, M3, M4, Msg) of
        {ok, Decoded} ->
            handle_message(S#state{phase=ws}, Fin, OPCode, <<Payload, Decoded>>);
        {more_masked, NewMask, NewRemLen, Decoded} ->
            Phase = {more_masked, Fin, OPCode, NewMask, NewRemLen, <<Payload/binary, Decoded/binary>>},
            {noreply, S#state{phase=Phase}}
    catch
        _ -> {noreply, S#state{phase=ws}}
    end;

handle_info({tcp, _Socket, Msg}, S = #state{socket=Socket}) ->
    ok = inet:setopts(Socket, [{active, once}]),
    io:format("got ill-fitting tcp message~n"),
    io:format("state=~p~n", [S]),
    io:format("message=~p~n", [Msg]),
    {noreply, S}.

%% Stub implementation
handle_call(_, _, State) ->
    {noreply, State}.

%% Stub implementation
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%=============================================================================
%% Private implementation
%%=============================================================================

handle_message(State, Fin, OPCode, _Msg) ->
    io:format("[~p] [~p]~n", [Fin, OPCode]),
    {noreply, State}.

send(Socket, Msg) ->
    ok = gen_tcp:send(Socket, Msg),
    ok = inet:setopts(Socket, [{active, once}]),
    ok.

handle_http(State, Socket, {"GET", _RequestURI, _HTTPVersion, Headers}) ->
    try eof_ws:handshake(Headers) of
        {ok, AcceptHash, _Protocols} ->
            ResponseMsg = eof_http:generate(
                "101",
                "Switching Protocols",
                [{"Upgrade", "websocket"}, {"Connection", "Upgrade"}, {"Sec-WebSocket-Accept", AcceptHash}]
            ),
            ok = send(Socket, ResponseMsg),
            {noreply, State#state{phase=ws}};
        {error, _} -> {noreply, State}
    catch
        _ -> {noreply, State}
    end.
