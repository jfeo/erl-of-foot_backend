-module(eof_ws_tests).
-include_lib("eunit/include/eunit.hrl").


handshake_test() ->
    ?assertEqual(eof_ws:handshake([{"Sec-WebSocket-Protocol", "chat"}, {"Sec-WebSocket-Key", "dGhlIHNhbXBsZSBub25jZQ=="}]), {ok, <<"s3pPLMBiTxaQ9kYGzzhZRbK+xOo=">>, ["chat"]}),
    ?assertEqual(eof_ws:handshake([{"Sec-WebSocket-Key", "dGhlIHNhbXBsZSBub25jZQ=="}]),
                 {ok, <<"s3pPLMBiTxaQ9kYGzzhZRbK+xOo=">>, []}),
    ?assertEqual(eof_ws:handshake([]), {error, key_header_missing}).

parse_test() ->
    ?assertEqual(eof_ws:parse(<<1:1, 0:3, 1:4, 1:1, 10:7, 10:8, 20:8, 30:8, 40:8, 75:8
, 85:8, 95:8, 105:8, 75:8, 85:8, 95:8, 105:8, 75:8, 85:8>>), {ok, 1, 1, 10, <<"AAAAAAAAAA">>}).
