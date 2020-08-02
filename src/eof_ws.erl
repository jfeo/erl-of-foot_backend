-module(eof_ws).
-export([handshake/1, parse/1, generate_header/1]).

%==============================================================================
% API
%==============================================================================

handshake(Headers) ->
    handshake(Headers, no_accept_hash, []).

parse(<<Fin:1, _Reserved:3, OPCode:4, Mask:1, PayloadLen:7, Rest/binary>>) ->
    parse_payload_length(Fin, OPCode, Mask, PayloadLen, Rest).

generate_header(_Header) -> {ok, <<>>}.

%==============================================================================
% Private implementation
%==============================================================================

handshake([], no_accept_hash, _Protocol) ->
    {error, key_header_missing};
handshake([], Hash, Protocols) ->
    {ok, Hash, Protocols};
handshake([{"Sec-WebSocket-Protocol", WebSocketProtocols} | Headers], Hash, []) ->
    handshake(Headers, Hash, [WebSocketProtocols]);
handshake([{"Sec-WebSocket-Key", WebSocketKey} | Headers], no_accept_hash, Protocols) ->
    Hash = crypto:hash(sha, [WebSocketKey, "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"]),
    B64 = base64:encode(Hash),
    handshake(Headers, B64, Protocols);
handshake([_Header | Headers], Hash, Protocols) -> handshake(Headers, Hash, Protocols).


parse_payload_length(Fin, OPCode, Mask, PayloadLen, <<Rest/binary>>) when PayloadLen < 126 ->
    parse_payload(Fin, OPCode, Mask, PayloadLen, Rest);
parse_payload_length(Fin, OPCode, Mask, PayloadLen, <<PayloadLenExt:16, Rest/binary>>) when PayloadLen =:= 126 ->
    parse_payload(Fin, OPCode, Mask, PayloadLenExt, Rest);
parse_payload_length(Fin, OPCode, Mask, PayloadLen, <<PayloadLenExt:64, Rest/binary>>) when PayloadLen =:= 127 ->
    parse_payload(Fin, OPCode, Mask, PayloadLenExt, Rest).

parse_payload(Fin, OPCode, 1, PayloadLen, <<MKey1:8, MKey2:8, MKey3:8, MKey4:8, PayloadData/binary>>) ->
    {ok, Fin, OPCode, decode_masked(PayloadLen, MKey1, MKey2, MKey3, MKey4, PayloadData, <<>>)};
parse_payload(Fin, OPCode, 0, _PayloadLen, Payload) ->
    {ok, Fin, OPCode, Payload}.

decode_masked(0, _M1, _M2, _M3, _M4, <<>>, Decoded) -> Decoded;
decode_masked(Len, M1, M2, M3, M4, <<Octet:8, Rest/binary>>, <<Decoded/binary>>) ->
    NextDecoded = Octet bxor M1,
    decode_masked(Len-1, M2, M3, M4, M1, Rest, <<Decoded/binary, NextDecoded:8>>).

