-module(eof_ws).
-export([handshake/1, parse/1, dump/2, decode/6, generate_header/1]).

%==============================================================================
% API
%==============================================================================

handshake(Headers) ->
    handshake(Headers, no_accept_hash, []).

parse(<<Fin:1, _Reserved:3, OPCode:4, MaskBit:1, PayloadLen:7, Payload/binary>>) ->
    {ok, Length, Rest} = parse_length(PayloadLen, Payload),
    ParsedPayload = case {MaskBit, Rest} of
        {1, <<M1:8, M2:8, M3:8, M4:8, Encoded/binary>>} -> decode(Length, M1, M2, M3, M4, Encoded);
        {0, Rest} -> dump(Rest, Length)
    end,
    case ParsedPayload of
        {ok, Data} ->
            {ok, Fin, OPCode, Length, Data};
        {more_masked, Mask, RemLen, Data} ->
            {more_masked, Fin, OPCode, Mask, RemLen, Data};
        {more_plain, RemLen, Data} ->
            {more_plain, Fin, OPCode, RemLen, Data}
    end.

dump(Length, Payload) ->
    MeasuredLength = byte_size(Payload),
    if MeasuredLength =:= Length ->
           {ok, Payload};
       MeasuredLength < Length ->
           {more_plain, Length - MeasuredLength, Payload};
       MeasuredLength > Length ->
           {error, too_much_data}
    end.

decode(Length, M1, M2, M4, M5, Encoded) ->
    decode(Length, M1, M2, M4, M5, Encoded, <<>>).

generate_header(_Header) -> {ok, <<>>}.

%==============================================================================
% Private implementation
%==============================================================================

handshake([], no_accept_hash, _Protocol) ->
    {error, key_header_missing};
handshake([], Hash, Protocols) ->
    {ok, Hash, Protocols};
handshake([{"Sec-WebSocket-Protocol", Protocol} | Headers], Hash, []) ->
    handshake(Headers, Hash, [Protocol]);
handshake([{"Sec-WebSocket-Key", WebSocketKey} | Headers], no_accept_hash, Protocols) ->
    Hash = crypto:hash(sha, [WebSocketKey, "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"]),
    B64 = base64:encode(Hash),
    handshake(Headers, B64, Protocols);
handshake([_Header | Headers], Hash, Protocols) -> handshake(Headers, Hash, Protocols).

parse_length(Length, <<Rest/binary>>) when Length < 126 ->
    {ok, Length, Rest};
parse_length(Length, <<LengthMed:16, Rest/binary>>) when Length =:= 126 ->
    {ok, LengthMed, Rest};
parse_length(Length, <<LengthLong:64, Rest/binary>>) when Length =:= 127 ->
    {ok, LengthLong, Rest}.

decode(0, _M1, _M2, _M3, _M4, <<>>, Decoded) ->
    {ok, Decoded};
decode(Len, M1, M2, M3, M4, <<>>, Decoded) when Len > 0 ->
    {more_masked, {M1, M2, M3, M4}, Len, Decoded};
decode(Len,  M1, M2, M3, M4, <<Octet:8, Encoded/binary>>, <<Decoded/binary>>) ->
    NextDecoded = Octet bxor M1,
    decode(Len-1, M2, M3, M4, M1, Encoded, <<Decoded/binary, NextDecoded:8>>).
