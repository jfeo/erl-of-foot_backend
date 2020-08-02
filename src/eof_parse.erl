-module(eof_parse).
-export([take_till/2]).

%==============================================================================
% API
%==============================================================================

take_till(Token, Data) -> take_till(Token, Data, [], Data, Token).

%==============================================================================
% Private implementation
%==============================================================================

take_till(_Token, <<>>, _Acc, _OldData, _OldToken) -> {error, reached_end};
take_till([], _Data, Acc, <<Head, OldData/binary>>, OldToken) ->
    take_till(OldToken, OldData, [Head|Acc], OldData, OldToken);
take_till([TokenHead], <<Head, Tail/binary>>, Acc, _OldData, _OldToken) when TokenHead =:= Head ->
    {ok, lists:reverse(Acc), Tail};
take_till([TokenHead | TokenTail], <<Head, Tail/binary>>, Acc, OldData, OldToken) when TokenHead =:= Head ->
    take_till(TokenTail, Tail, Acc, OldData, OldToken);
take_till([TokenHead | TokenTail], <<Head, Tail/binary>>, Acc, _OldData, OldToken) when TokenHead =/= Head ->
    take_till([TokenHead | TokenTail], Tail, [Head|Acc], Tail, OldToken).
