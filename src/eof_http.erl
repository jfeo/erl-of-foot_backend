-module(eof_http).
-import(eof_parse, [take_till/2]).
-export([generate/3, parse/1]).

%==============================================================================
% API
%==============================================================================

generate(StatusCode, ReasonPhrase, Headers) ->
    generate_headers(Headers, ["HTTP/1.1 ", StatusCode, " ", ReasonPhrase, 13, 10]).

parse(Msg) ->
    {ok, Method, Msg1} = take_till(" ", Msg),
    {ok, RequestURI, Msg2} = take_till(" ", Msg1),
    {ok, Version, Msg3} = take_till([13, 10], Msg2),
    {ok, Headers} = parse_headers(Msg3),
    {ok, {Method, RequestURI, Version, Headers}}.

%==============================================================================
% Private implementation
%==============================================================================

generate_headers([], Msg) -> [Msg, 13, 10];
generate_headers([{HeaderTag, HeaderValue} | Headers], Msg) ->
    generate_headers(Headers, [Msg, HeaderTag, ": ", HeaderValue, 13, 10]).

parse_headers(Msg) -> parse_headers(Msg, []).
parse_headers(<<13, 10>>, Headers) -> {ok, Headers};
parse_headers(Msg, Headers) ->
    {ok, HeaderName, Msg1} = take_till(": ", Msg),
    {ok, HeaderValue, Msg2} = take_till([13, 10], Msg1),
    parse_headers(Msg2, [{HeaderName, HeaderValue}|Headers]).
