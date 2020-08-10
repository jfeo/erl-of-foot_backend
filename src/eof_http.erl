-module(eof_http).
-import(eof_parse, [take_till/2]).
-export([generate/3, parse/1]).

%==============================================================================
% API
%==============================================================================

generate(StatusCode, ReasonPhrase, Headers) ->
    generate_headers(Headers, ["HTTP/1.1 ", StatusCode, " ", ReasonPhrase, 13, 10]).

parse(Msg) ->
    {Method, Msg1} = take_till(" ", Msg),
    {RequestURI, Msg2} = take_till(" ", Msg1),
    {Version, Msg3} = take_till([13, 10], Msg2),
    Headers = parse_headers(Msg3),
    {Method, RequestURI, Version, Headers}.

%==============================================================================
% Private implementation
%==============================================================================

generate_headers([], Msg) -> [Msg, 13, 10];
generate_headers([{HeaderTag, HeaderValue} | Headers], Msg) ->
    generate_headers(Headers, [Msg, HeaderTag, ": ", HeaderValue, 13, 10]).

parse_headers(Msg) ->
    parse_headers(Msg, []).

parse_headers(<<13, 10>>, Headers) ->
    Headers;
parse_headers(Msg, Headers) ->
    {HeaderName, Msg1} = take_till(": ", Msg),
    {HeaderValue, Msg2} = take_till([13, 10], Msg1),
    parse_headers(Msg2, [{HeaderName, HeaderValue}|Headers]).
