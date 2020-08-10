-module(eof_http_tests).
-include_lib("eunit/include/eunit.hrl").

parse_test() ->
    Msg1 = <<"GET /RequestURI HTTP/2", 13, 10,
             "Host: localhost", 13, 10, 13, 10>>,
    ?assertEqual({"GET", "/RequestURI", "HTTP/2", [{"Host", "localhost"}]},
                 eof_http:parse(Msg1)).

generate_test() ->
    ?assertEqual([[[["HTTP/1.1 ", "204", " ", "No Content", 13, 10],
                 "Accept-Ranges", ": ", "bytes", 13, 10], "Age", ": ", "12345", 13, 10], 13, 10],
                 eof_http:generate("204", "No Content",
                                   [{"Accept-Ranges", "bytes"}, {"Age", "12345"}])).
