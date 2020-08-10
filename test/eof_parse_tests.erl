-module(eof_parse_tests).
-include_lib("eunit/include/eunit.hrl").

take_till_test() ->
    ?assertEqual({"aabc", <<"bbcd">>}, eof_parse:take_till("-", <<"aabc-bbcd">>)),
    ?assertEqual({"", <<"bbcd">>}, eof_parse:take_till("-", <<"-bbcd">>)),
    ?assertEqual({"aabc", <<"">>}, eof_parse:take_till("-", <<"aabc-">>)),
    ?assertEqual(reached_end, eof_parse:take_till("-", <<"aabcbbcd">>)),
    ?assertEqual({"123", <<"abc">>}, eof_parse:take_till("-=-", <<"123-=-abc">>)).
