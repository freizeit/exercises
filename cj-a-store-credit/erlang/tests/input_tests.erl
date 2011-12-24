-module(input_tests).

-include_lib("eunit/include/eunit.hrl").

-import(helpers, [setup/0, teardown/1]).
-import(input).

% -----------------------------------------------------------------------------
read_n_lines_test_() ->
    {foreach,
     fun() -> helpers:setup() end,
     fun(_T) -> helpers:teardown() end,
     [
        fun test_read_n_lines_empty_file/1,
        fun test_read_n_lines_with_3/1
     ]
    }.

test_read_n_lines_empty_file(T) ->
    {"This should return 'eof' unless N is zero.",
     fun() ->
        {ok, Fh} = file:open(T, [read,raw,binary,{read_ahead,1048576}]),
        Result = input:read_n_lines(Fh, [], 3),
        ?assertEqual(eof, Result)
     end}.

test_read_n_lines_with_3(T) ->
    {"This should return {ok, [L1, L2, L3]}.",
     fun() ->
         ok = file:write_file(T, string:join(["L1", "L2", "L3"], "\n")),
        {ok, Fh} = file:open(T, [read,raw,{read_ahead,1048576}]),
        Result = input:read_n_lines(Fh, [], 3),
        ?assertEqual({ok, ["L1", "L2", "L3"]}, Result)
     end}.
