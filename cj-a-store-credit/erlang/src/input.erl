-module(input).
-export([handle_file/2, handle_store_record/3]).
-import(calc, [find_items/0]).
-include("data_defs.hrl").

-ifdef(TEST).
-import(test_helpers, [setup/0, teardown/1]).
-include_lib("eunit/include/eunit.hrl").
-endif.


%% @doc Read input file and spawn a process for each store record found.
%% Return the number of store records processed.
-spec
    handle_file(Path :: string(), Rcvr :: pid())
    -> integer().
handle_file(Path, Rcvr) ->
    {ok, Fh} = file:open(Path, [read,raw,{read_ahead,1048576}]),
    % ignore first line
    {ok, _} = file:read_line(Fh),
    do_handle_file(Fh, Rcvr, 0).


%% doc Read 3 lines and convert them to a store record. Call calc:find_items()
%% (in a separate process) for each store record found. Returns the number of
%% store records found.
%% Please note: we want to do as little work as possible in the process that
%% is reading the input file.
-spec
    do_handle_file(Fh :: file:io_device(), Rcvr :: pid(), N :: integer())
    -> integer().
do_handle_file(Fh, Rcvr, N) ->
    case read_n_lines(Fh, [], 3) of
        {ok, Ls} ->
            spawn(?MODULE, handle_store_record, [Ls, Rcvr, N+1]),
            do_handle_file(Fh, Rcvr, N+1);
        eof -> N
    end.


%% doc Handles one store record. Convert the 3 given input lines to a
%% 'storerec' and call the function that will find the 2 store items whose prise
%% adds up to the store credit granted.
-spec
    handle_store_record(Ls :: [string()], Rcvr :: pid(), N :: integer())
    -> none().
handle_store_record([L1, _, L3], Rcvr, N) ->
    {C, _} = string:to_integer(L1),
    Is = [P || {P,_} <- lists:map(fun string:to_integer/1, string:tokens(L3, " "))],
    R = #storerec{index=N, credit=C, items=Is},
    calc:find_items(R, Rcvr).


%% @doc Read the next N lines from the given file handle.
-spec
    read_n_lines(Fh :: file:io_device(), Acc :: [string()], N :: integer())
    -> {ok | [string()]} | eof | {error, file:ext_posix()}.
read_n_lines(_, Acc, 0) -> {ok, lists:reverse(Acc)};
read_n_lines(Fh, Acc, N) ->
    % Try reading another line.
    case file:read_line(Fh) of
        % We got another line, keep going.
        {ok, Data} -> read_n_lines(Fh, [Data|Acc], N-1);
        % Either at the end of file or something broke.
        EofOrError -> EofOrError
    end.










-ifdef(TEST).
% -----------------------------------------------------------------------------
% -----------------------------------------------------------------------------
% -----------------------------------------------------------------------------
% Tests for read_n_lines()
% -----------------------------------------------------------------------------
read_n_lines_test_() ->
    {foreach,
     fun() -> test_helpers:setup() end,
     fun(T) -> test_helpers:teardown(T) end,
     [
        fun test_read_n_lines_empty_file/1,
        fun test_read_n_lines_with_2_of_3/1,
        fun test_read_n_lines_with_3_of_3/1
     ]
    }.

test_read_n_lines_empty_file(T) ->
    {"This should return 'eof' unless N is zero.",
     fun() ->
        {ok, Fh} = file:open(T, [read,raw,{read_ahead,1048576}]),
        Result = read_n_lines(Fh, [], 3),
        ?assertEqual(eof, Result)
     end}.

test_read_n_lines_with_2_of_3(T) ->
    {"Not enough data (third line missing), this should return eof.",
     fun() ->
        ok = file:write_file(T, string:join(["L1", "L2"], "\n")),
        {ok, Fh} = file:open(T, [read,raw,{read_ahead,1048576}]),
        Result = read_n_lines(Fh, [], 3),
        ?assertEqual(eof, Result)
     end}.

test_read_n_lines_with_3_of_3(T) ->
    {"This should return {ok, [L1, L2, L3]}.",
     fun() ->
        ok = file:write_file(T, string:join(["L1", "L2", "L3"], "\n")),
        {ok, Fh} = file:open(T, [read,raw,{read_ahead,1048576}]),
        Result = read_n_lines(Fh, [], 3),
        ?assertEqual({ok, ["L1\n", "L2\n", "L3"]}, Result)
     end}.

% -----------------------------------------------------------------------------
% Tests for do_handle_file()
% -----------------------------------------------------------------------------
do_handle_file_test_() ->
    {foreach,
     fun() -> test_helpers:setup() end,
     fun(T) -> test_helpers:teardown(T) end,
     [
        fun test_do_handle_file_with_1_rec/1
     ]
    }.

test_do_handle_file_with_1_rec(T) ->
    {"Exercise do_handle_file() with one store record",
     fun() ->
        ok = file:write_file(T, string:join(["10", "3", "7 1 3"], "\n")),
        {ok, Fh} = file:open(T, [read,raw,{read_ahead,1048576}]),
        Count = do_handle_file(Fh, self(), 0),
        ?assertEqual(1, Count),
        receive
            Result ->
                ?assertEqual(<<"Case 1: 1 3">>, iolist_to_binary(Result))
        after 1000 ->
            ?assert(false)
        end
     end}.

% -----------------------------------------------------------------------------
% Tests for handle_file()
% -----------------------------------------------------------------------------
handle_file_test_() ->
    {foreach,
     fun() -> test_helpers:setup() end,
     fun(T) -> test_helpers:teardown(T) end,
     [
        fun test_handle_file_with_1_rec/1
     ]
    }.

test_handle_file_with_1_rec(T) ->
    {"Exercise handle_file() with one store record",
     fun() ->
        ok = file:write_file(
            T, string:join(["2", "15", "3", "3 7 8", "2", "2", "1 1"], "\n")),
        Count = handle_file(T, self()),
        ?assertEqual(2, Count),
        receive
            Result1 ->
                ?assertEqual(<<"Case 1: 2 3">>, iolist_to_binary(Result1))
        after 1000 ->
            ?assert(false)
        end,
        receive
            Result2 ->
                ?assertEqual(<<"Case 2: 1 2">>, iolist_to_binary(Result2))
        after 1000 ->
            ?assert(false)
        end
     end}.

% -----------------------------------------------------------------------------
% Tests for handle_store_record()
% -----------------------------------------------------------------------------
handle_store_record_with_solution_test() ->
    handle_store_record(["12", "4", "1 2 3 10"], self(), 71),
    receive
        Result ->
            ?assertEqual(<<"Case 71: 2 4">>, iolist_to_binary(Result))
    after 1000 ->
        ?assert(false)
    end.

handle_store_record_without_solution_test() ->
    handle_store_record(["13", "5", "1 2 0 10 33"], self(), 72),
    receive
        Result ->
            ?assertEqual(<<"No solution for case #72">>,
                         iolist_to_binary(Result))
    after 1000 ->
        ?assert(false)
    end.
-endif.
