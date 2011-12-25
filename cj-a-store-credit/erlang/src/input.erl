-module(input).
-export([handle_file/2]).
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
-spec
    do_handle_file(Fh :: file:io_device(), Rcvr :: pid(), N :: integer())
    -> integer().
do_handle_file(Fh, Rcvr, N) ->
    case read_n_lines(Fh, [], 3) of
        {ok, [L1,_,L3]} ->
            {C, _} = string:to_integer(L1),
            Is = [P || {P,_} <- lists:map(fun string:to_integer/1, string:tokens(L3, " "))],
            R = #storerec{index=N+1, credit=C, items=Is},
            spawn(calc, find_items, [R, Rcvr]),
            do_handle_file(Fh, Rcvr, N+1);
        eof -> N
    end.


%% @doc Read the next N lines from the given file handle.
-spec
    read_n_lines(Fh :: file:io_device(), Acc :: [string()], N :: integer())
    -> {ok | [string()]} | eof | {error, file:ext_posix()}.
read_n_lines(Fh, Acc, N) ->
    case N > 0 of
        % We are done, reverse the accumulated data and return.
        false -> {ok, lists:reverse(Acc)};
        true ->
            % Try reading another line.
            case file:read_line(Fh) of
                % We got another line, keep going.
                {ok, Data} ->
                    read_n_lines(Fh, [string:strip(Data, right, $\n)|Acc], N-1);
                % Either at the end of file or something broke.
                EofOrError -> EofOrError
            end
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
        ?assertEqual({ok, ["L1", "L2", "L3"]}, Result)
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
        ok = file:write_file(T, string:join(["1", "15", "3", "7 8 3"], "\n")),
        Count = handle_file(T, self()),
        ?assertEqual(1, Count),
        receive
            Result ->
                ?assertEqual(<<"Case 1: 1 2">>, iolist_to_binary(Result))
        after 1000 ->
            ?assert(false)
        end
     end}.
-endif.
