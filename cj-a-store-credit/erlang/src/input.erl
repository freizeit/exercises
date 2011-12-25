-module(input).
-export([handle_file/3]).

-ifdef(TEST).
-import(test_helpers, [setup/0, teardown/1]).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record(
    storerec, {index :: integer(), credit :: number(), items :: [number()] }).


%% @doc Read input file and spawn a process for each store record found. Return
%% the number of store records processed.
-spec
    handle_file(Path :: string(), Fun :: function(), Receiver :: pid())
    -> integer().
handle_file(Path, Fun, Receiver) ->
    {ok, Fh} = file:open(Path, [read,raw,{read_ahead,1048576}]),
    % ignore first line
    {ok, _} = file:read_line(Fh),
    do_handle_file(Fh, Fun, Receiver, 0).

-spec
    do_handle_file(Fh :: file:io_device(), Fun :: function(),
                   Receiver :: pid(), N :: integer())
    -> integer().
do_handle_file(Fh, _Fun, _Receiver, _N) ->
    {ok, Ls} = read_n_lines(Fh, [], 3),
    parse_storerec(Ls),
    1.

-spec parse_storerec(Ls :: [string()]) -> storerec | {error, string()}.
parse_storerec(_Ls) -> #storerec{}.

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
        {ok, Fh} = file:open(T, [read,raw,binary,{read_ahead,1048576}]),
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
-endif.
