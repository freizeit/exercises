-module(input).
-export([handle_file/3]).
-define(NOTEST, true).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-export([read_n_lines/3]).
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
