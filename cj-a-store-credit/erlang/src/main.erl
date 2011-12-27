-module(main).
-export([main/1, printer/3]).
-import(input, [handle_file/2]).

-ifdef(TEST).
-import(test_helpers, [setup/0, teardown/1]).
-define(DEBUG, true).
-include_lib("eunit/include/eunit.hrl").
-endif.


%% doc The main() function, triggers the calculation.
-spec main(Path :: string()) -> done.
main(Path) ->
    Rcvr = spawn(?MODULE, printer, [fun io:format/1, -1, 1]),
    input:handle_file(Path, Rcvr),
    done.


%% doc Prints the results.
-spec printer(Fun :: function(), Max :: integer(), Count :: integer()) -> done.
printer(_, N, N) -> done;
printer(Fun, Max, Count) ->
    receive
        {max, V} -> printer(Fun, V, Count);
        {res, S} -> Fun(S), printer(Fun, Max, Count+1)
    end.










-ifdef(TEST).
% -----------------------------------------------------------------------------
% -----------------------------------------------------------------------------
% -----------------------------------------------------------------------------
% Tests for printer()
% -----------------------------------------------------------------------------
printer_exit_condition_test() ->
    ?assertEqual(done, printer(fun erlang:exit/1, 11, 11)).

printer_exit_condition_test() ->
-endif.
