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
    Maxint = lists:last(erlang:system_info(heap_sizes)),
    Rcvr = spawn(?MODULE, printer, [fun io:format/1, Maxint, 1]),
    input:handle_file(Path, Rcvr),
    done.


%% doc Prints the results. The actual number of results is
%% communicated via the {count, C} message.
-spec
    printer(Fun :: function(), Count :: integer(), Idx :: integer())
    -> integer().
printer(Fun, Count, Idx) ->
    if
        Idx =< Count ->
            receive
                {count, C} -> printer(Fun, C, Idx);
                {res, S} -> Fun(S), printer(Fun, Count, Idx+1)
            end;
        true -> Count
    end.










-ifdef(TEST).
% -----------------------------------------------------------------------------
% -----------------------------------------------------------------------------
% -----------------------------------------------------------------------------
% Tests for printer()
% -----------------------------------------------------------------------------
printer_terminates_test() ->
    Me = self(),
    FakeFun = fun(S) -> Me ! S end,
    P = spawn(?MODULE, printer, [FakeFun, 33, 1]),
    timer:sleep(50),
    P ! { res, "R1" },
    P ! { res, "R3" },
    P ! { count, 3 },
    P ! { res, "R2" },
    receive "R1" -> ?assert(true)
    after 1000 -> ?assert(false)
    end,
    receive "R3" -> ?assert(true)
    after 1000 -> ?assert(false)
    end,
    receive "R2" -> ?assert(true)
    after 1000 -> ?assert(false)
    end,
    timer:sleep(50),
    ?assert(not is_process_alive(P)).
-endif.
