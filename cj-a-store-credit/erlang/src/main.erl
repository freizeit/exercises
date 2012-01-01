%% @author Muharem Hrnjadovic
%% @doc Solves the code jam practice problem described here:
%%          http://code.google.com/codejam/contest/dashboard?c=351101#s=p0
%%
%%      You receive a credit C at a local store and would like to buy
%%      two items. You first walk through the store and create a
%%      list L of all available items. From this list you would like to
%%      buy two items that add up to the entire value of the credit. The
%%      solution you provide will consist of the two integers indicating
%%      the positions of the items in your list (smaller number first).

-module(main).
-export([main/1]).
-import(input, [process_data/2]).

-ifdef(TEST).
-import(test_helpers, [setup/0, teardown/1]).
-define(DEBUG, true).
-include_lib("eunit/include/eunit.hrl").
-endif.


%% doc The main() function, triggers the calculation.
-spec main(Path :: string()) -> integer().
main(Path) ->
    % A crude approximation of maxint. Good enough for the purpose at hand.
    Maxint = lists:last(erlang:system_info(heap_sizes)),
    spawn(input, process_data, [Path, self()]),
    print_solutions(fun (S) -> io:format("~s~n", [S]) end, Maxint, 1).


%% doc Prints the results. The actual number of results is
%% communicated via the {count, C} message.
-spec
    print_solutions(Fun :: function(), Count :: integer(), Idx :: integer())
    -> integer().
print_solutions(Fun, Count, Idx) ->
    if
        Idx =< Count ->
            receive
                {count, C} -> print_solutions(Fun, C, Idx);
                {res, S} -> Fun(S), print_solutions(Fun, Count, Idx+1)
            end;
        true -> Count
    end.










-ifdef(TEST).
% -----------------------------------------------------------------------------
% -----------------------------------------------------------------------------
% -----------------------------------------------------------------------------
% Tests for print_solutions()
% -----------------------------------------------------------------------------
printer_terminates_test() ->
    Me = self(),
    FakeFun = fun(S) -> Me ! S end,
    P = spawn(?MODULE, print_solutions, [FakeFun, 33, 1]),
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
