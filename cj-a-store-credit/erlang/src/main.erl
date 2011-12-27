-module(main).
-export([main/1, printer/2]).
-import(input, [handle_file/2]).

-ifdef(TEST).
-import(test_helpers, [setup/0, teardown/1]).
-define(DEBUG, true).
-include_lib("eunit/include/eunit.hrl").
-endif.


%% doc The main() function, triggers the calculation.
-spec main(Path :: string()) -> none().
main(Path) -> nil.


%% doc Prints the results.
-spec printer(Max :: integer(), Count :: integer()) -> done.
printer(N, N) -> done;
printer(Max, Count) ->
    receive
        {max, V} -> printer(V, Count);
        {res, S} -> io:format(S), printer(Max, Count+1)
    end.
