-module(calc).
-export([find_items/2]).
-include("data_defs.hrl").

-ifdef(TEST).
-import(test_helpers, [setup/0, teardown/1]).
-define(DEBUG, true).
-include_lib("eunit/include/eunit.hrl").
-endif.


%% doc find the 2 items that add up to the store credit and send a result
%% message to the given receiver pid.
-spec find_items(Srec :: #storerec{}, Rcvr :: pid()) -> ok.
find_items(Srec, Rcvr) ->
    case do_find_items(Srec#storerec.credit, ei(Srec#storerec.items)) of
        { I1, I2 } ->
            Rcvr ! io_lib:format("Case ~w: ~w ~w", [Srec#storerec.index, I1, I2]);
        nomatch ->
            Rcvr ! "No solution for case #" ++ integer_to_list(Srec#storerec.index)
    end.

%% doc Find the two items whose prices add up to the granted store credit.
%% Return the 1-based indices of the items found or nomatch if there is no
%% solution.
-spec
do_find_items(C :: number(), Is :: [{integer(), number()}])
    -> {integer(), integer()} | nomatch.

% There is no solution for a zero store credit.
do_find_items(0, _) -> nomatch;
% No solution for an empty store (no items).
do_find_items(_, []) -> nomatch;
% No solution for an almost empty store (only one item).
do_find_items(_, [_]) -> nomatch;

do_find_items(C, [{Ih, Ph}|Is]) ->
    Rest = lists:dropwhile(fun({_, Pi}) -> Ph+Pi =/= C end, Is),
    case Rest of
        [] -> do_find_items(C, Is);
        [{Ii, _}|_] -> { Ih, Ii}
    end.


%% doc enumerate the given list.
-spec ei(Is :: [number()]) -> [{integer(), number()}].
ei(Is) -> lists:zip(lists:seq(1, length(Is)), Is).


-ifdef(TEST).
% -----------------------------------------------------------------------------
do_find_items_simple_test() ->
    ?assertEqual(
        {1, 2}, do_find_items(10, ei([2, 8]))).

do_find_items_not_so_simple_test() ->
    ?assertEqual(
        {2, 6}, do_find_items(9, ei([2, 8, 99, 22, 11, 1]))).

do_find_items_no_solution_test() ->
    ?assertEqual(
        nomatch, do_find_items(1, ei([2, 8, 99, 22, 11, 1]))).

do_find_items_single_store_item_test() ->
    ?assertEqual(
        nomatch, do_find_items(2, ei([2]))).

do_find_items_no_store_item_test() ->
    ?assertEqual(
        nomatch, do_find_items(2, ei([]))).

do_find_items_zero_credit_test() ->
    ?assertEqual(
        nomatch, do_find_items(0, [1, 2])).


find_items_simple_test() ->
    find_items(#storerec{index=11, credit=21, items=[3, 18]}, self()),
    receive
        Result ->
            ?assertEqual(<<"Case 11: 1 2">>, iolist_to_binary(Result))
    after 1000 ->
        ?assert(false)
    end.

find_items_not_so_simple_test() ->
    find_items(#storerec{index=12, credit=110, items=[2, 8, 99, 22, 11, 1]}, self()),
    receive
        Result ->
            ?assertEqual(<<"Case 12: 3 5">>, iolist_to_binary(Result))
    after 1000 ->
        ?assert(false)
    end.

find_items_no_solution_test() ->
    find_items(#storerec{index=13, credit=1, items=[2, 8, 99, 22, 11, 1]}, self()),
    receive
        Result ->
            ?assertEqual("No solution for case #13", Result)
    after 1000 ->
        ?assert(false)
    end.

find_items_single_store_item_test() ->
    find_items(#storerec{index=14, credit=2, items=[2]}, self()),
    receive
        Result ->
            ?assertEqual("No solution for case #14", Result)
    after 1000 ->
        ?assert(false)
    end.

find_items_no_store_item_test() ->
    find_items(#storerec{index=15, credit=2, items=[]}, self()),
    receive
        Result ->
            ?assertEqual("No solution for case #15", Result)
    after 1000 ->
        ?assert(false)
    end.

find_items_zero_credit_test() ->
    find_items(#storerec{index=16, credit=0, items=[1,2]}, self()),
    receive
        Result ->
            ?assertEqual("No solution for case #16", Result)
    after 1000 ->
        ?assert(false)
    end.
-endif.
