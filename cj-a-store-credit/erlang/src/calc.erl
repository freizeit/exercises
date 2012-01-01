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
            R = io_lib:format("Case #~w: ~w ~w", [Srec#storerec.index, I1, I2]);
        nomatch ->
            R = "No solution for case #" ++ integer_to_list(Srec#storerec.index)
    end,
    Rcvr ! {res, R}.


%% doc Find the two items whose prices add up to the granted store credit.
%% Return the 1-based indices of the items found or 'nomatch' if there is no
%% solution.
-spec
    do_find_items(C :: number(), Is :: [{integer(), number()}])
    -> {integer(), integer()} | nomatch.

% There is no solution for a zero store credit.
do_find_items(0, _) -> nomatch;
% No solution for an empty store (no items).
do_find_items(_, []) -> nomatch;
% No solution for an almost empty store (we need at least 2 items).
do_find_items(_, [_]) -> nomatch;

do_find_items(C, [{Idx, Price}|Is]) ->
    % We take the first store item in the list, then go through the
    % tail/remaining items until we find a match or until the latter are
    % exhausted.
    Rest = lists:dropwhile(
        fun({_, Other_price}) -> Price + Other_price =/= C end, Is),
    case Rest of
        % No match found.
        [] -> do_find_items(C, Is);
        % Match found, return the indices of the 2 store items
        % whose prices add to the granted store credit.
        [{Other_idx, _}|_] -> { Idx, Other_idx}
    end.


%% doc enumerate the given list.
-spec ei(Is :: [number()]) -> [{integer(), number()}].
ei(Is) -> lists:zip(lists:seq(1, length(Is)), Is).










-ifdef(TEST).
% -----------------------------------------------------------------------------
% -----------------------------------------------------------------------------
% -----------------------------------------------------------------------------
% Tests for do_find_items()
% -----------------------------------------------------------------------------
do_find_items_simple_test() ->
    ?assertEqual({1, 2}, do_find_items(10, ei([2, 8]))).

do_find_items_not_so_simple_test() ->
    ?assertEqual({2, 6}, do_find_items(9, ei([2, 8, 99, 22, 11, 1]))).

do_find_items_no_solution_test() ->
    ?assertEqual(nomatch, do_find_items(1, ei([2, 8, 99, 22, 11, 1]))).

do_find_items_single_store_item_test() ->
    ?assertEqual(nomatch, do_find_items(2, ei([2]))).

do_find_items_no_store_item_test() ->
    ?assertEqual(nomatch, do_find_items(2, ei([]))).

do_find_items_zero_credit_test() ->
    ?assertEqual(nomatch, do_find_items(0, [1, 2])).


% -----------------------------------------------------------------------------
% Tests for find_items()
% -----------------------------------------------------------------------------
find_items_simple_test() ->
    find_items(#storerec{index=11, credit=21, items=[3, 18]}, self()),
    receive
        {res, Result} ->
            ?assertEqual(<<"Case #11: 1 2">>, iolist_to_binary(Result))
    after 1000 ->
        ?assert(false)
    end.

find_items_not_so_simple_test() ->
    find_items(#storerec{index=12, credit=110, items=[2, 8, 99, 22, 11, 1]}, self()),
    receive
        {res, Result} ->
            ?assertEqual(<<"Case #12: 3 5">>, iolist_to_binary(Result))
    after 1000 ->
        ?assert(false)
    end.

find_items_no_solution_test() ->
    find_items(#storerec{index=13, credit=1, items=[2, 8, 99, 22, 11, 1]}, self()),
    receive
        {res, Result} ->
            ?assertEqual("No solution for case #13", Result)
    after 1000 ->
        ?assert(false)
    end.

find_items_single_store_item_test() ->
    find_items(#storerec{index=14, credit=2, items=[2]}, self()),
    receive
        {res, Result} ->
            ?assertEqual("No solution for case #14", Result)
    after 1000 ->
        ?assert(false)
    end.

find_items_no_store_item_test() ->
    find_items(#storerec{index=15, credit=2, items=[]}, self()),
    receive
        {res, Result} ->
            ?assertEqual("No solution for case #15", Result)
    after 1000 ->
        ?assert(false)
    end.

find_items_zero_credit_test() ->
    find_items(#storerec{index=16, credit=0, items=[1,2]}, self()),
    receive
        {res, Result} ->
            ?assertEqual("No solution for case #16", Result)
    after 1000 ->
        ?assert(false)
    end.
-endif.
