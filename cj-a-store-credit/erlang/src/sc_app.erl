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

-module(sc_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    sc_sup:start_link().

stop(_State) ->
    ok.
