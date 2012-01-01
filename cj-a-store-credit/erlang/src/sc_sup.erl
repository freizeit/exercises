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

-module(sc_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    % Children = [CHILD(sc_server, worker)],
    {ok, { {one_for_one, 5, 10}, []} }.
