-module(test_helpers).
-ifdef(TEST).
-export([setup/0, teardown/1]).
-include_lib("eunit/include/eunit.hrl").
-endif.


-ifdef(TEST).
%% @doc create a temporary file and write the given lines to it. Return the
%% full path to the temporary file. The caller is responsible for disposing of
%% it.
-spec setup() -> string().
setup() -> string:strip(os:cmd("mktemp"), right, $\n).


%% @doc Removes the given path and any sub-directories or files it contains.
-spec teardown(T :: string()) -> string().
teardown(T) -> os:cmd("rm -rf " ++ T).
-endif.
