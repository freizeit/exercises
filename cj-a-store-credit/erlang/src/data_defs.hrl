%% @type storerec() = #storerec{ index = integer(),
%%                               credit = number(),
%%                               items = [number()] }.
-record(storerec, { index=0, credit=0, items=[] }).
