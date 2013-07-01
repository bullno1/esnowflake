-module(esnowflake_tests).
-include_lib("eunit/include/eunit.hrl").

uniqueness_test_() ->
	{setup, spawn,
	 fun() ->
		application:start(emisc),
		application:start(esnowflake)
	 end,
	 fun(_) ->
		application:stop(esnowflake),
		application:stop(emisc)
	 end,
	 ?_test(
		?LET(Ids, [esnowflake:new_id() || _ <- lists:seq(0, 2000)],
			?assertEqual(Ids, lists:usort(Ids))))}.
