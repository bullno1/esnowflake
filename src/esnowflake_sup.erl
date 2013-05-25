-module(esnowflake_sup).
-behaviour(supervisor).
-export([init/1]).
-export([start_link/0]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Opts = application:get_all_env(esnowflake),
	{ok, {{one_for_one, 5, 60}, [
		{esnowflake,
			{esnowflake, start_link, [Opts]},
			permanent,
			1000,
			worker,
			[esnowflake]}
	]}}.
