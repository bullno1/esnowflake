-module(esnowflake).
-behaviour(gen_server).
-export([start_link/1]).
-export([new_id/0]).
-export([id_to_string/1, string_to_id/1]).
-export([id_to_bstring/1, bstring_to_id/1]).
-export([id_timestamp/1]).
-export([init/1, handle_cast/2, handle_call/3,
         handle_info/2, terminate/2, code_change/3]).
-export_type([id/0]).

-type id() :: <<_:64>>.
-define(MAX_SEQ, 4095).%2^12 - 1

-record(state, {
	last_time,
	worker_id,
	epoch,
	sequence = 0
}).

-spec start_link(string()) -> {ok, pid()} | {error, Reason :: any()}.
start_link(Opts) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

-spec new_id() -> id().
new_id() ->
	case gen_server:call(?MODULE, new_id) of
		{ok, Id} -> Id;
		{error, Error} ->
			error(Error)
	end.

-spec id_to_string(id()) -> string().
id_to_string(<<Int:64/integer>>) ->
	em_base62:encode(Int).

-spec string_to_id(string()) -> id().
string_to_id(Str) ->
	Int = em_base62:decode(Str),
	<<Int:64/integer>>.

-spec id_to_bstring(id()) -> binary().
id_to_bstring(Id) -> list_to_binary(id_to_string(Id)).

-spec bstring_to_id(binary()) -> id().
bstring_to_id(Str) -> string_to_id(binary_to_list(Str)).

-spec id_timestamp(id()) -> integer().
id_timestamp(<<Timestamp:42/integer, _:22/bitstring>>) ->
	Timestamp + ets:lookup_element(esnowflake, epoch, 2).

%% Private
init(Opts) ->
	{worker_id, WorkerId} = proplists:lookup(worker_id, Opts),
	{epoch, Epoch} = proplists:lookup(epoch, Opts),
	ets:new(esnowflake, [named_table, protected]),
	ets:insert(esnowflake, [{epoch, Epoch}]),
	{ok, #state{
		last_time = em_time:current_time_ms() - Epoch,
		worker_id = WorkerId,
		epoch = Epoch
	}}.

terminate(_Reason, _State) -> ok.

handle_call(new_id, _From,
			#state{last_time = LastTime,
			       worker_id = WorkerId,
			       sequence = Seq,
			       epoch = Epoch} = State) ->
	{Result, NewState} = make_id(Epoch, LastTime, WorkerId, Seq, State),
	{reply, Result, NewState}.

handle_cast(_Req, State) ->	{stop, invalid_cmd, State}.

handle_info(Msg, State) ->
	error_logger:warning_report(["Unrecognized msg", {msg, Msg}]),
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

make_id(Epoch, LastTime, WorkerId, Seq, State) ->
	CurrTime = em_time:current_time_ms() - Epoch,
	if
		%Time has changed
		CurrTime > LastTime ->
			NewSeq = 0,
			Id = pack_id(CurrTime, WorkerId, NewSeq),
			NewState = State#state{
				sequence = NewSeq,
				last_time = CurrTime
			},
			{{ok, Id}, NewState};
		%Time hasn't changed, max sequence reached
		CurrTime =:= LastTime, Seq =:= ?MAX_SEQ ->
			timer:sleep(10),
			make_id(Epoch, LastTime, WorkerId, Seq, State);
		%Time hasn't changed, max sequence not reached
		CurrTime =:= LastTime, Seq < ?MAX_SEQ ->
			NewSeq = Seq + 1,
			Id = pack_id(CurrTime, WorkerId, NewSeq),
			NewState = State#state{sequence = NewSeq},
			{{ok, Id}, NewState};
		CurrTime < LastTime ->
			{{error, time_flowed_backward}, State}
	end.

pack_id(CurrTime, WorkerId, Seq) -> <<CurrTime:42/integer, WorkerId:10/integer, Seq:12/integer>>.
