%% Copyright (c) 2011-2013 Basho Technologies, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.

%% @doc A simple gen_event backend used to monitor mailbox size and
%% switch log messages between synchronous and asynchronous modes.
%% A gen_event handler is used because a process getting its own mailbox
%% size doesn't involve getting a lock, and gen_event handlers run in their
%% parent's process.

-module(pmm_smpplib_log_throttle).

-behaviour(gen_event).

-export([
    init_configuration/0,
    add_handler/3,
    add_handler/4,
    log/2
]).

-export([
    init/1,
    handle_call/2,
    handle_event/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {
    hwm                :: non_neg_integer(),
    window_min         :: non_neg_integer(),
    async = true       :: boolean(),
    sync_event_handler :: function()
}).

%% ===================================================================
%% API
%% ===================================================================

-spec init_configuration() -> ok.
init_configuration() ->
    Props = [named_table, public, set, {keypos, 1}, {read_concurrency, true}],
    _ = try ets:new(?MODULE, Props) of
        _Result ->
            ok
    catch
        error:badarg -> ok
    end.


-spec log(Logger, Event) -> ok when
    Logger :: pid() | undefined,
    Event :: {pdu, Direction :: in | out, BinPdu :: binary()}.
log(undefined, _) -> ok;
log(Logger, {pdu, Direction, BinPdu}) when is_pid(Logger) ->
    Event = {pdu, Direction, BinPdu, os:timestamp()},
    case is_sync_mode(Logger) of
        false ->
            gen_event:notify(Logger, Event);
        true ->
            gen_event:sync_notify(Logger, Event)
    end.


-spec add_handler(Logger, Hwm, Window) -> ok when
    Logger :: pid(),
    Hwm :: non_neg_integer(),
    Window :: non_neg_integer().
add_handler(Logger, Hwm, Window) ->
    add_handler(Logger, Hwm, Window, fun() -> ok end).

-spec add_handler(Logger, Hwm, Window, SyncEventHandler) -> ok when
    Logger :: pid(),
    Hwm :: non_neg_integer(),
    Window :: non_neg_integer(),
    SyncEventHandler :: function().
add_handler(Logger, Hwm, Window, SyncEventHandler) when
        is_pid(Logger) andalso
        is_integer(Hwm) andalso Hwm > 0 andalso
        is_integer(Window) andalso Window > 0 andalso
        is_function(SyncEventHandler) ->
    gen_event:add_handler(Logger, ?MODULE, [Hwm, Window, SyncEventHandler]).

%% ===================================================================
%% Callbacks
%% ===================================================================

init([Hwm, Window, SyncEventHandler]) ->
    {ok, #state{
        hwm = Hwm,
        window_min = Hwm - Window,
        sync_event_handler = SyncEventHandler}}.


handle_call(_Request, State) ->
    {ok, ok, State}.

handle_event({pdu, Direction, BinPdu, _Timestamp}, St = #state{}) ->
    handle_event({pdu, Direction, BinPdu}, St);

handle_event({pdu, _Direction, _BinPdu}, State) ->
    {message_queue_len, Len} = erlang:process_info(self(), message_queue_len),
    case {Len > State#state.hwm, Len < State#state.window_min, State#state.async} of
        {true, _, true} ->
            set_sync_mode(self()),
            catch((State#state.sync_event_handler)()),
            {ok, State#state{async=false}};
        {_, true, false} ->
            set_async_mode(self()),
            {ok, State#state{async=true}};
        _ ->
            %% nothing needs to change
            {ok, State}
    end;
handle_event(_Event, State) ->
    {ok, State}.


handle_info(_Info, State) ->
    {ok, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% Internals
%% ===================================================================

set_sync_mode(Logger) when is_pid(Logger) ->
    ets:insert(?MODULE, {Logger, true}),
    ok.

set_async_mode(Logger) when is_pid(Logger) ->
    ets:insert(?MODULE, {Logger, false}),
    ok.

is_sync_mode(Logger) when is_pid(Logger) ->
    case ets:lookup(?MODULE, Logger) of
        [] ->
            false;
        [{Logger, Res}] ->
            Res
    end.
