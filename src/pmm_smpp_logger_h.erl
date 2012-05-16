-module(pmm_smpp_logger_h).

-behaviour(gen_event).

-include_lib("kernel/include/file.hrl").

%% API exports
-export([add_to_manager/1, add_to_manager/2, sup_add_to_manager/1,
         sup_add_to_manager/2, activate/2, deactivate/1]).

%% gen_event exports
-export([init/1, terminate/2, handle_call/2, handle_event/2, handle_info/2,
         code_change/3]).

-record(st, {fmt_fun :: fmt_fun(),
             base_dir :: string(),
             base_file_name :: string(),
             max_size :: pos_integer(),
             active = false :: boolean(),
             backlog = [] :: [{in | out, binary()}],
             file_name :: string(),
             fd :: pid(),
             date :: calendar:date(),
             first_entry :: calendar:time(),
             last_entry :: calendar:time(),
             tref :: reference()}).

-type fmt_fun() :: fun((Direction :: in | out, PDU::binary()) -> iodata()).

-define(gv(Key, Params), proplists:get_value(Key, Params)).

-define(FILE_OPTS, [write, raw, binary, append]).
-define(MIDNIGHT_CHECK_INTERVAL, 5000).

%% -------------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------------

-spec add_to_manager(pid()) -> 'ok'.
add_to_manager(SMPPLogMgr) ->
    add_to_manager(SMPPLogMgr, fun pmm_smpp_fmt:format/2).

-spec add_to_manager(pid(), fmt_fun()) -> 'ok'.
add_to_manager(SMPPLogMgr, FmtFun) ->
    gen_event:add_handler(SMPPLogMgr, ?MODULE, [FmtFun]).

-spec sup_add_to_manager(pid()) -> 'ok'.
sup_add_to_manager(SMPPLogMgr) ->
    sup_add_to_manager(SMPPLogMgr, fun pmm_smpp_fmt:format/2).

-spec sup_add_to_manager(pid(), fmt_fun()) -> 'ok'.
sup_add_to_manager(SMPPLogMgr, FmtFun) ->
    gen_event:add_sup_handler(SMPPLogMgr, ?MODULE, [FmtFun]).

-spec activate(pid(), proplists:proplist()) -> 'ok'.
activate(SMPPLogMgr, Params) ->
    gen_event:call(SMPPLogMgr, ?MODULE, {activate, Params}, infinity).

-spec deactivate(pid()) -> 'ok'.
deactivate(SMPPLogMgr) ->
    gen_event:call(SMPPLogMgr, ?MODULE, deactivate, infinity).

%% -------------------------------------------------------------------------
%% gen_event callback functions
%% -------------------------------------------------------------------------

init([FmtFun]) ->
    {ok, #st{fmt_fun = FmtFun}}.

terminate(_Arg, St) ->
    case St#st.fd of
        undefined -> ok;
        _         -> close_and_rename_prev_file(St)
    end.

handle_call({activate, Params}, #st{active = false} = St) ->
    TRef = erlang:start_timer(?MIDNIGHT_CHECK_INTERVAL, self(), midnight_check),
    St1 = St#st{active = true,
                backlog = [],
                tref = TRef,
                base_dir = ?gv(base_dir, Params),
                base_file_name = ?gv(base_file_name, Params),
                max_size = ?gv(max_size, Params)},
    {ok, ok, lists:foldr(fun({Direction, Pdu}, State) ->
                             do_log(Direction, Pdu, State)
                         end, St1, St#st.backlog)};

handle_call(deactivate, #st{active = true} = St) ->
    erlang:cancel_timer(St#st.tref),
    St1 = case St#st.fd of
              undefined -> St;
              _         -> close_and_rename_prev_file(St)
          end,
    {ok, ok, St1#st{tref = undefined, active = false}};

handle_call(Request, _St) ->
    {remove_handler, {unexpected_call, Request}}.

handle_event({pdu, Direction, BinPdu}, #st{active = false} = St) ->
    {ok, St#st{backlog = [{Direction, BinPdu}|St#st.backlog]}};

handle_event({pdu, Direction, BinPdu}, St) ->
    {ok, do_log(Direction, BinPdu, St)};

handle_event(_Event, St) ->
    {ok, St}.

handle_info({timeout, TRef, midnight_check}, #st{tref = TRef} = St) ->
    TRef2 = erlang:start_timer(?MIDNIGHT_CHECK_INTERVAL, self(), midnight_check),
    St1 = St#st{tref = TRef2},
    Date = date(),
    case {St#st.date, St#st.fd} of
        {_, undefined} ->
            {ok, St1};
        {Date, _} ->
            {ok, St1};
        _ ->
            {ok, handle_date_change(Date, St1#st{last_entry = {0, 0, 0}})}
    end;

handle_info(_Info, St) ->
    {ok, St}.

code_change(_OldVsn, St, _Extra) ->
    {ok, St}.

%% -------------------------------------------------------------------------
%% private functions
%% -------------------------------------------------------------------------

do_log(Direction, Pdu, St) ->
    do_log(ensure_dir_and_file, Direction, Pdu, St).

do_log(ensure_dir_and_file, Direction, Pdu, #st{fd = undefined} = St) ->
    {Date, Time} = calendar:local_time(),
    FileName = new_file_name(Date, Time, St),
    file:make_dir(log_dir(Date, St)),
    {ok, Fd} = file:open(FileName, ?FILE_OPTS),
    do_log(write_file, Direction, Pdu,
           St#st{fd = Fd, file_name = FileName, date = Date,
                 first_entry = Time, last_entry = Time});

do_log(ensure_dir_and_file, Direction, Pdu, St) ->
    {Date, Time} = calendar:local_time(),
    St1 = St#st{last_entry = Time},
    case St#st.date of
        Date -> do_log(write_file, Direction, Pdu, St1);
        _    -> do_log(write_file, Direction, Pdu, handle_date_change(Date, St1))
    end;

do_log(write_file, Direction, Pdu, St) ->
    file:write(St#st.fd, (St#st.fmt_fun)(Direction, Pdu)),
    {ok, FileInfo} = file:read_file_info(St#st.file_name),
    case FileInfo#file_info.size >= St#st.max_size of
        true ->
            close_and_rename_prev_file(St),
            NewName = new_file_name(St#st.date, St#st.last_entry, St),
            {ok, Fd} = file:open(NewName, ?FILE_OPTS),
            St#st{fd = Fd, file_name = NewName, first_entry = St#st.last_entry};
        false ->
            St
    end.

handle_date_change(Date, St) ->
    file:close(St#st.fd),
    ClosedName = filename:join(log_dir(St#st.date, St),
                               fmt_time(St#st.first_entry) ++ "_" ++
                               fmt_time({23, 59, 59}) ++ "_" ++
                               St#st.base_file_name),
    file:rename(St#st.file_name, ClosedName),
    NewName = new_file_name(Date, {0, 0, 0}, St),
    file:make_dir(log_dir(Date, St)),
    {ok, Fd} = file:open(NewName, ?FILE_OPTS),
    St#st{fd = Fd, file_name = NewName, date = Date, first_entry = {0, 0, 0}}.

close_and_rename_prev_file(St) ->
    file:close(St#st.fd),
    ClosedName = filename:join(log_dir(St#st.date, St),
                               fmt_time(St#st.first_entry) ++ "_" ++
                               fmt_time(St#st.last_entry)  ++ "_" ++
                               St#st.base_file_name),
    file:rename(St#st.file_name, ClosedName),
    St#st{file_name = undefined, fd = undefined}.

new_file_name(Date, Time, St) ->
    filename:join(log_dir(Date, St),
                  fmt_time(Time) ++ "_present_" ++ St#st.base_file_name).

log_dir(Date, St) ->
    filename:join(St#st.base_dir, fmt_date(Date)).

fmt_date({Y, M, D}) ->
    lists:flatten(io_lib:format("~w-~2..0w-~2..0w", [Y, M, D])).

fmt_time({H, M, S}) ->
    lists:flatten(io_lib:format("~2..0w~2..0w~2..0w", [H, M, S])).
