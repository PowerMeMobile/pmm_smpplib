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
             counter :: non_neg_integer(),
             max_size :: pos_integer(),
             active = false :: boolean(),
             backlog = [] :: [{in | out, binary()}],
             file_name :: string(),
             fd :: pid(),
             date :: calendar:date(),
             first_entry :: calendar:time(),
             last_entry :: calendar:time(),
             file_info_check_rate :: pos_integer(),
             delayed_write_bytes :: pos_integer(),
             delayed_write_mseconds :: pos_integer(),
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
    {ok, FileInfoCheckRate} = application:get_env(pmm_smpplib, file_info_check_rate, 100),
    {ok, DelayedWriteBytes} = application:get_env(pmm_smpplib, delayed_write_bytes, 1048576),
    {ok, DelayedWriteMilSeconds} =application:get_env(pmm_smpplib, delayed_write_mseconds, 5000),
    error_logger:info_report([
        {self, self()},
        {file_info_check_rate, FileInfoCheckRate},
        {delayed_write_bytes, DelayedWriteBytes},
        {delayed_write_mseconds, DelayedWriteMilSeconds}
    ]),
    St = #st{
        fmt_fun = FmtFun,
        file_info_check_rate = FileInfoCheckRate,
        delayed_write_bytes = DelayedWriteBytes,
        delayed_write_mseconds = DelayedWriteMilSeconds
    },
    {ok, St}.

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
                counter = 0,
                max_size = ?gv(max_size, Params)},
    FoldFun = fun({Direction, Pdu}, State) ->
        do_log(Direction, Pdu, undefined, State)
    end,
    NewSt = lists:foldr(FoldFun, St1, St#st.backlog),
    {ok, ok, NewSt};

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
    {ok, do_log(Direction, BinPdu, undefined, St)};

handle_event({pdu, Direction, BinPdu, Timestamp}, St) ->
    {ok, do_log(Direction, BinPdu, Timestamp, St)};

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

do_log(Direction, Pdu, Timestamp, St) ->
    do_log(ensure_dir_and_file, Direction, Pdu, Timestamp, St).

do_log(ensure_dir_and_file, Direction, Pdu, Timestamp, #st{fd = undefined} = St) ->
    {Date, Time} = calendar:local_time(),
    FileName = new_file_name(Date, Time, St),
    file:make_dir(log_dir(Date, St)),
    {ok, Fd} = file:open(FileName, get_file_opts(?FILE_OPTS, St)),
    do_log(write_file, Direction, Pdu, Timestamp,
           St#st{fd = Fd, file_name = FileName, date = Date,
                 first_entry = Time, last_entry = Time});

do_log(ensure_dir_and_file, Direction, Pdu, Timestamp, St) ->
    {Date, Time} = calendar:local_time(),
    St1 = St#st{last_entry = Time},
    case St#st.date of
        Date -> do_log(write_file, Direction, Pdu, Timestamp, St1);
        _    -> do_log(write_file, Direction, Pdu, Timestamp, handle_date_change(Date, St1))
    end;

do_log(write_file, Direction, Pdu, Timestamp0, St) ->
    Timestamp =
    case Timestamp0 of
        undefined ->  os:timestamp();
        _ -> Timestamp0
    end,
    FmtFun = St#st.fmt_fun,
    Formatted =
    if
        is_function(FmtFun, 2) ->
            FmtFun(Direction, Pdu);
        is_function(FmtFun, 3) ->
            FmtFun(Direction, Pdu, Timestamp)
    end,
    case file:write(St#st.fd, Formatted) of
        {error, badarg} ->
            PduHex = [io_lib:format("~2..0s", [integer_to_binary(I, 16)]) || <<I>> <= Pdu],
            {_, _, MicroSecs} = Timestamp,
            {{Y, Mon, D}, {H, Min, S}} = calendar:now_to_local_time(Timestamp),
            Time = io_lib:format("~2..0w~2..0w~2..0w ~2..0w:~2..0w:~2..0w.~3..0w",
                   [Y rem 100, Mon, D, H, Min, S, MicroSecs div 1000]),
            Msg = [$\n, Time, " Corrupted PDU hex: ", PduHex, $\n],
            file:write(St#st.fd, Msg);
        _ -> ok
    end,

    Counter = St#st.counter,
    case Counter rem St#st.file_info_check_rate =:= 0 of
        true ->
            {ok, FileInfo} = file:read_file_info(St#st.file_name),
            case FileInfo#file_info.size >= St#st.max_size of
                true ->
                    close_and_rename_prev_file(St),
                    NewName = new_file_name(St#st.date, St#st.last_entry, St),
                    {ok, Fd} = file:open(NewName, get_file_opts(?FILE_OPTS, St)),
                    St#st{fd = Fd, file_name = NewName, first_entry = St#st.last_entry, counter = 0};
                _ ->
                    St#st{counter = Counter + 1}
            end;
        _ ->
            St#st{counter = Counter + 1}
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
    {ok, Fd} = file:open(NewName, (?FILE_OPTS)),
    St#st{fd = Fd, file_name = NewName, date = Date, first_entry = {0, 0, 0}, counter = 0}.

close_and_rename_prev_file(St) ->
    file:close(St#st.fd),
    ClosedName = filename:join(log_dir(St#st.date, St),
                               fmt_time(St#st.first_entry) ++ "_" ++
                               fmt_time(St#st.last_entry)  ++ "_" ++
                               St#st.base_file_name),
    file:rename(St#st.file_name, ClosedName),
    St#st{file_name = undefined, fd = undefined, counter = 0}.

new_file_name(Date, Time, St) ->
    filename:join(log_dir(Date, St),
                  fmt_time(Time) ++ "_present_" ++ St#st.base_file_name).

log_dir(Date, St) ->
    filename:join(St#st.base_dir, fmt_date(Date)).

fmt_date({Y, M, D}) ->
    lists:flatten(io_lib:format("~w-~2..0w-~2..0w", [Y, M, D])).

fmt_time({H, M, S}) ->
    lists:flatten(io_lib:format("~2..0w~2..0w~2..0w", [H, M, S])).


get_file_opts(Default, St) ->
    [{delayed_write, St#st.delayed_write_bytes, St#st.delayed_write_mseconds} | Default].
