-module(pmm_smpp_fmt2).

-include_lib("oserl/include/oserl.hrl").

-export([format/5]).

-ifdef(TEST).
-compile(export_all).
-endif.

%% -------------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------------

-type time_locale() :: 'local' | 'universal'.
-type direction() :: in | out.
-type err_fun() :: fun((pos_integer()) -> string()).

-spec format(direction(), binary(), err_fun(), time_locale(), boolean()) -> iolist().
format(Direction, BinPdu, ErrFun, TimeLocale, LogShortMsgAndUdhHex) ->
    {ok, {_CmdId, _Status, _SeqNum, _Body} = Pdu} = smpp_operation:unpack(BinPdu),
    format(Pdu, Direction, BinPdu, ErrFun, TimeLocale, LogShortMsgAndUdhHex).

format({CmdId, _Status, _SeqNum, _Body} = Pdu, Direction, BinPdu, _ErrFun, TimeLocale, _LogShortMsgAndUdhHex) when
        CmdId =:= ?COMMAND_ID_ENQUIRE_LINK orelse
        CmdId =:= ?COMMAND_ID_ENQUIRE_LINK_RESP orelse
        CmdId =:= ?COMMAND_ID_SUBMIT_SM orelse
        CmdId =:= ?COMMAND_ID_SUBMIT_SM_RESP orelse
        CmdId =:= ?COMMAND_ID_DELIVER_SM orelse
        CmdId =:= ?COMMAND_ID_DELIVER_SM_RESP orelse
        CmdId =:= ?COMMAND_ID_BIND_RECEIVER orelse
        CmdId =:= ?COMMAND_ID_BIND_TRANSMITTER orelse
        CmdId =:= ?COMMAND_ID_BIND_TRANSCEIVER orelse
        CmdId =:= ?COMMAND_ID_BIND_RECEIVER_RESP orelse
        CmdId =:= ?COMMAND_ID_BIND_TRANSMITTER_RESP orelse
        CmdId =:= ?COMMAND_ID_BIND_TRANSCEIVER_RESP ->

    Banner = get_banner2(Direction, CmdId, TimeLocale),
    [
    $\n,
    get_first_line(Pdu, BinPdu, Banner),
    get_second_line(Pdu, BinPdu, Banner),
    get_third_line(Pdu, BinPdu, Banner)
    ];

format({CmdId, Status, SeqNum, _Body} = Pdu, Direction, BinPdu, ErrFun, TimeLocale, LogShortMsgAndUdhHex) ->
    Level = case Status of
                ?ESME_ROK -> "info";
                _         -> "error"
            end,
    Banner = banner(Direction, Level, TimeLocale),
    Details = case Level of
                  "error" -> [Banner, "error: ", ErrFun(Status), "\n"];
                  "info"  -> ""
              end,
    Size = size(BinPdu),

    Params0 = [{command_length, Size}|smpp_operation:to_list(Pdu)],

    {Params, ShortMessageHex} =
        get_short_message_and_udh_hex(CmdId, LogShortMsgAndUdhHex, Banner, Params0),

    [$\n,
     Banner, cmdname(CmdId), " ", integer_to_list(SeqNum), $\n,
     Details,
     Banner, "hex dump (", integer_to_list(Size), " bytes):", $\n,
     hexdump(BinPdu, Banner),
     ShortMessageHex,
     Banner, "params:", $\n,
     params(CmdId, Params, Banner), $\n].

%% -------------------------------------------------------------------------
%% private functions
%% -------------------------------------------------------------------------

banner(Direction, Level, TimeLocale) ->
    {_, _, MicroSecs} = Now = os:timestamp(),
    {{Y, Mon, D}, {H, Min, S}} =
        case TimeLocale of
            local     -> calendar:now_to_local_time(Now);
            universal -> calendar:now_to_universal_time(Now)
        end,
    Arrow = case Direction of in -> $<; out -> $> end,
    [io_lib:format("~2..0w~2..0w~2..0w ~2..0w:~2..0w:~2..0w.~3..0w",
                   [Y rem 100, Mon, D, H, Min, S, MicroSecs div 1000]),
     " ", Arrow, " [", Level, "] "].

cmdname(CmdId) ->
    string:to_upper(atom_to_list(?COMMAND_NAME(CmdId))).

hexdump(Bin, Banner) ->
    lists:map(fun(Row) ->
                  [Banner,
                   interpose(lists:map(fun hexify/1, slice_binary(Row, 4)), $:),
                   $\n]
              end,
              slice_binary(Bin, 16)).

slice_binary(Bin, Size) ->
    slice_binary(Bin, Size, []).

slice_binary(Bin, Size, Acc) ->
    case size(Bin) of
        0 ->
            lists:reverse(Acc);
        N when N =< Size ->
            lists:reverse(Acc, [Bin]);
        _ ->
            {Bin1, Bin2} = split_binary(Bin, Size),
            slice_binary(Bin2, Size, [Bin1|Acc])
    end.

hexify(Bin) ->
    hexify(Bin, []).

hexify(<<>>, Acc) ->
    lists:reverse(Acc);
hexify(<<N, Rest/binary>>, Acc) ->
    hexify(Rest, [integer_to_hexlist(N)|Acc]).


integer_to_hexlist(Int) when Int >= 0 andalso Int =< 255 ->
    case integer_to_list(Int, 16) of
        [HexSymbolCP] -> [$0, HexSymbolCP];
        Other -> Other
    end.


interpose([], _Sep) ->
    [];
interpose(List, Sep) ->
    tl(lists:foldr(fun(Item, Acc) -> [Sep, Item|Acc] end, [], List)).

params(CmdId, Params, Banner) ->
    BannerLen = lists:flatlength(Banner),
    {_, Result} =  lists:foldl(
        fun({K, V}, {Len, [H|T]}) ->
            V1 = case is_tuple(V) of
                    true -> io_lib:format("~p", [V]);
                    false -> V
            end,
            KV = lists:concat([K, "=", V1]),
            KVLen = length(KV),
            if
                Len =:= 0 ->
                    {BannerLen + KVLen, [[Banner, KV]|T]};
                Len + KVLen =< 90 ->
                    {Len + KVLen + 1, [[H, $,, KV]|T]};
                true ->
                    {BannerLen + KVLen, [[Banner, KV], [H, ",\n"]|T]}
            end
        end,
        {0, [[]]}, unfold_multi(CmdId, Params)),
    lists:reverse(Result).

%% unfold composite params of submit_multi_req and submit_multi_resp.
unfold_multi(?COMMAND_ID_SUBMIT_MULTI, Params) ->
    {value, {_, Dests}, Params1} = lists:keytake(dest_address, 1, Params),
    IndexedDests = case Dests of
                       [] -> [];
                       _  -> lists:zip(Dests, lists:seq(0, length(Dests) - 1))
                   end,
    Unfolded =
        lists:map(fun({{_, DestFlag, TON, NPI, Addr}, Idx}) ->
                      Key = lists:concat(["[", Idx, "]dest_flag"]),
                      Val = lists:concat([DestFlag, ",dest_addr_ton=", TON,
                                          ",dest_addr_npi=", NPI,
                                          ",destination_addr=", Addr]),
                      {Key, Val}
                  end, IndexedDests),
    Params1 ++ Unfolded;
unfold_multi(?COMMAND_ID_SUBMIT_MULTI_RESP, Params) ->
    {value, {_, Failed}, Params1} = lists:keytake(unsuccess_sme, 1, Params),
    IndexedFailed = case Failed of
                        [] -> [];
                        _  -> lists:zip(Failed, lists:seq(0, length(Failed) - 1))
                    end,
    Unfolded =
        lists:map(fun({{_, TON, NPI, Addr, Error}, Idx}) ->
                      Key = lists:concat(["[", Idx, "]dest_addr_ton"]),
                      Val = lists:concat([TON, ",dest_addr_npi=", NPI,
                                          ",destination_addr=", Addr,
                                          ",error_status_code=", Error]),
                      {Key, Val}
                  end, IndexedFailed),
    Params1 ++ Unfolded;
unfold_multi(_CmdId, Params) ->
    Params.

get_short_message_and_udh_hex(CmdId, true, Banner, Params0) when
        CmdId =:= ?COMMAND_ID_SUBMIT_SM orelse CmdId =:= ?COMMAND_ID_DELIVER_SM ->

   {value, {short_message, ShortMsg}, _} =
       lists:keytake(short_message, 1, Params0),

    HasUDH = smpp_sm:udhi(Params0),
    Params =
    case HasUDH of
        true ->
            {UDH, _Rest} = smpp_sm:chop_udh(ShortMsg),
            [{udh, hexify(list_to_binary(UDH))} | Params0];
        false -> Params0
    end,

    DataCoding = proplists:get_value(data_coding, Params),
    ShortMsgHex =
    case lists:member(DataCoding band 15, [0,1,3]) of
        false ->
            ShortMsgBin = list_to_binary(ShortMsg),
            [Banner, "short_message (", integer_to_list(size(ShortMsgBin)), " bytes):", $\n,
             hexdump(ShortMsgBin, Banner)];
        true -> ""
    end,
    {Params, ShortMsgHex};
get_short_message_and_udh_hex(_, _, _, Params) -> {Params, ""}.


get_first_line({CmdId, _Status, SeqNum, _Body} = _Pdu, BinPdu, Banner) when
        CmdId =:= ?COMMAND_ID_SUBMIT_SM orelse
        CmdId =:= ?COMMAND_ID_DELIVER_SM orelse
        CmdId =:= ?COMMAND_ID_ENQUIRE_LINK orelse
        CmdId =:= ?COMMAND_ID_ENQUIRE_LINK_RESP ->

    PduHex = hexify(BinPdu),
    [Banner, cmdname(CmdId), " ", integer_to_list(SeqNum), ",hex=", PduHex, $\n];

get_first_line({CmdId, Status, SeqNum, _Body} = Pdu, BinPdu, Banner) when
        CmdId =:= ?COMMAND_ID_DELIVER_SM_RESP orelse
        CmdId =:= ?COMMAND_ID_SUBMIT_SM_RESP ->

    CmdStatusName =
    case CmdId of
        ?COMMAND_ID_SUBMIT_SM_RESP ->
            ",SRstatus=";
        ?COMMAND_ID_DELIVER_SM_RESP ->
            ",DRstatus="
    end,
    CmdStatus = [CmdStatusName, integer_to_list(Status)],

    MsgId =
    case CmdId of
        ?COMMAND_ID_SUBMIT_SM_RESP ->
            [",msgid=", smpp_operation:get_value(message_id, Pdu, "")];
        _ -> ""
    end,

    PduHex = hexify(BinPdu),
    [Banner, cmdname(CmdId), " ", integer_to_list(SeqNum), CmdStatus, MsgId, ",hex=", PduHex, $\n];

get_first_line({CmdId, _Status, SeqNum, _Body} = Pdu, BinPdu, Banner) when
        CmdId =:= ?COMMAND_ID_BIND_RECEIVER orelse
        CmdId =:= ?COMMAND_ID_BIND_TRANSMITTER orelse
        CmdId =:= ?COMMAND_ID_BIND_TRANSCEIVER ->
    PduHex = hexify(BinPdu),
    SystemType = [",stype=", smpp_operation:get_value(system_type, Pdu)],
    SystemId = [",sid=", smpp_operation:get_value(system_id, Pdu)],
    Pass = [",password=", smpp_operation:get_value(password, Pdu)],
    Version = [",version=", integer_to_list(smpp_operation:get_value(interface_version, Pdu), 16)],
    [Banner, cmdname(CmdId), " ", integer_to_list(SeqNum), SystemType, SystemId, Pass, Version, ",hex=", PduHex, $\n];

get_first_line({CmdId, Status, SeqNum, _Body} = Pdu, BinPdu, Banner) when
        CmdId =:= ?COMMAND_ID_BIND_RECEIVER_RESP orelse
        CmdId =:= ?COMMAND_ID_BIND_TRANSMITTER_RESP orelse
        CmdId =:= ?COMMAND_ID_BIND_TRANSCEIVER_RESP ->
    PduHex = hexify(BinPdu),
    SystemId = [",sid=", smpp_operation:get_value(system_id, Pdu, "")],

    CmdStatus = [",Bstatus=", integer_to_list(Status)],
    [Banner, cmdname(CmdId), " ", integer_to_list(SeqNum), CmdStatus, SystemId, ",hex=", PduHex, $\n].


get_second_line({CmdId, _Status, _SeqNum, _Body} = Pdu, _BinPdu, Banner) when
        CmdId =:= ?COMMAND_ID_SUBMIT_SM orelse
        CmdId =:= ?COMMAND_ID_DELIVER_SM ->

    DestAddr = smpp_operation:get_value(destination_addr, Pdu, ""),

    SrcAddr = smpp_operation:get_value(source_addr, Pdu, ""),

    ShortMsg = smpp_operation:get_value(short_message, Pdu),
    HasUDH = smpp_sm:udhi(Pdu),
    ShortMsgLogMsgPart =
    if
        HasUDH andalso CmdId =:= ?COMMAND_ID_SUBMIT_SM andalso ShortMsg =/= undefined ->
            {_Udh, Rest} = smpp_sm:chop_udh(ShortMsg),
            Rest;
        ShortMsg =/= undefined ->
            ShortMsg;
        true -> ""
    end,

    ReceiptedMsgID =
    case smpp_operation:get_value(receipted_message_id, Pdu) of
        undefined -> "";
        RMI ->
            [",r_msgid=", RMI]
    end,

    [Banner, "dest=", DestAddr, ",src=", SrcAddr, ",msgtxt=", ShortMsgLogMsgPart, ReceiptedMsgID, $\n];

get_second_line(_Pdu, _BinPdu, _Banner) -> [].


get_third_line({CmdId, _Status, _SeqNum, _Body} = Pdu, _BinPdu, Banner) when
        CmdId =:= ?COMMAND_ID_SUBMIT_SM orelse
        CmdId =:= ?COMMAND_ID_DELIVER_SM ->

    EsmClass = smpp_operation:get_value(esm_class, Pdu),
    RegisteredDelivery = integer_to_list(smpp_operation:get_value(registered_delivery, Pdu)),
    DestAddrTon = integer_to_list(smpp_operation:get_value(dest_addr_ton, Pdu)),
    DestAddrNpi = integer_to_list(smpp_operation:get_value(dest_addr_npi, Pdu)),
    SourceAddrTon =
    case smpp_operation:get_value(source_addr_ton, Pdu) of
        undefined -> "";
        SourceAddrTonInt when is_integer(SourceAddrTonInt) ->
            integer_to_list(SourceAddrTonInt)
    end,
    SourceAddrNpi =
    case smpp_operation:get_value(source_addr_npi, Pdu) of
        undefined -> "";
        SourceAddrNpiInt when is_integer(SourceAddrNpiInt) ->
            integer_to_list(SourceAddrNpiInt)
    end,

    ValidityPeriod =
    case smpp_operation:get_value(validity_period, Pdu) of
        undefined -> "";
        "" -> "";
        VP ->
            [",validity=", VP]
    end,

    ServiceType =
    case smpp_operation:get_value(service_type, Pdu) of
        undefined -> "";
        "" -> "";
        ST ->
            [",stype=", ST]
    end,

    DataCoding = integer_to_list(smpp_operation:get_value(data_coding, Pdu)),

    SarMsgRefNum = smpp_operation:get_value(sar_msg_ref_num, Pdu),
    SarTotalSegments = smpp_operation:get_value(sar_total_segments, Pdu),
    SarSegmentsSeqnum = smpp_operation:get_value(sar_segment_seqnum, Pdu),

    SarDefined =
        SarMsgRefNum =/= undefined andalso
        SarTotalSegments =/= undefined andalso
        SarSegmentsSeqnum =/= undefined,

    ShortMsg = smpp_operation:get_value(short_message, Pdu),
    HasUDH = smpp_sm:udhi(Pdu),
    {UdhMsgRefNum, UdhTotalSegments, UdhSegmentsSeqnum} =
    if
        not SarDefined andalso
        HasUDH andalso
        CmdId =:= ?COMMAND_ID_SUBMIT_SM andalso
        ShortMsg =/= undefined ->

            try smpp_sm:ie(?IEI_CONCAT, ShortMsg) of %% NOTE: ignore 16bit ref num, because J3 does not support it
                [?IEI_CONCAT, _, RefNum0, TotalSegments0, SegmentsSeqnum0 | _] ->
                    {RefNum0, TotalSegments0, SegmentsSeqnum0}
            catch _:_ ->
                {undefined, undefined, undefined}
            end;

        true ->
            {undefined, undefined, undefined}
    end,

    UdhMultipartDefined = UdhMsgRefNum =/= undefined,

    Multi =
    if
        SarDefined ->
            [",multi(sar)=", integer_to_list(SarMsgRefNum), $/,
                integer_to_list(SarTotalSegments), $/, integer_to_list(SarSegmentsSeqnum)];
        UdhMultipartDefined ->
            [",multi(udh)=", integer_to_list(UdhMsgRefNum), $/,
                integer_to_list(UdhTotalSegments), $/, integer_to_list(UdhSegmentsSeqnum)];
        true -> []
    end,

    [Banner, "dcs=", DataCoding, ",DLR=", RegisteredDelivery,
        ",dtonnpi=", DestAddrTon, $/, DestAddrNpi,
        ",stonnpi=", SourceAddrTon, $/, SourceAddrNpi,
        ",esmc=", integer_to_list(EsmClass),
        Multi,
        ValidityPeriod,
        ServiceType, $\n];
get_third_line(_Pdu, _BinPdu, _Banner) -> [].


get_banner2(Direction, CmdId, TimeLocale) ->
    Arrow = case Direction of in -> $<; out -> $> end,

    CmdShortName =
    if
        CmdId =:= ?COMMAND_ID_ENQUIRE_LINK orelse
        CmdId =:= ?COMMAND_ID_ENQUIRE_LINK_RESP             -> "e";

        CmdId =:= ?COMMAND_ID_DELIVER_SM orelse
        CmdId =:= ?COMMAND_ID_DELIVER_SM_RESP               -> "d";

        CmdId =:= ?COMMAND_ID_BIND_RECEIVER orelse
        CmdId =:= ?COMMAND_ID_BIND_TRANSMITTER orelse
        CmdId =:= ?COMMAND_ID_BIND_TRANSCEIVER orelse
        CmdId =:= ?COMMAND_ID_BIND_RECEIVER_RESP orelse
        CmdId =:= ?COMMAND_ID_BIND_TRANSMITTER_RESP orelse
        CmdId =:= ?COMMAND_ID_BIND_TRANSCEIVER_RESP         -> "b";

        true                                                -> ""
    end,

    {_, _, MicroSecs} = Now = os:timestamp(),
    {{Y, Mon, D}, {H, Min, S}} =
    case TimeLocale of
        local     -> calendar:now_to_local_time(Now);
        universal -> calendar:now_to_universal_time(Now)
    end,
    [io_lib:format("~2..0w~2..0w~2..0w ~2..0w:~2..0w:~2..0w.~3..0w",
                   [Y rem 100, Mon, D, H, Min, S, MicroSecs div 1000]),
     " ", Arrow, CmdShortName, " "].
