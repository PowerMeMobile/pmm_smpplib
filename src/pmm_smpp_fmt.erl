-module(pmm_smpp_fmt).

-include_lib("oserl/include/oserl.hrl").

-export([format/2, format/4]).

%% -------------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------------

-spec format(in | out, binary()) -> iolist().
format(Direction, BinPdu) ->
    format(Direction, BinPdu, fun smpp_error:format/1, local).

-spec format(in | out, binary(), fun((pos_integer()) -> string()), 'local' | 'universal') ->
             iolist().
format(Direction, BinPdu, ErrFun, TimeLocale) ->
    {ok, {CmdId, Status, SeqNum, _Body} = Pdu} = smpp_operation:unpack(BinPdu),
    Level = case Status of
                ?ESME_ROK -> info;
                _         -> error
            end,
    Banner = banner(Direction, Level, TimeLocale),
    Details = case Level of
                  error -> [Banner, "error: ", ErrFun(Status), "\n"];
                  info  -> ""
              end,
    Size = size(BinPdu),
    Params = [{command_length, Size}|smpp_operation:to_list(Pdu)],
    [Banner, $\n,
     Banner, cmdname(CmdId), " ", integer_to_list(SeqNum), $\n,
     Details,
     Banner, "hex dump (", integer_to_list(Size), " bytes):", $\n,
     hexdump(BinPdu, Banner),
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
    [io_lib:format("~4w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w.~3..0w",
                   [Y, Mon, D, H, Min, S, MicroSecs div 1000]),
     " ", Arrow, " [", atom_to_list(Level), "] "].

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

integer_to_hexlist(N) ->
    [integer_to_hexdigit(N div 16), integer_to_hexdigit(N rem 16)].

integer_to_hexdigit(N) when N < 10 ->
    $0 + N;
integer_to_hexdigit(N) ->
    $A + N - 10.

interpose([], _Sep) ->
    [];
interpose(List, Sep) ->
    tl(lists:foldr(fun(Item, Acc) -> [Sep, Item|Acc] end, [], List)).

params(CmdId, Params, Banner) ->
    lists:reverse(
        lists:foldl(
            fun({K, V}, [H|T]) ->
                KV = lists:concat([K, "=", V]),
                Len = lists:flatlength(H),
                if
                    Len =:= 0 ->
                        [[Banner, KV]|T];
                    Len + length(KV) =< 100 ->
                        [[H, $,, KV]|T];
                    true ->
                        [[Banner, KV], [H, ",\n"]|T]
                end
            end,
            [[]], unfold_multi(CmdId, Params))).

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
