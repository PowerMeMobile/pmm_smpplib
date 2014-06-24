-module(pmm_smpp_fmt).

-include_lib("oserl/include/oserl.hrl").

-export([format/2, format/4]).

-compile(export_all).

%-define(TEST, 1).
-ifdef(TEST).
    -include_lib("eunit/include/eunit.hrl").
-endif.

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
                ?ESME_ROK -> "info";
                _         -> "error"
            end,
    Banner = banner(Direction, Level, TimeLocale),
    Details = case Level of
                  "error" -> [Banner, "error: ", ErrFun(Status), "\n"];
                  "info"  -> ""
              end,
    Size = size(BinPdu),
    Params = [{command_length, Size}|smpp_operation:to_list(Pdu)],
    [$\n,
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

integer_to_hexlist(0) -> "00";
integer_to_hexlist(1) -> "01";
integer_to_hexlist(2) -> "02";
integer_to_hexlist(3) -> "03";
integer_to_hexlist(4) -> "04";
integer_to_hexlist(5) -> "05";
integer_to_hexlist(6) -> "06";
integer_to_hexlist(7) -> "07";
integer_to_hexlist(8) -> "08";
integer_to_hexlist(9) -> "09";
integer_to_hexlist(10) -> "0A";
integer_to_hexlist(11) -> "0B";
integer_to_hexlist(12) -> "0C";
integer_to_hexlist(13) -> "0D";
integer_to_hexlist(14) -> "0E";
integer_to_hexlist(15) -> "0F";
integer_to_hexlist(16) -> "10";
integer_to_hexlist(17) -> "11";
integer_to_hexlist(18) -> "12";
integer_to_hexlist(19) -> "13";
integer_to_hexlist(20) -> "14";
integer_to_hexlist(21) -> "15";
integer_to_hexlist(22) -> "16";
integer_to_hexlist(23) -> "17";
integer_to_hexlist(24) -> "18";
integer_to_hexlist(25) -> "19";
integer_to_hexlist(26) -> "1A";
integer_to_hexlist(27) -> "1B";
integer_to_hexlist(28) -> "1C";
integer_to_hexlist(29) -> "1D";
integer_to_hexlist(30) -> "1E";
integer_to_hexlist(31) -> "1F";
integer_to_hexlist(32) -> "20";
integer_to_hexlist(33) -> "21";
integer_to_hexlist(34) -> "22";
integer_to_hexlist(35) -> "23";
integer_to_hexlist(36) -> "24";
integer_to_hexlist(37) -> "25";
integer_to_hexlist(38) -> "26";
integer_to_hexlist(39) -> "27";
integer_to_hexlist(40) -> "28";
integer_to_hexlist(41) -> "29";
integer_to_hexlist(42) -> "2A";
integer_to_hexlist(43) -> "2B";
integer_to_hexlist(44) -> "2C";
integer_to_hexlist(45) -> "2D";
integer_to_hexlist(46) -> "2E";
integer_to_hexlist(47) -> "2F";
integer_to_hexlist(48) -> "30";
integer_to_hexlist(49) -> "31";
integer_to_hexlist(50) -> "32";
integer_to_hexlist(51) -> "33";
integer_to_hexlist(52) -> "34";
integer_to_hexlist(53) -> "35";
integer_to_hexlist(54) -> "36";
integer_to_hexlist(55) -> "37";
integer_to_hexlist(56) -> "38";
integer_to_hexlist(57) -> "39";
integer_to_hexlist(58) -> "3A";
integer_to_hexlist(59) -> "3B";
integer_to_hexlist(60) -> "3C";
integer_to_hexlist(61) -> "3D";
integer_to_hexlist(62) -> "3E";
integer_to_hexlist(63) -> "3F";
integer_to_hexlist(64) -> "40";
integer_to_hexlist(65) -> "41";
integer_to_hexlist(66) -> "42";
integer_to_hexlist(67) -> "43";
integer_to_hexlist(68) -> "44";
integer_to_hexlist(69) -> "45";
integer_to_hexlist(70) -> "46";
integer_to_hexlist(71) -> "47";
integer_to_hexlist(72) -> "48";
integer_to_hexlist(73) -> "49";
integer_to_hexlist(74) -> "4A";
integer_to_hexlist(75) -> "4B";
integer_to_hexlist(76) -> "4C";
integer_to_hexlist(77) -> "4D";
integer_to_hexlist(78) -> "4E";
integer_to_hexlist(79) -> "4F";
integer_to_hexlist(80) -> "50";
integer_to_hexlist(81) -> "51";
integer_to_hexlist(82) -> "52";
integer_to_hexlist(83) -> "53";
integer_to_hexlist(84) -> "54";
integer_to_hexlist(85) -> "55";
integer_to_hexlist(86) -> "56";
integer_to_hexlist(87) -> "57";
integer_to_hexlist(88) -> "58";
integer_to_hexlist(89) -> "59";
integer_to_hexlist(90) -> "5A";
integer_to_hexlist(91) -> "5B";
integer_to_hexlist(92) -> "5C";
integer_to_hexlist(93) -> "5D";
integer_to_hexlist(94) -> "5E";
integer_to_hexlist(95) -> "5F";
integer_to_hexlist(96) -> "60";
integer_to_hexlist(97) -> "61";
integer_to_hexlist(98) -> "62";
integer_to_hexlist(99) -> "63";
integer_to_hexlist(100) -> "64";
integer_to_hexlist(101) -> "65";
integer_to_hexlist(102) -> "66";
integer_to_hexlist(103) -> "67";
integer_to_hexlist(104) -> "68";
integer_to_hexlist(105) -> "69";
integer_to_hexlist(106) -> "6A";
integer_to_hexlist(107) -> "6B";
integer_to_hexlist(108) -> "6C";
integer_to_hexlist(109) -> "6D";
integer_to_hexlist(110) -> "6E";
integer_to_hexlist(111) -> "6F";
integer_to_hexlist(112) -> "70";
integer_to_hexlist(113) -> "71";
integer_to_hexlist(114) -> "72";
integer_to_hexlist(115) -> "73";
integer_to_hexlist(116) -> "74";
integer_to_hexlist(117) -> "75";
integer_to_hexlist(118) -> "76";
integer_to_hexlist(119) -> "77";
integer_to_hexlist(120) -> "78";
integer_to_hexlist(121) -> "79";
integer_to_hexlist(122) -> "7A";
integer_to_hexlist(123) -> "7B";
integer_to_hexlist(124) -> "7C";
integer_to_hexlist(125) -> "7D";
integer_to_hexlist(126) -> "7E";
integer_to_hexlist(127) -> "7F";
integer_to_hexlist(128) -> "80";
integer_to_hexlist(129) -> "81";
integer_to_hexlist(130) -> "82";
integer_to_hexlist(131) -> "83";
integer_to_hexlist(132) -> "84";
integer_to_hexlist(133) -> "85";
integer_to_hexlist(134) -> "86";
integer_to_hexlist(135) -> "87";
integer_to_hexlist(136) -> "88";
integer_to_hexlist(137) -> "89";
integer_to_hexlist(138) -> "8A";
integer_to_hexlist(139) -> "8B";
integer_to_hexlist(140) -> "8C";
integer_to_hexlist(141) -> "8D";
integer_to_hexlist(142) -> "8E";
integer_to_hexlist(143) -> "8F";
integer_to_hexlist(144) -> "90";
integer_to_hexlist(145) -> "91";
integer_to_hexlist(146) -> "92";
integer_to_hexlist(147) -> "93";
integer_to_hexlist(148) -> "94";
integer_to_hexlist(149) -> "95";
integer_to_hexlist(150) -> "96";
integer_to_hexlist(151) -> "97";
integer_to_hexlist(152) -> "98";
integer_to_hexlist(153) -> "99";
integer_to_hexlist(154) -> "9A";
integer_to_hexlist(155) -> "9B";
integer_to_hexlist(156) -> "9C";
integer_to_hexlist(157) -> "9D";
integer_to_hexlist(158) -> "9E";
integer_to_hexlist(159) -> "9F";
integer_to_hexlist(160) -> "A0";
integer_to_hexlist(161) -> "A1";
integer_to_hexlist(162) -> "A2";
integer_to_hexlist(163) -> "A3";
integer_to_hexlist(164) -> "A4";
integer_to_hexlist(165) -> "A5";
integer_to_hexlist(166) -> "A6";
integer_to_hexlist(167) -> "A7";
integer_to_hexlist(168) -> "A8";
integer_to_hexlist(169) -> "A9";
integer_to_hexlist(170) -> "AA";
integer_to_hexlist(171) -> "AB";
integer_to_hexlist(172) -> "AC";
integer_to_hexlist(173) -> "AD";
integer_to_hexlist(174) -> "AE";
integer_to_hexlist(175) -> "AF";
integer_to_hexlist(176) -> "B0";
integer_to_hexlist(177) -> "B1";
integer_to_hexlist(178) -> "B2";
integer_to_hexlist(179) -> "B3";
integer_to_hexlist(180) -> "B4";
integer_to_hexlist(181) -> "B5";
integer_to_hexlist(182) -> "B6";
integer_to_hexlist(183) -> "B7";
integer_to_hexlist(184) -> "B8";
integer_to_hexlist(185) -> "B9";
integer_to_hexlist(186) -> "BA";
integer_to_hexlist(187) -> "BB";
integer_to_hexlist(188) -> "BC";
integer_to_hexlist(189) -> "BD";
integer_to_hexlist(190) -> "BE";
integer_to_hexlist(191) -> "BF";
integer_to_hexlist(192) -> "C0";
integer_to_hexlist(193) -> "C1";
integer_to_hexlist(194) -> "C2";
integer_to_hexlist(195) -> "C3";
integer_to_hexlist(196) -> "C4";
integer_to_hexlist(197) -> "C5";
integer_to_hexlist(198) -> "C6";
integer_to_hexlist(199) -> "C7";
integer_to_hexlist(200) -> "C8";
integer_to_hexlist(201) -> "C9";
integer_to_hexlist(202) -> "CA";
integer_to_hexlist(203) -> "CB";
integer_to_hexlist(204) -> "CC";
integer_to_hexlist(205) -> "CD";
integer_to_hexlist(206) -> "CE";
integer_to_hexlist(207) -> "CF";
integer_to_hexlist(208) -> "D0";
integer_to_hexlist(209) -> "D1";
integer_to_hexlist(210) -> "D2";
integer_to_hexlist(211) -> "D3";
integer_to_hexlist(212) -> "D4";
integer_to_hexlist(213) -> "D5";
integer_to_hexlist(214) -> "D6";
integer_to_hexlist(215) -> "D7";
integer_to_hexlist(216) -> "D8";
integer_to_hexlist(217) -> "D9";
integer_to_hexlist(218) -> "DA";
integer_to_hexlist(219) -> "DB";
integer_to_hexlist(220) -> "DC";
integer_to_hexlist(221) -> "DD";
integer_to_hexlist(222) -> "DE";
integer_to_hexlist(223) -> "DF";
integer_to_hexlist(224) -> "E0";
integer_to_hexlist(225) -> "E1";
integer_to_hexlist(226) -> "E2";
integer_to_hexlist(227) -> "E3";
integer_to_hexlist(228) -> "E4";
integer_to_hexlist(229) -> "E5";
integer_to_hexlist(230) -> "E6";
integer_to_hexlist(231) -> "E7";
integer_to_hexlist(232) -> "E8";
integer_to_hexlist(233) -> "E9";
integer_to_hexlist(234) -> "EA";
integer_to_hexlist(235) -> "EB";
integer_to_hexlist(236) -> "EC";
integer_to_hexlist(237) -> "ED";
integer_to_hexlist(238) -> "EE";
integer_to_hexlist(239) -> "EF";
integer_to_hexlist(240) -> "F0";
integer_to_hexlist(241) -> "F1";
integer_to_hexlist(242) -> "F2";
integer_to_hexlist(243) -> "F3";
integer_to_hexlist(244) -> "F4";
integer_to_hexlist(245) -> "F5";
integer_to_hexlist(246) -> "F6";
integer_to_hexlist(247) -> "F7";
integer_to_hexlist(248) -> "F8";
integer_to_hexlist(249) -> "F9";
integer_to_hexlist(250) -> "FA";
integer_to_hexlist(251) -> "FB";
integer_to_hexlist(252) -> "FC";
integer_to_hexlist(253) -> "FD";
integer_to_hexlist(254) -> "FE";
integer_to_hexlist(255) -> "FF".

integer_to_hexlist2(N) ->
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

%% -------------------------------------------------------------------------
%% Tests
%% -------------------------------------------------------------------------

-ifdef(TEST).

hexify_test() ->
    Expected = [
"00","01","02","03","04","05","06","07","08","09","0A",
"0B","0C","0D","0E","0F","10","11","12","13","14","15","16",
"17","18","19","1A","1B","1C","1D","1E","1F","20","21","22",
"23","24","25","26","27","28","29","2A","2B","2C","2D","2E",
"2F","30","31","32","33","34","35","36","37","38","39","3A",
"3B","3C","3D","3E","3F","40","41","42","43","44","45","46",
"47","48","49","4A","4B","4C","4D","4E","4F","50","51","52",
"53","54","55","56","57","58","59","5A","5B","5C","5D","5E",
"5F","60","61","62","63","64","65","66","67","68","69","6A",
"6B","6C","6D","6E","6F","70","71","72","73","74","75","76",
"77","78","79","7A","7B","7C","7D","7E","7F","80","81","82",
"83","84","85","86","87","88","89","8A","8B","8C","8D","8E",
"8F","90","91","92","93","94","95","96","97","98","99","9A",
"9B","9C","9D","9E","9F","A0","A1","A2","A3","A4","A5","A6",
"A7","A8","A9","AA","AB","AC","AD","AE","AF","B0","B1","B2",
"B3","B4","B5","B6","B7","B8","B9","BA","BB","BC","BD","BE",
"BF","C0","C1","C2","C3","C4","C5","C6","C7","C8","C9","CA",
"CB","CC","CD","CE","CF","D0","D1","D2","D3","D4","D5","D6",
"D7","D8","D9","DA","DB","DC","DD","DE","DF","E0","E1","E2",
"E3","E4","E5","E6","E7","E8","E9","EA","EB","EC","ED","EE",
"EF","F0","F1","F2","F3","F4","F5","F6","F7","F8","F9","FA",
"FB","FC","FD","FE","FF"
    ],
    Actual = hexify(list_to_binary(lists:seq(0, 255))),
    ?assertEqual(Expected, Actual).

params_1_test() ->
    Params = [
        {command_length,16},
        {command_id,21},
        {command_status,0},
        {sequence_number,2}
    ],
    Expected =
"command_length=16,command_id=21,command_status=0,sequence_number=2",
    Actual = lists:flatten(pmm_smpp_fmt:params(0, Params, "")),
    ?assertEqual(Expected, Actual).

params_2_test() ->
    Params = [
        {command_length,46},
        {command_id,1},
        {command_status,0},
        {sequence_number,1},
        {address_range,[]},
        {addr_npi,1},
        {addr_ton,1},
        {interface_version,52},
        {system_type,"SMPP"},
        {password,"password"},
        {system_id,"smppclient2"}
    ],
    Expected =
"command_length=46,command_id=1,command_status=0,sequence_number=1,address_range=,addr_npi=1,\n"
"addr_ton=1,interface_version=52,system_type=SMPP,password=password,system_id=smppclient2",
    Actual = lists:flatten(pmm_smpp_fmt:params(0, Params, "")),
    ?assertEqual(Expected, Actual).

params_3_test() ->
    Params = [
        {command_length,157},
        {command_id,5},
        {command_status,0},
        {sequence_number,206},
        {short_message,"id:205 sub:001 dlvrd:001 submit date:1406031650 done date:1406031650 stat:DELIVRD err:000 Text:Hello"},
        {sm_default_msg_id,0},
        {data_coding,0},
        {replace_if_present_flag,0},
        {registered_delivery,0},
        {validity_period,[]},
        {schedule_delivery_time,[]},
        {priority_flag,0},
        {protocol_id,0},
        {esm_class,4},
        {destination_addr,"375296660004"},
        {dest_addr_npi,1},
        {dest_addr_ton,1},
        {source_addr,"375297778899"},
        {source_addr_npi,1},
        {source_addr_ton,1},
        {service_type,[]}
    ],
    Expected =
"command_length=157,command_id=5,command_status=0,sequence_number=206,\n"
"short_message=id:205 sub:001 dlvrd:001 submit date:1406031650 done date:1406031650 stat:DELIVRD err:000 Text:Hello,\n"
"sm_default_msg_id=0,data_coding=0,replace_if_present_flag=0,registered_delivery=0,\n"
"validity_period=,schedule_delivery_time=,priority_flag=0,protocol_id=0,esm_class=4,\n"
"destination_addr=375296660004,dest_addr_npi=1,dest_addr_ton=1,source_addr=375297778899,\n"
"source_addr_npi=1,source_addr_ton=1,service_type=",
    Actual = lists:flatten(pmm_smpp_fmt:params(0, Params, "")),
    ?assertEqual(Expected, Actual).

-endif.
