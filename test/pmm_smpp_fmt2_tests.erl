-module(pmm_smpp_fmt2_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("oserl/include/oserl.hrl").

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
    Actual = pmm_smpp_fmt2:hexify(list_to_binary(lists:seq(0, 255))),
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
        {service_type,[]},
        {cid, 444}
    ],
    Expected =
"command_length=157,command_id=5,command_status=0,sequence_number=206,\n"
"short_message=id:205 sub:001 dlvrd:001 submit date:1406031650 done date:1406031650 stat:DELIVRD err:000 Text:Hello,\n"
"sm_default_msg_id=0,data_coding=0,replace_if_present_flag=0,registered_delivery=0,\n"
"validity_period=,schedule_delivery_time=,priority_flag=0,protocol_id=0,esm_class=4,\n"
"destination_addr=375296660004,dest_addr_npi=1,dest_addr_ton=1,source_addr=375297778899,\n"
"source_addr_npi=1,source_addr_ton=1,service_type=,cid=444",
    Actual = lists:flatten(pmm_smpp_fmt:params(0, Params, "")),
    ?assertEqual(Expected, Actual).

sm_hex_test() ->
    DefaultResult = {[{key, value}], ""},
    DefaultResult = pmm_smpp_fmt2:get_short_message_and_udh_hex(someCmdId, true, "", [{key, value}]),
    DefaultResult = pmm_smpp_fmt2:get_short_message_and_udh_hex(?COMMAND_ID_DELIVER_SM, false, "", [{key, value}]),
    DefaultResult = pmm_smpp_fmt2:get_short_message_and_udh_hex(?COMMAND_ID_SUBMIT_SM, false, "", [{key, value}]),

    Params0 = [
        {esm_class, 0},
        {data_coding, 0},
        {short_message, "msg"}
    ],
    {Params0, ""} = pmm_smpp_fmt2:get_short_message_and_udh_hex(?COMMAND_ID_SUBMIT_SM, true, "", Params0),

    Params1 = [
        {esm_class, 0},
        {data_coding, 1},
        {short_message, "msg"}
    ],
    {Params1, ""} = pmm_smpp_fmt2:get_short_message_and_udh_hex(?COMMAND_ID_SUBMIT_SM, true, "", Params1),

    Params2 = [
        {esm_class, 0},
        {data_coding, 3},
        {short_message, "msg"}
    ],
    {Params2, ""} = pmm_smpp_fmt2:get_short_message_and_udh_hex(?COMMAND_ID_SUBMIT_SM, true, "", Params2),


    Params3 = [
        {esm_class, 0},
        {data_coding, 2},
        {short_message, "hello"}
    ],
    SMHex3 = "short_message (5 bytes):\n68656C6C:6F\n",
    {Params3, SMHex3Out} =
        pmm_smpp_fmt2:get_short_message_and_udh_hex(?COMMAND_ID_SUBMIT_SM, true, "", Params3),
    ?debugVal(lists:flatten(SMHex3Out)),
    SMHex3 = lists:flatten(SMHex3Out),

    %% Concatenated short message, 16-bit reference number
    UDH = [4,8,2,0,1],
    Params4 = [
        {esm_class, 0},
        {data_coding, 2},
        {short_message, UDH ++ "hello"}
    ],
    SMHex4 = "short_message (10 bytes):\n04080200:0168656C:6C6F\n",
    {Params4, SMHex4Out} =
        pmm_smpp_fmt2:get_short_message_and_udh_hex(?COMMAND_ID_SUBMIT_SM, true, "", Params4),
    ?debugVal(lists:flatten(SMHex4Out)),
    SMHex4 = lists:flatten(SMHex4Out),

    Params5 = [
        {esm_class, 64},
        {data_coding, 3},
        {short_message, UDH ++ "hello"}
    ],
    Params5Out = [{udh, pmm_smpp_fmt2:hexify(list_to_binary(UDH))} | Params5],
    {Params5Out, ""} =
        pmm_smpp_fmt2:get_short_message_and_udh_hex(?COMMAND_ID_SUBMIT_SM, true, "", Params5).
