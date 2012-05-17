-module(pmm_udh).

-export([concat_ie/3, concat_8_ie/3, concat_16_ie/3]).
-export([port_addressing_ie/2, port_addressing_8_ie/2, port_addressing_16_ie/2]).

-define(CONCAT_8_IEI, 0).  % IEI for Concatenated SMs (8 bit reference number).
-define(CONCAT_16_IEI, 8). % IEI for Concatenated SMs (16 bit reference number).
-define(PORT_8_IEI, 4).    % IEI for Application Port Addressing (8 bit address).
-define(PORT_16_IEI, 5).   % IEI for Application Port Addressing (16 bit address).

-define(CONCAT_8_IEDL, 3).  % IEDL for Concatenated SMs (8 bit reference number).
-define(CONCAT_16_IEDL, 4). % IEDL for Concatenated SMs (16 bit reference number).
-define(PORT_8_IEDL, 2).    % IEDL for Application Port Addressing (8 bit address).
-define(PORT_16_IEDL, 4).   % IEDL for Application Port Addressing (16 bit address).

%% -------------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------------

-spec concat_ie(0..65535, 1..255, 1..255) -> list().
concat_ie(RefNum, TotalSegments, SeqNum) when RefNum >= 0, RefNum =< 65535,
                                              TotalSegments >= 1, TotalSegments =< 255,
                                              SeqNum >= 1, SeqNum =< 255 ->
    if
        RefNum =< 255 ->
            concat_8_ie(RefNum, TotalSegments, SeqNum);
        true ->
            concat_16_ie(RefNum, TotalSegments, SeqNum)
    end.

-spec concat_8_ie(0..255, 1..255, 1..255) -> list().
concat_8_ie(RefNum, TotalSegments, SeqNum) ->
    [?CONCAT_8_IEI, ?CONCAT_8_IEDL, RefNum, TotalSegments, SeqNum].

-spec concat_16_ie(0..65535, 1..255, 1..255) -> list().
concat_16_ie(RefNum, TotalSegments, SeqNum) ->
    <<RefNum1, RefNum2>> = <<RefNum:16>>,
    [?CONCAT_16_IEI, ?CONCAT_16_IEDL, RefNum1, RefNum2, TotalSegments, SeqNum].

-spec port_addressing_ie(0..65535, 0..65535) -> list().
port_addressing_ie(DestPort, OrigPort) when DestPort >= 0, DestPort =< 65535,
                                            OrigPort >= 0, OrigPort =< 65535 ->
    if
        DestPort =< 255 andalso OrigPort =< 255 ->
            port_addressing_8_ie(DestPort, OrigPort);
        true ->
            port_addressing_16_ie(DestPort, OrigPort)
    end.

-spec port_addressing_8_ie(0..255, 0..255) -> list().
port_addressing_8_ie(DestPort, OrigPort) ->
    [?PORT_8_IEI, ?PORT_8_IEDL, DestPort, OrigPort].

-spec port_addressing_16_ie(0..65535, 0..65535) -> list().
port_addressing_16_ie(DestPort, OrigPort) ->
    <<DestPort1, DestPort2>> = <<DestPort:16>>,
    <<OrigPort1, OrigPort2>> = <<OrigPort:16>>,
    [?PORT_16_IEI, ?PORT_16_IEDL, DestPort1, DestPort2, OrigPort1, OrigPort2].
