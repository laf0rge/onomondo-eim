% Author: Harald Welte <hwelte@sysmocom.de> / sysmocom - s.f.m.c. GmbH
% Author: Philipp Maier <pmaier@sysmocom.de> / sysmocom - s.f.m.c. GmbH

-module(eim_cfg).

-export([get_eim_cfg/0]).

get_eim_cfg() ->
    {ok, EimId} = application:get_env(onomondo_eim, eim_id),
    {ok, EsipaIp} = application:get_env(onomondo_eim, esipa_ip),
    {ok, EsipaPort} = application:get_env(onomondo_eim, esipa_port),
    EimFqdn = string:join([inet:ntoa(EsipaIp), io_lib:format(":~B", [EsipaPort])], ""),
    EimConfigurationData = #{eimId => EimId,
			     eimFqdn => EimFqdn,
			     associationToken => 123}, %TODO: make this configurable? where does the value come from?
    EimConfigurationDataList = [EimConfigurationData],
    GetEimConfigurationDataResponse = #{eimConfigurationDataList => EimConfigurationDataList},

    {ok, EncodedGetEimConfigurationDataResponse} = 'SGP32Definitions':encode('GetEimConfigurationDataResponse', GetEimConfigurationDataResponse),
    utils:binary_to_hex(EncodedGetEimConfigurationDataResponse).
