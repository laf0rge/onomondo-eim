% Author: Philipp Maier <pmaier@sysmocom.de> / sysmocom - s.f.m.c. GmbH

-module(esipa_euiccPackage).

-export([order_to_euiccPackageSigned/2]).

order_to_euiccPackageSigned(Order, EidValue) ->
    % TODO: add support for eCO

    % Convert Order to PSMO list
    {[{<<"psmo">>, PsmoOrderList}]} = Order,
    Order2Psmo = fun(PsmoOrder) ->
			 case PsmoOrder of
			     {[{<<"enable">>, {[{<<"iccid">>,Iccid}, {<<"rollback">>,true}]}}]} ->
				 {enable, #{iccid => utils:hex_to_binary(Iccid), rollbackFlag => null}};
			     {[{<<"enable">>, {[{<<"iccid">>,Iccid}, {<<"rollback">>,false}]}}]} ->
				 {enable, #{iccid => utils:hex_to_binary(Iccid)}};
			     {[{<<"disable">>, {[{<<"iccid">>,Iccid}]}}]} ->
				 {disable, #{iccid => utils:hex_to_binary(Iccid)}};
			     {[{<<"delete">>, {[{<<"iccid">>,Iccid}]}}]} ->
				 {delete, #{iccid => utils:hex_to_binary(Iccid)}}
				% TODO: when nothing matches, we get a crash report and nothing else
				% happens. We should instead ensure that an undefinedError is returned
				% to the ipad and the work is finished with a failure status.
			 end
		 end,
    PsmoList = [Order2Psmo(O) || O <- PsmoOrderList ],

    % Format EuiccPackageSigned
    {ok, EimId} = application:get_env(onomondo_eim, eim_id),
    EuiccPackage = {psmoList, PsmoList},
    #{eimId => list_to_binary(EimId),
      eidValue => EidValue,
      counterValue => 0, % TODO: pick a suitable value (how?)
      transactionId => <<1,2,3,4>>, %TODO: generate a random transaction id (and store it?)
      euiccPackage => EuiccPackage}.
