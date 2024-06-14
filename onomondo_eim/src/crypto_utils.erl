% Author: Philipp Maier <pmaier@sysmocom.de> / sysmocom - s.f.m.c. GmbH

-module(crypto_utils).

-export([sign_euiccPackageSigned/1, verify_euiccPackageResultSigned/1]).

%for debugging
-export([der_to_plain/1, plain_to_der/1]).


%Convert from DER encoded signature format to plain format (see also BSI TR03111 5.2.1)
der_to_plain(DERSignature) ->
    {ok, [R, S]} = 'DERSignature':decode('DERSignature', DERSignature),
    RPlain = utils:hex_to_binary(list_to_binary(integer_to_list(R, 16))),
    SPlain = utils:hex_to_binary(list_to_binary(integer_to_list(S, 16))),
    utils:join_binary_list([RPlain, SPlain]).

%Convert from plain format to DER encoded signature format (see also BSI TR03111 5.2.1)
plain_to_der(PlainSignature) ->
    R = binary_part(PlainSignature, 0, 32),
    S = binary_part(PlainSignature, 32, 32),
    DERSignature = [binary:decode_unsigned(R),binary:decode_unsigned(S)],
    {ok, DERSignatureEncoded} = 'DERSignature':encode('DERSignature', DERSignature),
    DERSignatureEncoded.

sign_euiccPackageSigned(EuiccPackageSigned) ->
    %TODO: read the associationToken from somewhere, we likely have to store
    %an associationToken for each eUICC in the (to be developed) eUICC database.
    %for now we will use a hardcoded associationToken for test:
    AssociationToken = <<132,1,1>>,

    %Format message to be signed
    {ok, EuiccPackageSignedEnc} = 'SGP32Definitions':encode('EuiccPackageSigned', EuiccPackageSigned),
    MsgToBeSigned = utils:join_binary_list([EuiccPackageSignedEnc, AssociationToken]),

    %Load private key from eIM certificate
    {ok, EimKeyPath} = application:get_env(onomondo_eim, eim_key),
    {ok, EimKeyPem} = file:read_file(EimKeyPath),
    [EimKeyPemEntry] = public_key:pem_decode(EimKeyPem),
    EimKeyECPrivateKey = public_key:pem_entry_decode(EimKeyPemEntry),

    %Sign message
    % We use SHA-256 as signature hash/digest, see also GSMA SGP.22, section 2.6.5 and
    % https://www.erlang.org/doc/apps/public_key/public_key#sign/4
    der_to_plain(public_key:sign(MsgToBeSigned, sha256, EimKeyECPrivateKey)).

verify_signature(Message, Signature) ->
    DERSignature = plain_to_der(Signature),
    io:format("===============================================================~n"),
    io:format("============Signature>~p~n", [utils:binary_to_hex(Signature)]),
    io:format("============DERSignature>~p~n", [utils:binary_to_hex(DERSignature)]),
    io:format("============Message>~p~n", [utils:binary_to_hex(Message)]),
    io:format("===============================================================~n"),

    %NIST
%    SubjectPublicKey = <<4,109,179,245,58,220,135,220,47,241,12,123,252,216,122,209,58,
%              233,112,9,175,160,101,166,117,126,229,113,179,242,235,177,143,
%              70,193,214,143,62,222,176,231,75,46,93,84,32,81,231,210,127,80,
%              149,32,40,96,90,253,239,121,254,159,255,208,57,89>>,
%    Algorithm = {1,2,840,10045,3,1,7},


    %BRP
%    SubjectPublicKey = <<4,62,89,12,56,169,194,86,49,94,207,243,41,20,22,221,51,84,9,166,
%              102,253,65,179,181,30,94,81,20,243,67,171,240,162,103,116,198,
%              194,108,72,117,58,254,40,54,67,34,123,182,96,140,210,97,204,151,
%              45,55,74,71,145,36,235,242,119,34>>,
%    Algorithm = {1,3,36,3,3,2,8,1,1,7},

    SubjectPublicKey = <<4,189,44,85,178,139,78,128,28,160,177,79,
                                   25,87,136,253,134,199,255,73,118,76,136,
                                   162,147,76,105,89,74,38,255,100,117,73,243,
                                   241,96,96,189,156,141,121,61,149,208,18,
                                   111,164,41,201,73,102,254,120,66,150,114,
                                   99,121,90,115,196,152,241,219>>,
    Algorithm = {1,2,840,10045,3,1,7},

    ECPublicKey = {{'ECPoint',SubjectPublicKey}, {namedCurve,Algorithm}},

    Result = public_key:verify(Message, sha256, DERSignature, ECPublicKey),
    io:format("===============================================================~n"),
    io:format("============Result>~p~n", [Result]),
    io:format("===============================================================~n"),
    ok.

verify_euiccPackageResultSigned(EuiccPackageResult) ->

    case EuiccPackageResult of
		  {euiccPackageResultSigned, EuiccPackageResultSigned} ->
		      EuiccPackageResultDataSigned = maps:get(euiccPackageResultDataSigned, EuiccPackageResultSigned),
		      {ok, EuiccPackageResultDataSigned_enc} = 'SGP32Definitions':encode('EuiccPackageResultDataSigned', EuiccPackageResultDataSigned),
		      EuiccSignEPR = maps:get(euiccSignEPR, EuiccPackageResultSigned),
		      verify_signature(EuiccPackageResultDataSigned_enc, EuiccSignEPR);
		  {euiccPackageErrorSigned, EuiccPackageErrorSigned} ->
		      EuiccPackageErrorDataSigned = maps:get(euiccPackageResultDataSigned, EuiccPackageErrorSigned),
		      {ok, EuiccPackageErrorDataSigned_enc} = 'SGP32Definitions':encode('EuiccPackageErrorDataSigned', EuiccPackageErrorDataSigned),
		      EuiccSignEPR = maps:get(euiccSignEPR, EuiccPackageErrorSigned),
		      verify_signature(EuiccPackageErrorDataSigned_enc, EuiccSignEPR);
		  {euiccPackageErrorUnsigned, _} ->
		      ok; % This result has no signature
		  _ ->
		      error
    end.
