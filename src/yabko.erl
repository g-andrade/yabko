%% Copyright (c) 2017 Guilherme Andrade <yabko@gandrade.net>
%%
%% Permission is hereby granted, free of charge, to any person obtaining a
%% copy  of this software and associated documentation files (the "Software"),
%% to deal in the Software without restriction, including without limitation
%% the rights to use, copy, modify, merge, publish, distribute, sublicense,
%% and/or sell copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
%% DEALINGS IN THE SOFTWARE.
%%
%% Yabko is an independent project and has not been authorized, sponsored,
%% or otherwise approved by Apple Inc.

-module(yabko).

-include("yabko_common.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([decode/1]).                    -ignore_xref({decode,1}).

%% ------------------------------------------------------------------
%% Type Definitions
%% ------------------------------------------------------------------

-type object() ::
        undefined |
        boolean() |
        int64() |
        float() |
        calendar:datetime() |
        {uid, uint64()} |
        [object()] |
        #{ binary() => object() }.
-export_type([object/0]).

-type int64() :: -9223372036854775808..9223372036854775807.
-export_type([int64/0]).

-type uint64() :: 0..18446744073709551615.
-export_type([uint64/0]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec decode(Data) -> {ok, DecodedObject} | {error, Error}
        when Data :: iodata(),
             DecodedObject :: object(),
             Error :: {exception, atom(), term(), [term()]}.

decode(<<"bplist", Version:2/binary, EncodedPList/binary>>) when Version =:= <<"00">>;
                                                                 Version =:= <<"01">> ->
    try yabko_bin:decode(EncodedPList, 8) of
        PList -> {ok, PList}
    catch
        Class:Reason ->
            {error, {exception, Class, Reason, erlang:get_stacktrace()}}
    end;
decode(<<EncodedPList/binary>>) ->
    try yabko_xml:decode(EncodedPList) of
        PList -> {ok, PList}
    catch
        Class:Reason ->
            {error, {exception, Class, Reason, erlang:get_stacktrace()}}
    end;
decode(IoData) ->
    Binary = iolist_to_binary(IoData),
    decode(Binary).

%% ------------------------------------------------------------------
%% Unit Tests
%% ------------------------------------------------------------------
-ifdef(TEST).

%% @private
bin_decode_test() ->
    run_test__("test_data/test.bin.plist", {ok, expected_generic_test_data()}).

%% @private
xml_decode_test() ->
    run_test__("test_data/test.xml.plist", {ok, expected_generic_test_data()}).

%% @private
uid_decode_bin_test() ->
    % taken from https://github.com/rodneyrehm/CFPropertyList
    run_test__("test_data/uid-list.plist", {ok, expected_uid_test_data()}).

%% @private
uid_decode_xml_test() ->
    % taken from https://github.com/rodneyrehm/CFPropertyList
    run_test__("test_data/uid-list.xml", {ok, expected_uid_test_data()}).

%% @private
bin_unicode_decode_test() ->
    run_test__("test_data/unicode.bin.plist", {ok, expected_unicode_test_data()}).

%% @private
xml_unicode_decode_test() ->
    run_test__("test_data/unicode.xml.plist", {ok, expected_unicode_test_data()}).

%% @private
float32_decode_test() ->
    run_test__("test_data/float32.bin.plist", {ok, #{<<"etc etc..">> => 1.0}}).

%% @private
run_test__(Path, ExpectedResult) ->
    {ok, Encoded} = file:read_file(Path),
    ?assertEqual(ExpectedResult, decode(Encoded)).

%% @private
expected_generic_test_data() ->
    #{<<"Lincoln">> =>
      #{<<"DOB">> => {{1809,2,12},{9,18,0}},
        <<"Eulogy">> =>
        <<183,5,15,253,33,21,131,249,92,108,122,222,146,36,48,160,
          55,207,61,19,93,90,230,12,62,148,58,9,80,96,143,66,31,
          185,212,80,46,38,211,39,178,206,171,74,45,85,147,156,
          163,192,214,241,175,82,155,80,17,145,231,188,98,37,206,
          74,238,249,180,115,103,174,202,250,152,186,185,42,89,84,
          189,191,69,196,134,21,63,96,106,0,52,224,177,7,41,108,
          77,50,128,206,137,167,52,237,20,147,139,204,227,182,93,
          79,2,206,138,30,60,3,248,35,31,164,98,207,116,224,244,
          24,204,63,173,160,113,53,114,230,166,104,141,6,0,151>>,
        <<"IsNamedGeorge">> => false,
        <<"Name">> => <<"Abraham Lincoln">>,
        <<"Scores">> =>
        [8,-8,512,-512,65536,-65536,4294967296,-4294967296,3.14,
         -3.14,4.900000095367432,-4.900000095367432]},
      <<"Washington">> =>
      #{<<"DOB">> => {{1732,2,17},{1,32,0}},
        <<"Eulogy">> =>
        <<19,220,90,23,220,229,7,1,57,36,3,160,249,63,76,151,169,
          162,164,181>>,
        <<"IsNamedGeorge">> => true,
        <<"Name">> => <<"George Washington">>,
        <<"Scores">> => [6,4.599999904632568,6]}}.

%% @private
expected_uid_test_data() ->
    #{<<"small">> => {uid,1},
      <<"medium">> => {uid,256},
      <<"large">> => {uid,65536},
      <<"huge">> => {uid,4294967296}}.

%% @private
expected_unicode_test_data() ->
    #{<<"A thing">> =>
      <<"他度於樂。實術醫收參不民對續子義發性親的論草像終因："
        "地己關物理應在她決精……在論被主馬路兩關保分怕件比人"
        "……孩經感發特，那經情你相，破族木相資個外生"
        "。熱民待……天女度之竟賽外的史次道就就只……動技會，"
        "世國系笑早，四司作方極中面：晚寫許或美大下正人濟"
        "保樂這候可著了下供半於受下唱素動古下。"/utf8>>}.

-endif.
