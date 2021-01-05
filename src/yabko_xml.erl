%% Copyright (c) 2017-2021 Guilherme Andrade <yabko@gandrade.net>
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

%% @private
-module(yabko_xml).

-include_lib("xmerl/include/xmerl.hrl").
-include("yabko_common.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([decode/1]).

%% ------------------------------------------------------------------
%% Macro Definitions
%% ------------------------------------------------------------------

-define(is_between(V, Min, Max), ((V) >= (Min) andalso (V) =< (Max))).
-define(is_int64(V), ?is_between(V, -9223372036854775808, 9223372036854775807)).
-define(is_uint64(V), ?is_between(V, 0, 18446744073709551615)).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec decode(nonempty_binary()) -> yabko:object() | no_return().
decode(Binary) ->
    String = binary_to_list(Binary),
    {Root, _Rest} = xmerl_scan:string(String),
    decode_root(Root).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

decode_root(#xmlElement{ name = plist } = Element) ->
    Children = Element#xmlElement.content,
    [RootElement] = filter_relevant_sequence_elements(Children),
    decode_element(RootElement).

%%
%% TODO null?
%%
decode_element(#xmlElement{ name = false }) ->
    false;
decode_element(#xmlElement{ name = true }) ->
    true;
decode_element(#xmlElement{ name = integer } = Element) ->
    EncodedInt = extract_element_text(Element, latin1),
    Int = binary_to_integer(EncodedInt),
    true = ?is_int64(Int),
    Int;
decode_element(#xmlElement{ name = real } = Element) ->
    EncodedFloat = extract_element_text(Element, latin1),
    binary_to_float(EncodedFloat);
decode_element(#xmlElement{ name = date } = Element) ->
    EncodedDate = extract_element_text(Element, latin1),
    iso8601:parse(EncodedDate);
decode_element(#xmlElement{ name = data } = Element) ->
    EncodedData = extract_element_text(Element, latin1),
    base64:decode(EncodedData);
decode_element(#xmlElement{ name = string } = Element) ->
    extract_element_text(Element, utf8);
decode_element(#xmlElement{ name = array } = Element) ->
    decode_elements(Element#xmlElement.content);
decode_element(#xmlElement{ name = dict } = Element) ->
    decode_dict_elements(Element#xmlElement.content).

decode_elements(List) ->
    Filtered = filter_relevant_sequence_elements(List),
    lists:map(fun decode_element/1, Filtered).

decode_dict_elements(Elements) ->
    Filtered = filter_relevant_sequence_elements(Elements),
    decode_dict_elements_recur(Filtered, []).

decode_dict_elements_recur([KeyElement, ValueElement | Next], Acc) ->
    Key = extract_key_element_text(KeyElement),
    Value = decode_element(ValueElement),
    decode_dict_elements_recur(Next, [{Key,Value} | Acc]);
decode_dict_elements_recur([], Acc) ->
    case Acc of
        [{<<"CF$UID">>, Integer}] when ?is_uint64(Integer) ->
            % UID - special case
            {uid, Integer};
        _ ->
            maps:from_list(Acc)
    end.

extract_key_element_text(#xmlElement{ name = key } = Element) ->
    extract_element_text(Element, utf8).

extract_element_text(Element, InEncoding) ->
    Content = Element#xmlElement.content,
    #xmlText{ value = Text } = lists:keyfind(xmlText, 1, Content),
    <<Binary/binary>> = unicode:characters_to_binary(Text, InEncoding),
    binary:copy(Binary).

filter_relevant_sequence_elements(List) ->
    lists:filter(fun is_relevant_sequence_element/1, List).

is_relevant_sequence_element(#xmlElement{}) -> true;
is_relevant_sequence_element(_) -> false.
