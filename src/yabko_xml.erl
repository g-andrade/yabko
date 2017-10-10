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

-define(is_int64(V), ((V) >= -9223372036854775808 andalso ((V) =< 9223372036854775807))).

-define(assert(Condition), ((Condition) orelse exit(assertion_failed))).

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
    decode_elements(Element#xmlElement.content).

decode_elements(List) ->
    Filtered = filter_relevant_sequence_elements(List),
    lists:map(fun decode_element/1, Filtered).

%%
%% TODO null?
%%
decode_element(#xmlElement{ name = false }) ->
    false;
decode_element(#xmlElement{ name = true }) ->
    true;
decode_element(#xmlElement{ name = integer } = Element) ->
    EncodedInt = extract_element_text(Element),
    Int = binary_to_integer(EncodedInt),
    true = ?is_int64(Int),
    Int;
decode_element(#xmlElement{ name = real } = Element) ->
    EncodedFloat = extract_element_text(Element),
    binary_to_float(EncodedFloat);
decode_element(#xmlElement{ name = date } = Element) ->
    EncodedDate = extract_element_text(Element),
    iso8601:parse(EncodedDate);
%%
%% TODO uid?
%%
decode_element(#xmlElement{ name = data } = Element) ->
    EncodedData = extract_element_text(Element),
    base64:decode(EncodedData);
decode_element(#xmlElement{ name = string } = Element) ->
    Data = extract_element_text(Element),
    <<_/binary>> = unicode:characters_to_binary(Data, utf8);
decode_element(#xmlElement{ name = array } = Element) ->
    decode_elements(Element#xmlElement.content);
%%
%% TODO set?
%%
decode_element(#xmlElement{ name = dict } = Element) ->
    decode_dict_elements(Element#xmlElement.content).

decode_dict_elements(Elements) ->
    Filtered = filter_relevant_sequence_elements(Elements),
    decode_dict_elements_recur(Filtered, []).

decode_dict_elements_recur([KeyElement, ValueElement | Next], Acc) ->
    Key = extract_key_element_text(KeyElement),
    Value = decode_element(ValueElement),
    decode_dict_elements_recur(Next, [{Key,Value} | Acc]);
decode_dict_elements_recur([], Acc) ->
    maps:from_list(Acc).

extract_key_element_text(#xmlElement{ name = key } = Element) ->
    extract_element_text(Element).

extract_element_text(Element) ->
    Content = Element#xmlElement.content,
    #xmlText{ value = Text } = lists:keyfind(xmlText, 1, Content),
    iolist_to_binary(Text).

filter_relevant_sequence_elements(List) ->
    lists:filter(fun is_relevant_sequence_element/1, List).

is_relevant_sequence_element(#xmlElement{}) -> true;
is_relevant_sequence_element(_) -> false.
