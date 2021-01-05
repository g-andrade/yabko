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

%% @reference
%%
%% * [http://fileformats.archiveteam.org/wiki/Property_List/Binary](http://fileformats.archiveteam.org/wiki/Property_List/Binary)
%% * [https://en.wikipedia.org/wiki/Property_list#macOS](https://en.wikipedia.org/wiki/Property_list#macOS)
%% * [https://www.taksati.org/plists/](https://www.taksati.org/plists/)
%% * and misc. references glimpsed around the web

%% @private
-module(yabko_bin).

-include("yabko_common.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([decode/2]).

%% ------------------------------------------------------------------
%% Macro Definitions
%% ------------------------------------------------------------------

-define(SINGLETON, 0).
-define(INTEGER, 1).
-define(FLOAT, 2).
-define(DATE, 3).
-define(BINARY, 4).
-define(ASCII, 5).
-define(UTF16, 6).
-define(UID, 8).
-define(ARRAY, 10).
-define(SET, 12).
-define(DICT, 13).

-define(DATE_EPOCH, ({{2001,1,1},{0,0,0}})).

-define(assert(Condition, OrElse), ((Condition) orelse error(OrElse))).

%% ------------------------------------------------------------------
%% Type Definitions
%% ------------------------------------------------------------------

-type settings() ::
        #{ offset_size => 1..4,
           ref_size => 1 | 2,
           number_of_objects => uint32(),
           root_object_id => uint32(),
           offset_table_offset => offset() }.

-type uint32() :: 0..4294967295.

-type offset() :: uint32().

-type unresolved_objects() ::
        #{ offset() => unresolved_object() }.

-type unresolved_object() ::
        {term, undefined} |
        {term, boolean()} |
        {term, yabko:int64()} |
        {term, float()} |
        {term, calendar:datetime()} |
        {term, {uid, yabko:uint64()}} |
        {array, [unresolved_object()]} |
        {set, [unresolved_object()]} |
        {map, [{binary(), unresolved_object()}]}.

-type object_offsets() ::
        #{ object_id() => offset() }.

-type object_id() :: uint32().

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec decode(nonempty_binary(), non_neg_integer()) -> yabko:object() | no_return().
decode(EncodedPList, OffsetFromStart) ->
    TrailerOffset = byte_size(EncodedPList) - 32,
    <<EncodedObjectsAndOffsets:TrailerOffset/binary, Trailer:32/binary>> = EncodedPList,
    Settings = decode_trailer(Trailer),
    #{ number_of_objects := NumberOfObjects,
       offset_table_offset := OffsetTableOffset } = Settings,

    RealOffsetTableOffset = OffsetTableOffset - OffsetFromStart,
    <<EncodedObjects:RealOffsetTableOffset/binary, OffsetTable/binary>> = EncodedObjectsAndOffsets,
    ObjectsPerOffset = decode_objects(EncodedObjects, Settings, OffsetFromStart),
    ?assert(map_size(ObjectsPerOffset) =< NumberOfObjects,
            {mismatched_number_of_objects, ObjectsPerOffset, NumberOfObjects}),
    OffsetPerObjectId = decode_object_offsets(OffsetTable, Settings),
    ResolvedObjects = resolve_object_refs(ObjectsPerOffset, OffsetPerObjectId),
    reconstruct_tree(ResolvedObjects, Settings).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%-spec decode_trailer(nonempty_binary()) -> settings().
decode_trailer(<<0,0,0,0,0,0, OffsetSize, RefSize,
                 0,0,0,0, NumberOfObjects:32,
                 0,0,0,0, RootObjectId:32,
                 0,0,0,0, OffsetTableOffset:32>>)
  when OffsetSize >= 1, OffsetSize =< 4,
       RefSize >= 1, RefSize =< 2
->
    #{ offset_size => OffsetSize, % in bytes
       ref_size => RefSize, % in bytes
       number_of_objects => NumberOfObjects,
       root_object_id => RootObjectId,
       offset_table_offset => OffsetTableOffset
     }.

-spec decode_objects(binary(), settings(), non_neg_integer()) -> unresolved_objects().
decode_objects(Bin, Settings, InitialOffset) ->
    decode_objects_recur(Bin, Settings, InitialOffset, []).

-spec decode_objects_recur(binary(), settings(), non_neg_integer(),
                           [{offset(), unresolved_object()}]) -> unresolved_objects().
decode_objects_recur(<<>>, _Settings, _Offset, Acc) ->
    maps:from_list(Acc);
decode_objects_recur(Bin, Settings, Offset, Acc) ->
    {Value, Rest} = decode_object(Bin, Settings),
    NewOffset = Offset + (byte_size(Bin) - byte_size(Rest)),
    NewAcc = [{Offset, Value} | Acc],
    decode_objects_recur(Rest, Settings, NewOffset, NewAcc).

-spec decode_object(nonempty_binary(), settings()) -> {unresolved_object(), binary()}.
decode_object(<<?SINGLETON:4, 0:4, Rest/binary>>, _Settings) ->
    {{term, undefined}, Rest};
decode_object(<<?SINGLETON:4, 8:4, Rest/binary>>, _Settings) ->
    {{term, false}, Rest};
decode_object(<<?SINGLETON:4, 9:4, Rest/binary>>, _Settings) ->
    {{term, true}, Rest};
decode_object(<<?SINGLETON:4, 15:4, Rest/binary>>, Settings) ->
    % fill byte; skip it
    decode_object(Rest, Settings);
decode_object(<<?INTEGER:4, _/bits>> = Data, _Settings) ->
    {Uint, Rest} = decode_integer(Data),
    {{term, Uint}, Rest};
decode_object(<<?FLOAT:4, 2:4, Float32:32/float, Rest/binary>>, _Settings) ->
    {{term, Float32}, Rest};
decode_object(<<?FLOAT:4, 3:4, Float64:64/float, Rest/binary>>, _Settings) ->
    {{term, Float64}, Rest};
decode_object(<<?DATE:4, 3:4, Timestamp:64/float, Rest/binary>>, _Settings) ->
    TruncTimestamp = trunc(Timestamp),
    Epoch = calendar:datetime_to_gregorian_seconds(?DATE_EPOCH),
    Date = calendar:gregorian_seconds_to_datetime(Epoch + TruncTimestamp),
    {{term, Date}, Rest};
decode_object(<<?UID:4, 0:4, UID, Rest/binary>>, _Settings) ->
    {{term, {uid, UID}}, Rest};
decode_object(<<?UID:4, 1:4, UID:16, Rest/binary>>, _Settings) ->
    {{term, {uid, UID}}, Rest};
decode_object(<<?UID:4, 2:4, UID:32, Rest/binary>>, _Settings) ->
    {{term, {uid, UID}}, Rest};
decode_object(<<?UID:4, 3:4, UID:64, Rest/binary>>, _Settings) ->
    {{term, {uid, UID}}, Rest};
decode_object(<<TypeTag:4, SizeTag:4, Rest/binary>>, Settings)
  when TypeTag =:= ?BINARY;
       TypeTag =:= ?ASCII;
       TypeTag =:= ?UTF16;
       TypeTag =:= ?ARRAY;
       TypeTag =:= ?SET;
       TypeTag =:= ?DICT
->
    {Size, Rest2} = decode_varsized_object_size(TypeTag, SizeTag, Settings, Rest),
    <<Data:Size/binary, Rest3/binary>> = Rest2,
    Object = decode_varsized_object_data(TypeTag, Data, Settings),
    {Object, Rest3}.

%% ------------------------------------------------------------------
%% Internal Function Definitions - Variably sized objects
%% ------------------------------------------------------------------

decode_varsized_object_size(TypeTag, SizeTag, Settings, Data) ->
    {Length, Rest} = decode_varsized_object_length(SizeTag, Data),
    Size = calculate_varsized_object_size(TypeTag, Length, Settings),
    {Size, Rest}.

decode_varsized_object_length(SizeTag, Data) when SizeTag =:= 15 ->
    decode_integer(Data);
decode_varsized_object_length(SizeTag, Data) ->
    {SizeTag, Data}.

decode_integer(<<?INTEGER:4, 0:4, Uint8, Rest/binary>>) ->
    {Uint8, Rest};
decode_integer(<<?INTEGER:4, 1:4, Uint16:16, Rest/binary>>) ->
    {Uint16, Rest};
decode_integer(<<?INTEGER:4, 2:4, Uint32:32, Rest/binary>>) ->
    {Uint32, Rest};
decode_integer(<<?INTEGER:4, 3:4, Int64:64/signed, Rest/binary>>) ->
    {Int64, Rest}.

calculate_varsized_object_size(?BINARY, Length, _Settings) ->
    Length;
calculate_varsized_object_size(?ASCII, Length, _Settings) ->
    Length;
calculate_varsized_object_size(?UTF16, Length, _Settings) ->
    Length * 2;
calculate_varsized_object_size(?ARRAY, Length, Settings) ->
    Length * maps:get(ref_size, Settings);
calculate_varsized_object_size(?SET, Length, Settings) ->
    Length * maps:get(ref_size, Settings);
calculate_varsized_object_size(?DICT, Length, Settings) ->
    Length * 2 * maps:get(ref_size, Settings).

decode_varsized_object_data(?BINARY, Data, _Settings) ->
    {term, binary:copy(Data)};
decode_varsized_object_data(?ASCII, Data, _Settings) ->
    <<Decoded/binary>> = unicode:characters_to_binary(Data, latin1),
    {term, binary:copy(Decoded)};
decode_varsized_object_data(?UTF16, Data, _Settings) ->
    <<Decoded/binary>> = unicode:characters_to_binary(Data, utf16),
    {term, binary:copy(Decoded)};
decode_varsized_object_data(?ARRAY, Data, Settings) ->
    {array, decode_ref_sequence(Data, Settings)};
decode_varsized_object_data(?SET, Data, Settings) ->
    {set, decode_ref_sequence(Data, Settings)};
decode_varsized_object_data(?DICT, Data, Settings) ->
    HalfDataSize = byte_size(Data) div 2,
    <<EncodedKeyRefs:HalfDataSize/binary, EncodedValueRefs:HalfDataSize/binary>> = Data,
    KeyRefs = decode_ref_sequence(EncodedKeyRefs, Settings),
    ValueRefs = decode_ref_sequence(EncodedValueRefs, Settings),
    {map, lists:zip(KeyRefs, ValueRefs)}.

decode_ref_sequence(Data, #{ ref_size := RefSize }) ->
    decode_uint_sequence(Data, RefSize).

%% ------------------------------------------------------------------
%% Internal Function Definitions - Offsets
%% ------------------------------------------------------------------

-spec decode_object_offsets(binary(), settings()) -> object_offsets().
decode_object_offsets(Data, #{ offset_size := OffsetSize }) ->
    Offsets = decode_uint_sequence(Data, OffsetSize),
    KVList = enumerate(Offsets, 0),
    maps:from_list(KVList).

enumerate(List, Start) ->
    {Enumerated, _} =
        lists:mapfoldl(
          fun (Value, Counter) ->
                  {{Counter, Value}, Counter + 1}
          end,
          Start, List),
    Enumerated.

%% ------------------------------------------------------------------
%% Internal Function Definitions - Original Data Reconstruction
%% ------------------------------------------------------------------

resolve_object_refs(ObjectsPerOffset, OffsetPerObjectId) ->
    maps:map(
      fun (_ObjectId, Offset) ->
              maps:get(Offset, ObjectsPerOffset)
      end,
      OffsetPerObjectId).

reconstruct_tree(ResolvedObjects, #{ root_object_id := RootObjectId }) ->
    RootObject = maps:get(RootObjectId, ResolvedObjects),
    reconstruct_tree_recur(RootObject, ResolvedObjects, [RootObjectId]).

%
% TODO optimize the following (we're doing more object reconstructions than needed)
%
reconstruct_tree_recur({term, Value}, _ResolvedObjects, _RefPath) ->
    Value;
reconstruct_tree_recur({array, RefList}, ResolvedObjects, RefPath) ->
    lists:map(
      fun (Ref) ->
              assert_lack_of_cycles(Ref, RefPath),
              Value = maps:get(Ref, ResolvedObjects),
              reconstruct_tree_recur(
                Value, ResolvedObjects, [Ref | RefPath])
      end,
      RefList);
reconstruct_tree_recur({set, RefList}, ResolvedObjects, RefPath) ->
    List =
        lists:map(
          fun (Ref) ->
                  assert_lack_of_cycles(Ref, RefPath),
                  Value = maps:get(Ref, ResolvedObjects),
                  reconstruct_tree_recur(
                    Value, ResolvedObjects, [Ref | RefPath])
          end,
          RefList),
    Set = ordsets:from_list(List),
    ordsets:to_list(Set);
reconstruct_tree_recur({map, RefKVList}, ResolvedObjects, RefPath) ->
    KVList =
        lists:map(
          fun ({KeyRef, ValueRef}) ->
                  assert_lack_of_cycles(KeyRef, RefPath),
                  assert_lack_of_cycles(ValueRef, RefPath),
                  Key = maps:get(KeyRef, ResolvedObjects),
                  Value = maps:get(ValueRef, ResolvedObjects),
                  {reconstruct_tree_recur(
                     Key, ResolvedObjects, [KeyRef | RefPath]),
                   reconstruct_tree_recur(
                     Value, ResolvedObjects, [ValueRef | RefPath])}
          end,
          RefKVList),
    maps:from_list(KVList).

assert_lack_of_cycles(Ref, RefPathSoFar) ->
    lists:member(Ref, RefPathSoFar) andalso error({cycling_reference, Ref, RefPathSoFar}).

%% ------------------------------------------------------------------
%% Internal Function Definitions - Utilities
%% ------------------------------------------------------------------

decode_uint_sequence(Data, 1 = _UnitSize) ->
    decode_uint8_sequence(Data);
decode_uint_sequence(Data, 2 = _UnitSize) ->
    decode_uint16_sequence(Data);
decode_uint_sequence(Data, 3 = _UnitSize) ->
    decode_uint24_sequence(Data);
decode_uint_sequence(Data, 4 = _UnitSize) ->
    decode_uint32_sequence(Data).

decode_uint8_sequence(Data) ->
    binary_to_list(Data).

decode_uint16_sequence(Data) ->
    decode_uint16_sequence_recur(Data, []).

decode_uint16_sequence_recur(<<Ref:16, Rest/binary>>, Acc) ->
    decode_uint16_sequence_recur(Rest, [Ref | Acc]);
decode_uint16_sequence_recur(<<>>, Acc) ->
    lists:reverse(Acc).

decode_uint24_sequence(Data) ->
    decode_uint24_sequence_recur(Data, []).

decode_uint24_sequence_recur(<<Ref:24, Rest/binary>>, Acc) ->
    decode_uint24_sequence_recur(Rest, [Ref | Acc]);
decode_uint24_sequence_recur(<<>>, Acc) ->
    lists:reverse(Acc).

decode_uint32_sequence(Data) ->
    decode_uint32_sequence_recur(Data, []).

decode_uint32_sequence_recur(<<Ref:32, Rest/binary>>, Acc) ->
    decode_uint32_sequence_recur(Rest, [Ref | Acc]);
decode_uint32_sequence_recur(<<>>, Acc) ->
    lists:reverse(Acc).
