-module(yabko).

-include("yabko_common.hrl").

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

-spec decode(nonempty_binary()) -> {ok, object()} | {error, {exception, atom(), term(), [term()]}}.
decode(<<"bplist", Version:2/binary, EncodedPList/binary>>) when Version =:= <<"00">>;
                                                                 Version =:= <<"01">> ->
    try yabko_bin:decode(EncodedPList, 8) of
        PList -> {ok, PList}
    catch
        Class:Reason ->
            {error, {exception, Class, Reason, erlang:get_stacktrace()}}
    end.
