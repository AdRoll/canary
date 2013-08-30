%% Copyright
-module(canary_utils).
-author("jesse").

%% API
-compile(export_all).

%% Contains a hodge-podge of utility functions used in the project.

pytime() ->
    pytime(erlang:now()).
pytime({MegaSecs, Secs, MicroSecs}) ->
    (1.0e+6 * MegaSecs) + Secs + (1.0e-6 * MicroSecs).


%% Join binaries together.
bjoin(List) ->
    bjoin(List, <<".">>).
bjoin([], _C) ->
    <<>>;
bjoin([H | Rest], BinaryChars) ->
    bjoin(H, Rest, BinaryChars).
bjoin(Head, List, BinaryChars) ->
    F = fun(Current, Acc) ->
        <<BinaryChars / binary, Current / binary, Acc / binary>>
    end,
    JoinedRest = lists:foldr(F, <<>>, List),
    <<Head / binary, JoinedRest / binary>>.


%% Split binaries using given binary char.
bsplit(Binary, SplittingChar) ->
    binary:split(Binary, SplittingChar, [global]).


%% Convert any object to binary
tobin(undefined) ->
    <<"">>;
tobin(X) when is_binary(X) ->
    X;
tobin(true) ->
    <<"t">>;
tobin(false) ->
    <<"f">>;
tobin(X) when is_list(X) ->
    list_to_binary(X);
tobin(X) when is_float(X) ->
    list_to_binary(float_to_list(X));
tobin(X) when is_integer(X) ->
    list_to_binary(integer_to_list(X));
tobin(X) when is_atom(X) ->
    atom_to_binary(X, utf8);
tobin(X) ->
    list_to_binary(io_lib:format("~s", [X])).


%% Convert any object to string
str(X) when is_list(X) ->
    X;
str(X) when is_binary(X) ->
    binary_to_list(X);
str(X) when is_float(X) ->
    float_to_list(X);
str(X) when is_integer(X) ->
    integer_to_list(X);
str(X) ->
    mochifmt:convert_field(X, "s").


tojson([]) ->
    <<"[]">>;
tojson(L) ->
    mochijson2:encode(L).


getpl(PL, Key) ->
    proplists:get_value(Key, PL).
getpl(PL, Key, Default) ->
    proplists:get_value(Key, PL, Default).
store_pl(PL, Key, Value) ->
    lists:keystore(Key, 1, PL, {Key, Value}).


%% Samples the passed function, executing it only a percentage of the time
%%  as dictated by the passed sampling on/off values.
%%
-spec rate_limited_exec(fun(), pos_integer(), pos_integer()) -> term().
rate_limited_exec(Fun, Numerator, Denominator)
    when is_integer(Numerator), Numerator > 0, is_integer(Denominator), Denominator > 0 ->

    case rate_limited_guard((Numerator / Denominator) * 100) of
        true ->
            Fun();
        false ->
            ok
    end.

-spec rate_limited_guard(pos_integer()) -> boolean().
rate_limited_guard(Percent) when Percent > 0 ->
    random:uniform() =< Percent / 100.0;
rate_limited_guard(_) ->
    false.


