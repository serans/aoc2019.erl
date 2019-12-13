-module(day1).
-export( [solve/1] ).


calculate_fuel(Input, Accum) ->
    case io:fread(Input, [], "~d") of
        eof -> Accum;
        {ok, [Mass]} -> calculate_fuel(Input, Accum + required_fuel(Mass))
    end.


required_fuel(Mass) when Mass < 9 -> 0;
required_fuel(Mass) ->
    AddedFuel=trunc(Mass/3)-2,
    AddedFuel + required_fuel(AddedFuel).

solve(FileName) ->
    {ok, Input} = file:open(FileName, [read]),
    io:format("Required fuel:"),
    calculate_fuel(Input,0).

problem1() ->
    solve('day1.input').
