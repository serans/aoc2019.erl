-module(day10).
-compile(export_all).

-define(INPUT_FILE, "input/day10.txt").
-define(ASTEROID, 35).
% Asteroid map:
%   . -> empty
%   # -> asteroid
%
% Coordinates: X,Y (0,0) is left,top.
%
% Best asteriod for monitoring station
%   -> the one that can detect the most asteroids in straight line

asteroid_xy({X,Y,?ASTEROID}) -> {true, {X,Y}};
asteroid_xy(_) -> false.

read_asteroids(Input) -> read_asteroids(Input, 0, []).

read_asteroids(Input, Y, Asteroids) ->

    case io:get_line(Input,"") of
        eof  -> Asteroids;

        Line -> L = lists:droplast(Line), %remove \n
                Xcoord = lists:seq(0,length(L)-1),
                Ycoord = lists:duplicate(length(L), Y),
                Row = lists:zip3(Xcoord, Ycoord, L),

                RowAsteroids = lists:filtermap(fun(Elem) -> asteroid_xy(Elem) end, Row),

                read_asteroids(Input, Y+1, Asteroids ++ RowAsteroids)
    end.


problem0() ->
    {ok, Input} = file:open(?INPUT_FILE, [read]),
    Asteroids = read_asteroids(Input),

    CountVisible = fun({X0,Y0}) ->
        A = [ math:atan2(X0-X1, Y0-Y1) || {X1,Y1} <- Asteroids , {X1,Y1} =/= {X0,Y0} ],
        NumVisible = length(lists:usort(A)),
        {NumVisible,{X0,Y0}}
    end,

    Max = fun({Count0, Node0}, {Count1, Node1}) ->
        if Count0  > Count1 -> {Count0,Node0};
           Count0 =< Count1 -> {Count1,Node1}
        end
    end,

    lists:foldl(Max, {0, none}, lists:map( CountVisible, Asteroids)).

