-module(day10).
-compile(export_all).

-define(INPUT_FILE, "input/day10.txt").
-define(ASTEROID, 35).
% Asteroid map:
%   . -> empty
%   # -> asteroid
%
% Coordinates: X,Y (0,0) is left,top.

asteroid_ay({X,Y,?ASTEROID}) -> {true, {X,Y}};
asteroid_ay(_) -> false.

read_asteroids() ->
    {ok, Input} = file:open(?INPUT_FILE, [read]),
    read_asteroids(Input, 0, []).

read_asteroids(Input, Y, Asteroids) ->

    case io:get_line(Input,"") of
        eof  -> Asteroids;

        Line -> L = lists:droplast(Line), %remove \n
                Xcoord = lists:seq(0,length(L)-1),
                Ycoord = lists:duplicate(length(L), Y),
                Row = lists:zip3(Xcoord, Ycoord, L),

                RowAsteroids = lists:filtermap(fun(Elem) -> asteroid_ay(Elem) end, Row),

                read_asteroids(Input, Y+1, Asteroids ++ RowAsteroids)
    end.

angle({X0,Y0}, {X1,Y1}) ->
    Atan = math:atan2(X0-X1, Y1-Y0),

    % Make atan always positive for easier ordering
    if Atan < 0  -> Atan+2*math:pi();
       Atan >= 0 -> Atan
    end.

distance({X0,Y0}, {X1,Y1}) -> math:sqrt(math:pow(X0-X1,2) + math:pow(Y0-Y1,2)).

% Problem 1
% Best asteriod for setting up monitoring station
%   -> the one that can detect the most asteroids in straight line
problem1() ->
    Asteroids = read_asteroids(),

    CountVisible = fun(P0) ->
        Angles = [ angle(P0, P1) || P1 <- Asteroids , P0 =/= P1 ],
        NumVisible = length(lists:usort(Angles)),
        {NumVisible,P0}
    end,

    Max = fun({Count0, Node0}, {Count1, Node1}) ->
        if Count0  > Count1 -> {Count0,Node0};
           Count0 =< Count1 -> {Count1,Node1}
        end
    end,

    lists:foldl(Max, {0, none}, lists:map( CountVisible, Asteroids)).

% Problem 2:
%
% Detect which asteroid will be the 200th asteroid to be vaporized
% - Laser starts pointing upwards, and rotate clockwise
% - Only 1 asteroid can be vaporized at a time

line_of_sight(Base, Asteroids) -> line_of_sight(Base, Asteroids, #{}).
line_of_sight(_, [], Map) -> Map;

line_of_sight(Base, [Base | Rest], Map) -> line_of_sight(Base, Rest, Map);
line_of_sight(Base, [A | Rest], Map) ->
    Angle = angle(A, Base),
    Distance = distance(Base, A),

    case maps:find(Angle, Map) of
        error -> line_of_sight(Base, Rest, maps:put(Angle, [Distance], Map));
        {ok, List} -> NewList = lists:sort([Distance] ++ List),
                      line_of_sight(Base, Rest, maps:put(Angle, NewList, Map))
    end.

laser_round(Angles, Targets) -> laser_round([], Angles, Targets).

laser_round(Vaporized, [], Targets) ->
    {Vaporized, Targets};

laser_round(Vaporized, [CurrentAngle | NextAngle], Targets) ->
    Distances = maps:get(CurrentAngle, Targets),
    TargetDistance = hd(Distances),
    ShadowTargets = tl(Distances),
    MissingTargets = if ShadowTargets =:= [] -> maps:remove(CurrentAngle, Targets);
                        ShadowTargets =/= [] -> maps:put(CurrentAngle, ShadowTargets, Targets)
                     end,
    VaporizedTarget = {TargetDistance, CurrentAngle},
    laser_round(Vaporized ++ [VaporizedTarget], NextAngle, MissingTargets).

vaporize(Targets) -> vaporize([], Targets).
vaporize(Vaporized, Targets) ->
      case maps:keys(Targets) of
          [] -> Vaporized;

          Angles  -> SortedAngles = lists:sort(Angles),
               {VaporizedInRound, MissingTargets} = laser_round(SortedAngles, Targets),
               vaporize(Vaporized ++ VaporizedInRound, MissingTargets)
      end.

cartesian({Dist, Angle},{OrigX, OrigY}) ->
    {
      round(OrigX + math:sin(Angle) * Dist),
      round(OrigY - math:cos(Angle) * Dist)
    }.

problem2() ->

    % Base position from problem1 solution.
    {_,Base} = problem1(),
    io:format("Base: ~p~n",[Base]),
    Asteroids = read_asteroids(),

    % Group asteroids by angle to base, sorted by distance
    LineOfSight = line_of_sight(Base, Asteroids),

    % 200th vaporized meteorite
    cartesian(lists:nth(200, vaporize(LineOfSight)), Base).

