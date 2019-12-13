-module(day3).
-compile(export_all).

% Wire description:
% - Wires all originate in same point
% - Each segment described as {R,L,U,D}[0-9]
% - eg: R8,U5,L5,D3
%
%   ...........
%   ....+----+.
%   ....|....|.
%   ....|....|.
%   ....|....|.
%   .........|.
%   .o-------+.
%   ...........
%
% INPUT: file describing 2 wires (1 per line)
% PROBLEM 1: manhattan distance from origin to closest wire crossing
% PROBLEM 2: crossing where CABLE DISTANCE is the lowest


%%%%%%%%%%%%%%%
% Parse Input %
%%%%%%%%%%%%%%%

cable_lines(CableDescription) ->
    Movements = string:split(CableDescription, ",",all),
    cable_lines(Movements, {0,0}, []).

cable_lines([], _, Lines) -> Lines;
cable_lines([Movement | Rest], {X0,Y0}, Lines) ->
    Direction = string:slice(Movement,0,1),
    Length = list_to_integer(lists:nthtail(1,Movement)),

    {X1,Y1} = case Direction of
       "R" -> {X0 + Length, Y0         };
       "L" -> {X0 - Length, Y0         };
       "U" -> {X0,          Y0 + Length};
       "D" -> {X0,          Y0 - Length}
    end,

    cable_lines(Rest,{X1,Y1}, Lines ++ [ {{X0,Y0},{X1,Y1}} ]). %order_points({X0,Y0},{X1,Y1}) ]).

% Makes sure a point is "ordered" (ie: X0,Y0 is lower-left)
% order_points({X0,Y0},{X1,Y1}) when X0 < X1 ; Y0 < Y1 -> {{X0,Y0},{X1,Y1}};
% order_points({X0,Y0},{X1,Y1})                        -> {{X1,Y1},{X0,Y0}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Calculate Crossing points %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Vertical x Horizontal
crossing({{Ax,Y},{Bx,Y}}, {{X,Cy},{X,Dy}}) ->
     case {intersec({Ax,Bx},{X,X}), intersec({Cy,Dy},{Y,Y})} of
         {[X],[Y]} -> [{X,Y}];
         _         -> []
     end;
% Horizontal x Vertical
crossing({{X,Cy},{X,Dy}}, {{Ax,Y},{Bx,Y}}) ->
     case {intersec({Ax,Bx},{X,X}), intersec({Cy,Dy},{Y,Y})} of
         {[X],[Y]} -> [{X,Y}];
         _         -> []
     end;

% Both on plane X
crossing({{X,Ax},{X,Ay}}, {{X,Bx},{X,By}}) ->
    case intersec({Ax,Ay},{Bx,By}) of
        [] -> [];
        Yn -> [ {X,Y} || Y <- Yn ]
    end;
% Both on plane y
crossing({{Ax,Y},{Ay,Y}}, {{Bx,Y},{By,Y}}) ->
    case intersec({Ax,Ay},{Bx,By}) of
        [] -> [];
        Xn -> [ {X,Y} || X <- Xn ]
    end;

crossing(_, _) -> [] .

% Ordering points to reduce number of cases
intersec({A1,A2}, B) when A1 > A2 -> intersec({A2,A1},B);
intersec(A, {B1,B2}) when B1 > B2 -> intersec(A,{B2,B1});
% Intersecting ranges for two 1-D lines
%   cases
%             A1----------A2  B1--B2
%             A1------B1==A2--B2
%             A1--B1==B2--A2
%         B1--A1==B2------A2
%         B1--A1==========A2--B2
%     B1--B2  A1----------A2
%
intersec({A1,A2},{B1,B2}) when A1 =< B1, B1 =< A2, A2 =< B2 -> lists:seq(B1,A2);
intersec({A1,A2},{B1,B2}) when A1 =< B1, B1 =< B2, B2 =< A2 -> lists:seq(B1,B2);
intersec({A1,A2},{B1,B2}) when B1 =< A1, A1 =< B2, B2 =< A2 -> lists:seq(A1,B2);
intersec({A1,A2},{B1,B2}) when B1 =< A1, A1 =< A2, A2 =< B2 -> lists:seq(A1,A2);
intersec(_, _) -> [].


%%%%%%%%%%%%
% Problems %
%%%%%%%%%%%%

cable_len(Cable, Point) -> follow_cable(Cable, Point, 0).

follow_cable([ {{X ,Ay},{X ,By}} | Rest ], {X,Y}, Cost) ->
    case intersec({Ay,By}, {Y,Y}) of
       []  -> follow_cable(Rest, {X,Y}, Cost + By-Ay);
       [P] -> Cost + abs(Ay-P)
    end;

follow_cable([ {{Ax ,Y},{Bx ,Y}} | Rest ], {X,Y}, Cost) ->
    case intersec({Ax,Bx}, {X,X}) of
       []  -> follow_cable(Rest, {X,Y}, Cost + abs(Bx-Ax));
       [P] -> Cost + abs(Ax-P)
    end;

follow_cable( [ {{Ax,Ay},{Bx,By}} | Rest ], Point, Cost) ->
    SegmentCost = abs(Ax-Bx) + abs(Ay-By),
    follow_cable(Rest, Point, Cost + SegmentCost).


solve(FileName) ->
    {ok, Input} = file:read_file(FileName),
    [A, B] = string:split(string:trim(binary_to_list(Input)),"\n",all),
    CableA=cable_lines(A),
    CableB=cable_lines(B),

    Solution1=solve(CableA, CableB, fun({X,Y}) -> abs(X) + abs(Y) end),

    CostFunction2 = fun(P) -> cable_len(CableA, P) + cable_len(CableB, P) end,
    Solution2=solve(CableA, CableB, CostFunction2),

    io:format("Solution 1:~p~n", [Solution1]),
    io:format("Solution 2:~p~n", [Solution2]).

solve(CableA, CableB, CostFunction) ->
    FindBest = fun({LineA, LineB}, Best) ->
        case crossing(LineA, LineB) of
            []     -> Best;
            Points ->
                % Ignore points at origin as per problem description
                CostList = [ CostFunction(P) || P <- Points, P =/= {0,0} ],

                if CostList == [] -> Best;
                   Best =:= none -> lists:min(CostList);
                   Best =/= none -> lists:min(CostList ++ [Best])
                end
        end
    end,

    AllCables = [ {LineA, LineB} || LineA <- CableA, LineB <- CableB],

    lists:foldl(FindBest, none, AllCables).

