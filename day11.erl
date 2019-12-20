-module(day11).
-compile(export_all).

-define(BLACK,0).
-define(WHITE,1).

-define(UP,0).
-define(RIGHT,1).
-define(DOWN,2).
-define(LEFT,3).

-define(V_UP,   {0,-1}).
-define(V_DOWN, {0,+1}).
-define(V_LEFT, {-1,0}).
-define(V_RIGHT,{+1,0}).

-define(DIRECTIONS, [?V_UP, ?V_RIGHT, ?V_DOWN, ?V_LEFT]).

-define(ROTATE_LEFT, -1).
-define(ROTATE_RIGHT, 1).

%%%%%%%%%%%%%%%%%
% State Control %
%%%%%%%%%%%%%%%%%

%-record(path, { hist=[{0,0,?BLACK}], direction=?UP}).

% In problem 2 we start with a WHITE panel
-record(path, { hist=[{0,0,?WHITE}], direction=?UP}).

rotate(State, Sense) ->
    NDirection = mod(State#path.direction + Sense, 4),
    State#path{direction=NDirection}.

cell_colour(_,_,[]) -> ?BLACK;
cell_colour(X,Y,[{X,Y,Colour}|_]) -> Colour;
cell_colour(X,Y,[_|T]) -> cell_colour(X,Y,T).

current_colour(State) ->
    {_,_,Color} = hd(State#path.hist),
    Color.

paint(State, Colour) ->
    Hist = State#path.hist,
    {X,Y,_} = hd(Hist),
    State#path{hist=[{X,Y,Colour}] ++ Hist}.

advance(State) ->
    Hist = State#path.hist,

    V_Direction = lists:nth(State#path.direction + 1, ?DIRECTIONS),

    {X,Y} = v_add(hd(Hist), V_Direction),
    Colour = cell_colour(X,Y,Hist),

    State#path{hist = [{X,Y,Colour}] ++ Hist}.

% Aux functions
v_add({X0,Y0,_},{X1,Y1}) -> {X0+X1, Y0+Y1}.
mod(X,Y)->(X rem Y + Y) rem Y.

%%%%%%%%%
% Robot %
%%%%%%%%%

robot(Computer) ->
    robot_read_color(#path{}, Computer).

robot_read_color(State, Computer) ->
    receive
        awaiting_input -> Computer ! current_colour(State),
                          robot_paint(State, Computer);
        halted -> {ok, State};
        E -> io:format("ERROR unexpected: ~p~n",[E]), error
    end.

robot_paint(State, Computer) ->
    receive
        Colour when Colour =:= ?BLACK; Colour =:= ?WHITE ->
            NState = paint(State, Colour),
            robot_rotate(NState, Computer);
        E -> io:format("ERROR unexpected: ~p~n",[E]), error
    end.

robot_rotate(State, Computer) ->
    Rotation = receive
        0 -> rotate(State, ?ROTATE_LEFT);
        1 -> rotate(State, ?ROTATE_RIGHT);
        E -> io:format("ERROR unexpected: ~p~n",[E]), error
    end,
    NewPosition = advance(Rotation),
    robot_read_color(NewPosition, Computer).

%%%%%%%%%%%%%
% Problem I %
%%%%%%%%%%%%%

visited_cells({X,Y,Colour}, Visited) ->
    case maps:find({X,Y}, Visited) of
        {ok, _ } -> Visited;
        error -> maps:put({X,Y}, Colour, Visited)
    end.

problem1() ->
    Computer = intcode:load("input/day11.txt"),
    {ok, FinalState} = robot(Computer),

    VisitedCells = fun(A,B) -> visited_cells(A,B) end,

    Path = lists:foldl(VisitedCells, #{}, FinalState#path.hist),
    length(maps:keys(Path)).

order({X0,Y0,_},{X1,Y1,_}) ->
    if X0 < X1 -> true;
       X0 > X1 -> false;
       Y0 < Y1 -> true
    end.

problem2() ->
    Computer = intcode:load("input/day11.txt"),
    {ok, FinalState} = robot(Computer),

    FinalState.
