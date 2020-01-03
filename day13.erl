-module(day13).
-compile(export_all).
% Computer sends 3 msgs:
% X (dist. from left)
% Y (dist. from top)
% title_id:
%   0 - empty
%   1 - wall
%   2 - block (can be broken by ball)
%   3 - horizontal paddle
%   4 - ball
%
-define(EMPTY  , 0).
-define(WALL   , 1).
-define(BLOCK  , 2).
-define(PADDLE , 3).
-define(BALL   , 4).

recvn(N) -> recvn(N,[]).

recvn(N, List) when N =:= 0 -> {ok, list_to_tuple(List)};
recvn(N, List) ->
    receive
        Item when is_integer(Item)-> recvn(N-1, List ++ [Item]);
        Message -> 
            if List =:= [] -> {msg, Message};
               List =/= [] -> throw("Unknown output")
            end
    end.

count_blocks(Acc, Type) ->
    case recvn(3) of
        {ok, {_,_,Type}} -> count_blocks(Acc+1, Type);
        {ok, {_,_,_}}    -> count_blocks(Acc, Type);
        {msg, halted}    -> Acc
    end.

problem1() ->
    % how many block tiles?
    intcode:load("input/day13.txt"),
    count_blocks(0,?BLOCK).
%
%--- Part Two ---
%
-define(JOYSTICK_LEFT, -1).
-define(JOYSTICK_NEUTRAL, 0).
-define(JOYSTICK_RIGHT, 1).

-record(state, { screen=[], score=0}).

% Find screen dimensions (I'll assume they don't change)
screen_dimensions(W,H) ->
    case recvn(3) of
        {ok, {X,Y,_}} -> screen_dimensions(max(W,X), max(H,Y));
        {msg, halted} -> {W,H}
    end.

screen_dimensions() ->
    intcode:load("input/day13.txt"),
    {W,H} = screen_dimensions(0,0),
    io:format("Screen: ~px~p ~n",[W,H]).

-define(WIDTH,  45).
-define(HEIGHT, 24).

%%%%%%%%%%%%%%%%%%
% Screen Helpers %
%%%%%%%%%%%%%%%%%%
mod(X,Y)->(X rem Y + Y) rem Y.
pos_to_xy(N) ->
    X = mod((N-1), ?WIDTH),
    Y = (N-1) div ?WIDTH,
    {X,Y}.

xy_to_pos({X,Y}) -> (X + ?WIDTH * Y) +1.

display([]) -> io:format("~n",[]);
display([H | T]) ->
    Sprite = case H of
        ?EMPTY -> " ";
        ?BLOCK -> "$";
        ?WALL  -> "#";
        ?BALL  -> "o";
        ?PADDLE-> "_"
    end,

    NewLine = (length(T) rem ?WIDTH) =:= 0,

    if NewLine     -> io:format("~s~n",[Sprite]);
       not NewLine -> io:format("~s",[Sprite])
    end,

    display(T).

list_set(List, Position, Value) ->
    lists:sublist(List, 1, Position-1) ++ [Value] ++ lists:nthtail(Position, List).

%%%%%%%%%%%%%%%%%%%%%%%
% Read Program Output %
%%%%%%%%%%%%%%%%%%%%%%%

initial_state() ->
    InitialScreen = lists:duplicate(?WIDTH * ?HEIGHT, ?EMPTY),
    #state{screen = InitialScreen, score=0}.


read_output(PrevGame) ->

    Game = if PrevGame =:= first_turn -> initial_state();
              PrevGame =/= first_turn -> PrevGame
           end,

    case recvn(3) of
        {ok, {-1, 0, Score}} ->
            read_output(Game#state{score=Score});
        {ok, { X, Y, TileId}} ->
            Screen = list_set(Game#state.screen, xy_to_pos({X,Y}), TileId), 
            read_output(Game#state{screen = Screen});
        {msg, Msg} -> {Msg, Game};
        _ -> throw("UNKNOWN")
    end.

find_block(L,Block) -> find_block(L#state.screen ,Block,0).
find_block([],_,_) -> none;
find_block([Block|_], Block, Pos) -> pos_to_xy(Pos);
find_block([_|RestOfScreen], Block, Pos) -> find_block(RestOfScreen, Block, Pos+1).


follow_ball(first_turn) -> ?JOYSTICK_NEUTRAL;
follow_ball(CurrScreen) ->
    % We only take the X coordinate
    {Paddle, _}   = find_block(CurrScreen, ?PADDLE),
    {Ball, _}     = find_block(CurrScreen, ?BALL),
    if Ball  <  Paddle -> ?JOYSTICK_LEFT;
       Ball =:= Paddle -> ?JOYSTICK_NEUTRAL;
       Ball  >  Paddle -> ?JOYSTICK_RIGHT
    end.


%%%%%%%%%%%%%%%%%%
% Turn mechanics %
%%%%%%%%%%%%%%%%%%

game_turn(Arcade) ->
    % This is the first turn, so there's no previous state to be updated
    game_turn(Arcade, first_turn).

game_turn(Arcade, Game) ->
    % Read Output
    {State, GameUpdate} = read_output(Game),
    io:format(os:cmd(clear)),
    display(GameUpdate#state.screen),
    io:format("SCORE: ~p~n",[GameUpdate#state.score]),

    % Play Action
    if State =:= halted ->
           io:format("GAME OVER~n");
       State =/= halted -> 
           JoystickAction = follow_ball(GameUpdate),
           Arcade ! JoystickAction,
           game_turn(Arcade, GameUpdate)
    end.

problem2() ->
    Arcade = intcode:load("input/day13.txt"),
    game_turn(Arcade). 
