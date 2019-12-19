-module(day12).
-compile(export_all).

-record(coor,{x=0,y=0,z=0}).
-record(moon,{pos=#coor{}, vel=#coor{}}).

moon(X,Y,Z) -> #moon{pos = #coor{x=X, y=Y, z=Z}}.

print_moon(Moon) ->
    io:format("pos=<x=~p, y=~p, z=~p>, vel=<x=~p, y=~p, z=~p>~n", [
              Moon#moon.pos#coor.x,
              Moon#moon.pos#coor.y,
              Moon#moon.pos#coor.z,
              Moon#moon.vel#coor.x,
              Moon#moon.vel#coor.y,
              Moon#moon.vel#coor.z
     ]).

pull(ValA, ValB) when ValA < ValB -> {+1, -1};
pull(ValA, ValB) when ValA > ValB -> {-1, +1};
pull(_, _) -> {0, 0}.

vdiff(Vdx, Vdy, Vdz, Moon) ->
    Vx = Moon#moon.vel#coor.x + Vdx,
    Vy = Moon#moon.vel#coor.y + Vdy,
    Vz = Moon#moon.vel#coor.z + Vdz,

    Moon#moon{vel={coor,Vx,Vy,Vz}}.

gravity({NameA,NameB}, State) ->
    A = maps:get(NameA, State),
    B = maps:get(NameB, State),
    {VdAx, VdBx} = pull(A#moon.pos#coor.x,B#moon.pos#coor.x),
    {VdAy, VdBy} = pull(A#moon.pos#coor.y,B#moon.pos#coor.y),
    {VdAz, VdBz} = pull(A#moon.pos#coor.z,B#moon.pos#coor.z),

    StateA = maps:put(NameA, vdiff(VdAx, VdAy, VdAz, A), State),
    StateAB= maps:put(NameB, vdiff(VdBx, VdBy, VdBz, B), StateA),

    StateAB.

velocity(Moon) ->
    {_, X,Y,Z} = Moon#moon.pos,
    {_, Xd, Yd, Zd} = Moon#moon.vel,
    Moon#moon{pos={coor, X+Xd, Y+Yd, Z+Zd}}.

energy(Moon) ->
    Potential = abs(Moon#moon.pos#coor.x)+
                abs(Moon#moon.pos#coor.y)+
                abs(Moon#moon.pos#coor.z),
    Kinetic = abs(Moon#moon.vel#coor.x)+
              abs(Moon#moon.vel#coor.y)+
              abs(Moon#moon.vel#coor.z),

    Potential * Kinetic.

combinations([]) -> [];
combinations([H | T]) -> [{H, X} || X <- T] ++ combinations(T).

run_simulation(InitState, NumSteps) ->
    MoonNames = maps:keys(InitState),
    Pairs = combinations(MoonNames),
    simulation_step(NumSteps, InitState, Pairs).

simulation_step(0, State, _) -> State;

simulation_step(N, OldState, Pairs) ->
    ApplyGravity = fun(Pair, State) ->
                           gravity(Pair,State)
                   end,
    ApplyVelocity = fun(MoonName, State) ->
                           Moon = maps:get(MoonName, State),
                           maps:put(MoonName, velocity(Moon), State)
                     end,

    StepGravity = lists:foldl(ApplyGravity, OldState, Pairs),
    StepVelocity = lists:foldl(ApplyVelocity, StepGravity, maps:keys(StepGravity)),
    simulation_step(N-1, StepVelocity, Pairs).

problem1() ->
    Moons = #{io => moon(17, -9, 4),
              eu => moon(2, 2, -13),
              gn => moon(-1, 5, -1),
              cl => moon(4, 7, -7)},

    Result = run_simulation(Moons, 1000),

    lists:foreach(fun(X) -> print_moon(X) end, maps:values(Result)),

    TotalEnergy = lists:foldl(fun(X,T) -> energy(X)+T end, 0, maps:values(Result)),
    io:format("TOTAL ENERGY: ~p~n",[TotalEnergy]).
