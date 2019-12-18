-module(day6).
-compile(export_all).
%
% Orbit map consists
% xxx)yyy -> Object yyy orbits around abject xxx
% All objects except COM orbit around 1 other object
%

% map {node name -> children }
add_orbit(Orbits, Child, Parent) ->
    NewOrbits = case maps:find(Child, Orbits) of
        {ok, {Grandchilden, none}} -> maps:put(Child,{Grandchilden,Parent},Orbits);
        error -> maps:put(Child,{[], Parent}, Orbits)
    end,

    case maps:find(Parent, NewOrbits) of
        {ok, {OldChildren, Grandpa} } -> maps:put(Parent, {OldChildren ++ [Child], Grandpa}, NewOrbits);
        error -> maps:put(Parent, {[Child], none}, NewOrbits)
    end.

% Finds out orbit length for all objects in the tree
orbits_length(Orbits) -> orbits_length(Orbits, [{0,"COM"}], 0).
orbits_length(_, [], Total) -> Total;
orbits_length(Orbits, [{Depth,Planet} | RestOfPlanets], Total) ->
    NewTotal = Total + Depth,

    case maps:find(Planet, Orbits) of
        % planet has no moons
        error -> orbits_length(Orbits, RestOfPlanets, NewTotal);
        % planet has moons -> add to frontier
        {ok, {Moons,_}} -> Frontier = RestOfPlanets ++ [ {Depth+1, X} || X <- Moons ],
                           orbits_length(Orbits, Frontier, NewTotal)
    end.


% Number of min transfers between "YOU" and "SAN" so that you both orbit the same object
search_transfer(Orbits, From, To) ->
    NumHops = search_path(Orbits, [{0,From}], [], To),
    NumHops -2.

search_path(_, [], _, _) -> no_solution;

search_path(_, [{Depth,Destination} |_], _, Destination) -> Depth;

search_path(Orbits, [{Depth, Planet} | Frontier], Visited, Destination) ->

    {ok, {Children, Parent}} = maps:find(Planet, Orbits),

    NVisited = [Planet] ++ Visited,

    NFrontier = Frontier ++ [ {Depth+1,X} || X <- Children ++ [Parent], X =/= none, not lists:member(X, NVisited), not lists:member(X, Frontier)],
    search_path(Orbits, NFrontier,NVisited, Destination).

%%%%%%%%%
% Input %
%%%%%%%%%
read_orbits(Input, Orbits) ->
    case io:fread(Input, [], "~s") of
        eof -> Orbits;
        {ok, X} -> [Planet, Moon]=string:split(X,")",all),
                   read_orbits(Input, add_orbit(Orbits, Moon, Planet))
    end.

%%%%%%%%%%%%
% PROBLEMS %
%%%%%%%%%%%%
problem1() ->
    {ok, Input} = file:open("input/day6.txt", [read]),
    Orbits = read_orbits(Input, #{}),
    orbits_length(Orbits).

problem2() ->
    {ok, Input} = file:open("input/day6.txt", [read]),
    Orbits = read_orbits(Input, #{}),
    search_transfer(Orbits, "YOU","SAN").

