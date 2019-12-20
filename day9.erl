-module(day9).
-compile(export_all).

readout() ->
    receive
        halted -> ok;
        N -> io:format("~p~n",[N]), readout()
    end.

solve() ->
    io:format("Problem I~n"),
    Problem1 = intcode:load("input/day9.txt"),
    Problem1 ! 1,
    readout(),

    io:format("Problem I~n"),
    Problem2 = intcode:load("input/day9.txt"),
    Problem2 ! 2,
    readout().
