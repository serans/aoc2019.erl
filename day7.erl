-module(day7).
-compile(export_all).

-define(AMP_CHAIN_INPUT,0).
-define(AMP_COUNT, 5).
-define(INPUT_FILE, "input/day7.txt").
%%%%%%%%%%%%%%%%%%%%%%%
% Chain Amplificators %
%%%%%%%%%%%%%%%%%%%%%%%

feedback(LastValue, InputAmp) ->
    receive
        halted   -> LastValue;

        NewValue -> InputAmp ! NewValue,
                    feedback(NewValue, InputAmp)
    end.

chain_amps(Phases) -> chain_amps(none, Phases).

chain_amps(none, [Phase|Rest]) ->
    OutputAmp = intcode:run_file(?INPUT_FILE, self(), none),
    OutputAmp ! Phase,
    chain_amps(OutputAmp, Rest);

chain_amps(PrevAmp, [Phase | Rest]) ->
    ChainAmp = intcode:run_file(?INPUT_FILE, PrevAmp, none),
    ChainAmp ! Phase,
    chain_amps(ChainAmp, Rest);

chain_amps(InputAmp, []) ->
    InputAmp ! ?AMP_CHAIN_INPUT,
    feedback(no_output, InputAmp).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Find best configuration %
%%%%%%%%%%%%%%%%%%%%%%%%%%%
perms([]) -> [[]];
perms(L) -> [[H|T] || H <- L, T <- perms(L--[H])].

problem1() ->
    lists:max( lists:map(fun(X) -> chain_amps(X) end, perms([0,1,2,3,4]))).

problem2() ->
    lists:max( lists:map(fun(X) -> chain_amps(X) end, perms([5,6,7,8,9]))).
