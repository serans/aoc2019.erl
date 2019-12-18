-module(day2).
-compile(export_all).

%%%%%%%%%%%%%%%%%%%
% Intcode Machine %
%%%%%%%%%%%%%%%%%%%

% Decode the instruction in the IC (Instruction Counter) and sends
% it to execution
fetch_instruction(Regs, IC) ->
    OP_HALT=99,
    OP_SUM=1,
    OP_MULT=2,

    case register_at(Regs, IC) of
        OP_HALT -> register_at(Regs,0);
        OP_SUM  -> execute(Regs, IC, fun(X,Y) -> X+Y end);
        OP_MULT -> execute(Regs, IC, fun(X,Y) -> X*Y end)
    end.

% Executes a function and increments the IC
execute(Regs, IC, Function) ->
    IC_INCREMENT=4,

    Op1Addr=register_at(Regs, IC+1), Op1=register_at(Regs, Op1Addr),
    Op2Addr=register_at(Regs, IC+2), Op2=register_at(Regs, Op2Addr),
    DestAddr=register_at(Regs, IC+3),

    UpdatedRegs = register_replace(Regs, DestAddr, Function(Op1, Op2)),
    fetch_instruction(UpdatedRegs, IC + IC_INCREMENT).

% Some sugar: intcode is 0-indexed but erlang is 1-indexed.
register_at(Regs, Addr) ->
    lists:nth(Addr + 1, Regs).

register_replace(Regs, Addr, NewValue) ->
    lists:sublist(Regs, 1, Addr) ++ [NewValue] ++ lists:nthtail(Addr+1, Regs).

%%%%%%%%%%%%%%%%%%%
% Running intcode %
%%%%%%%%%%%%%%%%%%%
load(FileName) ->
    {ok, Input} = file:read_file(FileName),
    RawProgram = string:trim(binary_to_list(Input)),
    [ list_to_integer(X) || X <- string:split(RawProgram, ",", all)].

run(Program) -> fetch_instruction(Program, 0).

%%%%%%%%%%%%
% Problems %
%%%%%%%%%%%%

% Problem 1, just run the intcode
problem1() -> run(load('input/day2.txt')).

% Problem 2, search which instructions produce a given result
problem2() -> problem2(load('input/day2.txt'),0).

problem2(_      , Number) when Number > 9999 -> undefined;
problem2(Program, Number) ->
    GOAL=19690720,

    Verb = Number rem 100,
    Noun = Number div 100,
    Result = run(register_replace(register_replace(Program, 1, Noun),2,Verb)),

    if Result =:= GOAL -> Number;
       Result =/= GOAL -> problem2(Program, Number+1)
    end.

