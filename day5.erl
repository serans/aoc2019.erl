-module(day5).
-compile(export_all).

% Will run on the Intcode computer (day2)
% new OPcode:
%   3,X -> reads into X
%   4,X -> outputs $X
%
% add Parameter mode:
%   0 -> position mode (as used in day2)
%   1 -> immediate mode (params as values)
%
% INSTRUCTIONS: ABCDE
%   DE - OPCODE
%   C,B,A - mode of 1st, 2nd, 3rd parameters

% Decode the instruction in the IC (Instruction Counter) and sends
% it to execution

-define(ADDRESS_MODE, 0).
-define(DIRECT_MODE, 1).

%%%%%%%%%%%%%%%%%%
% Fetch / Decode %
%%%%%%%%%%%%%%%%%%
decode(OP) ->
    OPCode = OP rem 100,
    ModeA  = (OP div 100) rem 10,
    ModeB  = (OP div 1000) rem 10,
    ModeC  = (OP div 10000),
    {OPCode, ModeA, ModeB, ModeC}.

fetch_mem(Regs, Address, 1) -> register_at(Regs, Address);
fetch_mem(Regs ,Address, 0) -> register_at(Regs, register_at(Regs,Address)).

fetch_instruction(Regs, IC) when IC >= length(Regs) -> eof;
fetch_instruction(Regs, IC) ->

    {OPCode,ModeA,ModeB,_} = decode(register_at(Regs, IC)),

    case OPCode of
        99 -> io:format("> HALTED. mem[0]=~p~n", [register_at(Regs, 0)]), halted;
         1 -> add(Regs, IC, ModeA, ModeB);
         2 -> mult(Regs, IC, ModeA, ModeB);
         3 -> input(Regs, IC);
         4 -> output(Regs, IC, ModeA);
         5 -> jnz(Regs, IC, ModeA, ModeB);
         6 -> jz(Regs, IC, ModeA, ModeB);
         7 -> less_than(Regs, IC, ModeA, ModeB);
         8 -> equals(Regs, IC, ModeA, ModeB)
    end.

% Some sugar: intcode is 0-indexed but erlang is 1-indexed.
register_at(Regs, Addr) -> lists:nth(Addr + 1, Regs).

register_replace(Regs, Addr, NewValue) ->
    lists:sublist(Regs, 1, Addr) ++ [NewValue] ++ lists:nthtail(Addr+1, Regs).
%%%%%%%%%%%%%
% Execution %
%%%%%%%%%%%%%
add(Regs, IC, ModeA, ModeB) -> alu(Regs,IC,ModeA,ModeB, fun(X,Y) -> X+Y end).
mult(Regs, IC, ModeA, ModeB) -> alu(Regs,IC,ModeA,ModeB, fun(X,Y) -> X*Y end).

less_than(Regs, IC, ModeA, ModeB) ->
    alu(Regs,IC,ModeA,ModeB, fun(X,Y) ->
                               if X  < Y -> 1;
                                  X >= Y -> 0
                               end
                             end).

equals(Regs, IC, ModeA, ModeB) ->
    alu(Regs,IC,ModeA,ModeB, fun(X,Y) ->
                               if X =:= Y -> 1;
                                  X =/= Y -> 0
                               end
                             end).

alu(Regs, IC, ModeA, ModeB, Function) ->
    Op1 = fetch_mem(Regs, IC+1, ModeA),
    Op2 = fetch_mem(Regs, IC+2, ModeB),
    Dst = register_at(Regs, IC+3),

    UpdatedRegs = register_replace(Regs, Dst, Function(Op1, Op2)),
    fetch_instruction(UpdatedRegs, IC + 4).

jnz(Regs, IC, ModeA, ModeB) -> jump_if(Regs, IC, ModeA, ModeB, fun(X) -> X =/= 0 end).
jz(Regs, IC, ModeA, ModeB)  -> jump_if(Regs, IC, ModeA, ModeB, fun(X) -> X =:= 0 end).

jump_if(Regs, IC, ModeA, ModeB, JumpTest) ->
    Op1 = fetch_mem(Regs, IC+1, ModeA),
    JumpDst = fetch_mem(Regs, IC+2, ModeB),

    case JumpTest(Op1) of
       true -> fetch_instruction(Regs, JumpDst);
       false -> fetch_instruction(Regs, IC + 3)
    end.

input(Regs, IC) ->
    Dst = register_at(Regs, IC +1),

    case io:fread("$ ","~d") of
        {ok, [N]} -> fetch_instruction(register_replace(Regs, Dst, N), IC+2)
    end.

output(Regs, IC, ModeA) ->
    io:format("out> ~p~n", [fetch_mem(Regs, IC+1, ModeA)]),
    fetch_instruction(Regs, IC+2).

%%%%%%%%%%%%%%%%%%%
% Running intcode %
%%%%%%%%%%%%%%%%%%%
load_file(FileName) ->
    {ok, Input} = file:read_file(FileName),
    RawProgram = string:trim(binary_to_list(Input)),
    load_string(RawProgram).

load_string(RawProgram) ->
    [ list_to_integer(X) || X <- string:split(RawProgram, ",", all)].

run(Program) when is_list(Program) -> fetch_instruction(Program, 0);
run(Program) -> fetch_instruction(load_string(Program), 0).
