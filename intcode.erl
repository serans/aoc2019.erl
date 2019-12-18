-module(intcode).
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

-record(computer, {
          mem=[], %memory of the computer (list of integers)
          ic=0, % instruction counter
          modeA=undef, % memory access mode for 1st param of current instruction
          modeB=undef, % memory access mode for 2nd param of current instruction
          input=undef, % we'll ping this process when we need input
                       % not striclty needed but useful for interactive sessions.
          output=undef % process that will receive output of the program

         }).

%%%%%%%%%%%%%%%%%%
% Fetch / Decode %
%%%%%%%%%%%%%%%%%%
-define(DIRECT_ACCESS, 1).
-define(POINTER_ACCESS, 0).

% register operations (note that erlang is 1-indexed)
register_at(Mem, Addr) -> lists:nth(Addr + 1, Mem).
register_set(Mem, Addr, NewValue) ->
    lists:sublist(Mem, 1, Addr) ++ [NewValue] ++ lists:nthtail(Addr+1, Mem).

fetch_mem(Mem, Address, ?DIRECT_ACCESS)  -> register_at(Mem, Address);
fetch_mem(Mem ,Address, ?POINTER_ACCESS) -> register_at(Mem, register_at(Mem,Address)).

fetch_instruction(State) when State#computer.ic >= length(State#computer.mem) ->
    % inform output that we reached the end of the program
    State#computer.output ! eof;

fetch_instruction(State) ->

    Memory = State#computer.mem,
    IC = State#computer.ic,
    {OPCode,ModeA,ModeB} = decode(fetch_mem(Memory, IC, ?DIRECT_ACCESS)),

    NewState = State#computer{modeA=ModeA, modeB=ModeB},

    case OPCode of
        99 -> State#computer.output ! halted;
         1 -> add(NewState);
         2 -> mult(NewState);
         3 -> input(NewState);
         4 -> output(NewState);
         5 -> jnz(NewState);
         6 -> jz(NewState);
         7 -> less_than(NewState);
         8 -> equals(NewState)
    end.

decode(OP) ->
    OPCode = OP rem 100,
    ModeA  = (OP div 100) rem 10,
    ModeB  = (OP div 1000) rem 10,
    {OPCode, ModeA, ModeB}.

%%%%%%%%%%%%%
% Execution %
%%%%%%%%%%%%%
add(State) -> alu(State, fun(X,Y) -> X+Y end).
mult(State) -> alu(State, fun(X,Y) -> X*Y end).

less_than(State) -> alu(State, fun(X,Y) ->
        if X  < Y -> 1;
           X >= Y -> 0
        end
    end).

equals(State) -> alu(State, fun(X,Y) ->
        if X =:= Y -> 1;
           X =/= Y -> 0
        end
    end).

alu(State, Function) ->
    Mem = State#computer.mem,
    IC  = State#computer.ic,
    Op1 = fetch_mem(Mem, IC+1, State#computer.modeA),
    Op2 = fetch_mem(Mem, IC+2, State#computer.modeB),
    Dst = fetch_mem(Mem, IC+3, ?DIRECT_ACCESS),

    UpdatedMem = register_set(Mem, Dst, Function(Op1, Op2)),
    fetch_instruction(State#computer{mem = UpdatedMem, ic=IC+4}).

jnz(State) -> jump_if(State, fun(X) -> X =/= 0 end).
jz(State)  -> jump_if(State, fun(X) -> X =:= 0 end).

jump_if(State, JumpTest) ->
    Mem = State#computer.mem,
    IC = State#computer.ic,
    Op1 = fetch_mem(Mem, IC+1, State#computer.modeA),
    JumpDst = fetch_mem(Mem, IC+2, State#computer.modeB),

    case JumpTest(Op1) of
       true -> fetch_instruction(State#computer{ic = JumpDst});
       false -> fetch_instruction(State#computer{ic = IC+3})
    end.

input(State) when is_pid(State#computer.input) ->
    % If we have a process set as Input, then notify we're waiting for input
    State#computer.input ! awaiting_input,
    receive_input(State);

input(State) -> receive_input(State).

receive_input(State) ->
    Mem = State#computer.mem,
    IC = State#computer.ic,
    Dst = fetch_mem(Mem, IC +1, ?DIRECT_ACCESS),

    receive
        N -> NewMem = register_set(Mem, Dst, N),
             fetch_instruction(State#computer{mem=NewMem, ic=IC+2})
    end.

output(State) ->
    Mem = State#computer.mem,
    IC = State#computer.ic,
    State#computer.output ! fetch_mem(Mem, IC+1, State#computer.modeA),
    fetch_instruction(State#computer{ic = IC+2}).

%%%%%%%%%%%%%%%%%%%
% Running intcode %
%%%%%%%%%%%%%%%%%%%

run_file(FileName, Output, Input) ->
    {ok, FileContents} = file:read_file(FileName),
    SourceCode= string:trim(binary_to_list(FileContents)),
    run(SourceCode, Output, Input).

run(SourceCode, Output, Input) ->
    Instructions = [ list_to_integer(X) || X <- string:split(SourceCode, ",", all)],
    Program = #computer{mem = Instructions, input=Input, output=Output},
    spawn(intcode, fetch_instruction,[Program]).

% Interactive shell
shell(IntcodeMachine) when is_pid(IntcodeMachine)->
    receive
        awaiting_input ->
            case io:fread("[o_o] input? ","~d") of
                {ok, [N]} -> IntcodeMachine ! N,
                             shell(IntcodeMachine)
            end;
        N when is_integer(N) -> io:format("[^_^] intcode says: ~p~n",[N]),
               shell(IntcodeMachine);
        halted -> io:format("[-_-] intcode halted~n");
        eof    -> io:format("[x_x] intcode eof~n")
    end.

shell_load(FileName) ->
    IntcodeMachine = run_file(FileName, self(), self()),
    shell(IntcodeMachine).

shell_run(SourceCode) ->
    IntcodeMachine = run(SourceCode, self(), self()),
    shell(IntcodeMachine).

%%%%%%%%%
% Tests %
%%%%%%%%%
test() ->
    shell_run("3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99").
