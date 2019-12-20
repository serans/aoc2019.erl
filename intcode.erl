-module(intcode).
-compile(export_all).

-record(computer, {
          mem=[], %memory of the computer (list of integers)
          ic=0, % instruction counter
          base=0, % base for relative mode memory access
          modeA=undef, % memory access mode for 1st param of current instruction
          modeB=undef, % memory access mode for 2nd param of current instruction
          modeC=undef, % memory access mode for 3rd param of current instruction
          input=undef, % we'll ping this process when we need input
                       % not striclty needed but useful for interactive sessions.
          output=undef % process that will receive output of the program

         }).

%%%%%%%%%%%%%%%%%
% Memory Access %
%%%%%%%%%%%%%%%%%

-define(RELATIVE_ACCESS, 2).
-define(DIRECT_ACCESS, 1).
-define(POINTER_ACCESS, 0).

% register operations (note that erlang is 1-indexed)

register_at(Mem, Addr) when Addr >= length(Mem) -> 0;
register_at(Mem, Addr) -> lists:nth(Addr + 1, Mem).

register_set(Mem, Addr, NewValue) ->
    NewMem = allocate_mem(Mem, Addr),
    lists:sublist(NewMem, 1, Addr) ++ [NewValue] ++ lists:nthtail(Addr+1, NewMem).

% It is possible to address a memory position which has not been initialized,
% in that case we expand the memory with zeros.
allocate_mem(Mem, Addr) when Addr >= length(Mem) ->
    Mem ++ [ 0 || _ <- lists:seq(0, Addr - length(Mem)) ];
allocate_mem(Mem, _) -> Mem.

set_mem(State, Address, Mode, Value) ->
    Offset = if Mode =:= ?RELATIVE_ACCESS -> State#computer.base;
                Mode =:= ?POINTER_ACCESS  -> 0
             end,
    Mem = State#computer.mem,
    ResolvedAddress = register_at(Mem, Address) + Offset,
    register_set(Mem, ResolvedAddress, Value).

fetch_mem(State, Address, ?DIRECT_ACCESS) ->
    register_at(State#computer.mem, Address);

fetch_mem(State, Address, ?POINTER_ACCESS) ->
    Mem = State#computer.mem,
    ResolvedAddress = register_at(Mem,Address),
    register_at(Mem, ResolvedAddress);

fetch_mem(State, Address, ?RELATIVE_ACCESS) ->
    Mem = State#computer.mem,
    ResolvedAddress  = register_at(Mem, Address) + State#computer.base,
    register_at(Mem, ResolvedAddress).

%%%%%%%%%%%%%%%%%%
% Fetch / Decode %
%%%%%%%%%%%%%%%%%%

fetch_instruction(State) when State#computer.ic >= length(State#computer.mem) ->
    % inform output that we have reached the end of the program
    State#computer.output ! eof;

fetch_instruction(State) ->

    IC = State#computer.ic,
    {OPCode,ModeA,ModeB,ModeC} = decode(fetch_mem(State, IC, ?DIRECT_ACCESS)),

    NewState = State#computer{modeA=ModeA, modeB=ModeB, modeC=ModeC},

    case OPCode of
        99 -> State#computer.output ! halted;
         1 -> add(NewState);
         2 -> mult(NewState);
         3 -> input(NewState);
         4 -> output(NewState);
         5 -> jnz(NewState);
         6 -> jz(NewState);
         7 -> less_than(NewState);
         8 -> equals(NewState);
         9 -> base(NewState)
    end.

decode(OP) ->
    OPCode = OP rem 100,
    ModeA  = (OP div 100) rem 10,
    ModeB  = (OP div 1000) rem 10,
    ModeC  = (OP div 10000) rem 10,
    {OPCode, ModeA, ModeB, ModeC}.

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
    IC  = State#computer.ic,
    Op1 = fetch_mem(State, IC+1, State#computer.modeA),
    Op2 = fetch_mem(State, IC+2, State#computer.modeB),

    UpdatedMem = set_mem(State, IC+3, State#computer.modeC, Function(Op1, Op2)),
    fetch_instruction(State#computer{mem = UpdatedMem, ic=IC+4}).

jnz(State) -> jump_if(State, fun(X) -> X =/= 0 end).
jz(State)  -> jump_if(State, fun(X) -> X =:= 0 end).

jump_if(State, JumpTest) ->
    IC = State#computer.ic,
    Op1 = fetch_mem(State, IC+1, State#computer.modeA),
    JumpDst = fetch_mem(State, IC+2, State#computer.modeB),

    case JumpTest(Op1) of
       true -> fetch_instruction(State#computer{ic = JumpDst});
       false -> fetch_instruction(State#computer{ic = IC+3})
    end.

base(State) ->
    IC = State#computer.ic,
    CurrentBase = State#computer.base,
    BaseOffset = fetch_mem(State, IC+1, State#computer.modeA),

    fetch_instruction(State#computer{ic=IC+2, base=CurrentBase+BaseOffset}).

input(State) when is_pid(State#computer.input) ->
    % If we have a process set as Input, then notify we're waiting for input
    State#computer.input ! awaiting_input,
    receive_input(State);

input(State) -> receive_input(State).

receive_input(State) ->
    IC = State#computer.ic,

    receive
        N -> NewMem = set_mem(State, IC+1, State#computer.modeA, N),
             fetch_instruction(State#computer{mem=NewMem, ic=IC+2})
    end.

output(State) ->
    IC = State#computer.ic,
    State#computer.output ! fetch_mem(State, IC+1, State#computer.modeA),
    fetch_instruction(State#computer{ic = IC+2}).

%%%%%%%%%%%%%%%%%%%
% Running intcode %
%%%%%%%%%%%%%%%%%%%

load(FileName, Output, Input) ->
    {ok, FileContents} = file:read_file(FileName),
    SourceCode= string:trim(binary_to_list(FileContents)),
    run(SourceCode, Output, Input).

run(SourceCode, Output, Input) ->
    Instructions = [ list_to_integer(X) || X <- string:split(SourceCode, ",", all)],
    Program = #computer{mem = Instructions, input=Input, output=Output},
    spawn(intcode, fetch_instruction,[Program]).

run(FileName) -> run(FileName, self(), self()).
load(SourceCode) -> load(SourceCode, self(), self()).

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
    IntcodeMachine = load(FileName),
    shell(IntcodeMachine).

shell_run(SourceCode) ->
    IntcodeMachine = run(SourceCode),
    shell(IntcodeMachine).

%%%%%%%%%
% Tests %
%%%%%%%%%

expect([]) -> ok;
expect([Expected|Rest]) ->
    receive
        Expected -> expect(Rest)
    end.

test() ->

    io:format("1) returns 999, 1000 or 1001 if INPUT is respectively < 8, ==8, >8~n"),
    Src1 = "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99",
    shell_default:flush(),

    P0 = run(Src1), P0 ! 7,
    expect([awaiting_input, 999, halted]),
    P1 = run(Src1), P1 ! 8,
    expect([awaiting_input,1000, halted]),
    P2 = run(Src1), P2 ! 9,
    expect([awaiting_input,1001, halted]),



    io:format("2) returns 1219070632396864~n"),
    run("1102,34915192,34915192,7,4,7,99,0"), expect([1219070632396864, halted]),

    io:format("3) Quine\n"),
    Quine="109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99",
    run(Quine),
    expect([109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99, halted]),

    all_ok.
