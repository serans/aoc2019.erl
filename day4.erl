-module(day4).
-compile(export_all).
% --- Day 4: Secure Container ---
%
% You arrive at the Venus fuel depot only to discover it's protected by a password. The Elves had written the password on a sticky note, but someone threw it out.
%
% However, they do remember a few key facts about the password:
%
%     It is a six-digit number.
%     The value is within the range given in your puzzle input.
%     Two adjacent digits are the same (like 22 in 122345).
%     Going from left to right, the digits never decrease; they only ever increase or stay the same (like 111123 or 135679).
%
% Other than the range rule, the following are true:
%
%     111111 meets these criteria (double 11, never decreases).
%     223450 does not meet these criteria (decreasing pair of digits 50).
%     123789 does not meet these criteria (no double).
%
% How many different passwords within the range given in your puzzle input meet these criteria?
%
% Your puzzle input is 402328-864247.

% Convert a number to a list of digits
digits(N) -> digits(N, []).

digits(N, Digits) when N =:= 0 -> Digits;
digits(N, Digits) -> digits(N div 10, [N rem 10] ++ Digits).

% Apply test to every N and N+1 in a list
any(Test, [X, Y | Rest ]) ->
    case Test(X,Y) of
        true -> true;
        false -> any(Test, [Y] ++ Rest)
    end;
any(_, _ ) -> false.

% rules
has_double_digits(N)     ->     any(fun(X,Y) -> X =:= Y end, digits(N)).
has_increasing_digits(N) -> not any(fun(X,Y) -> X  >  Y end, digits(N)).

%%%%%%%%%%
% Solver %
%%%%%%%%%%
solve(Min,Max,Rule) -> solve(Min,Max,Rule,Min,0).

solve(_  , Max, _   , N, Count) when N > Max -> Count;
solve(Min, Max, Rule, N, Count) ->
    case Rule(N) of
        true  -> solve(Min, Max, Rule, N+1, Count+1);
        false -> solve(Min, Max, Rule, N+1, Count)
    end.

%%%%%%%%%%%%%
% Problem 1 %
%%%%%%%%%%%%%
problem1() ->
    Problem1Rule = fun(N) -> has_double_digits(N) and has_increasing_digits(N) end,
    solve(402328,864247,Problem1Rule).

%%%%%%%%%%%%%
% Problem 2 %
%%%%%%%%%%%%%
% There must be at least one digit repeated *exactly* twice
% eg: 123444 BAD / 111122 GOOD
has_2n_group(N) -> has_2n_group(N, none, 0).
has_2n_group([], _, Count) -> Count =:= 2;
has_2n_group([H|T], Last, Count) when H =/= Last ->
    if Count =:= 2 -> true;
       Count =/= 2 -> has_2n_group(T,H,1)
    end;
has_2n_group([H|T], H, Count) -> has_2n_group(T,H,Count+1).

problem2() ->
    Problem1Rule = fun(N) -> has_2n_group(digits(N)) and has_increasing_digits(N) end,
    solve(402328,864247,Problem1Rule).

test() ->
    io:format("test 0: ~p~n", [digits(123456) =:= [1,2,3,4,5,6]  ]),
    io:format("test 1: ~p~n", [has_double_digits(123456) =:= false]),
    io:format("test 2: ~p~n", [has_double_digits(12) =:= false ]),
    io:format("test 3: ~p~n", [has_double_digits(11) =:= true]),
    io:format("test 4: ~p~n", [has_double_digits(122854) =:= true]).
