-module(day8).
-compile(export_all).

-define(WIDTH,  25).
-define(HEIGHT,  6).
-define(INPUT_FILE, "input/day8.txt").

% Part I:
% - each digit represents a pixel (left to right, top to bottom)
% - each image can have several layers
%
% Q: number of '1' digits multiplied by number of '2' digits in layer with fewer '0' digits
-define(LAYER_LEN, ?WIDTH * ?HEIGHT).

count(List, Element) ->
    lists:foldl(fun(X, Count) ->
                        case X-48 of Element -> Count+1;
                             _ -> Count
                        end
                end, 0, List).

checksum(RawImage) -> checksum(RawImage, {undef,undef}).

checksum([], {_, Value}) -> Value;

checksum(RawImage, {MinZeros, Value}) ->
    Layer = lists:sublist(RawImage, ?LAYER_LEN),
    Rest = lists:nthtail(?LAYER_LEN, RawImage),
    Zeros = count(Layer, 0),

    if MinZeros =/= undef, Zeros >= MinZeros -> checksum(Rest, {MinZeros, Value});

       Zeros <  MinZeros  -> NewValue = count(Layer, 1) * count(Layer, 2),
                             checksum(Rest, {Zeros, NewValue})
    end.

problem1() ->
    {ok, FileContents} = file:read_file(?INPUT_FILE),
    RawImage = string:trim(binary_to_list(FileContents)),
    io:format("Layer size ~p~n", [?LAYER_LEN] ),

    checksum(RawImage).

% Part 2
-define(TRANSPARENT, 2+48).

get_layers(RawImage) -> get_layers(RawImage, []).

get_layers([], Layers) -> Layers;

get_layers(RawImage, Layers) ->
    Layer = lists:sublist(RawImage, ?LAYER_LEN),
    Rest = lists:nthtail(?LAYER_LEN, RawImage),

    get_layers(Rest, [Layer] ++ Layers).

merge_layers(TL, BL) ->
    Merge = fun(Top, Bottom) ->
      case Top of
          ?TRANSPARENT -> Bottom;
          _ -> Top
      end
    end,

    lists:zipwith(Merge, TL, BL).

problem2() ->
    {ok, FileContents} = file:read_file(?INPUT_FILE),
    RawImage = string:trim(binary_to_list(FileContents)),
    Layers = get_layers(RawImage),

    Image = lists:foldl(fun(T, B) -> merge_layers(T,B) end, hd(Layers), Layers),
    display(Image).

display([]) -> io:format("~n",[]);
display([H | T]) ->
    case length(T) rem ?WIDTH of
        ?WIDTH-1 -> io:format("~n"),car(H);
        _ -> car(H)
    end,
    display(T).

car(C) when C =:= 50 -> io:format("T");
car(C) when C =:= 49 -> io:format("#");
car(C) when C =:= 48 -> io:format(" ").
