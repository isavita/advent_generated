-module(task).
-export([call/0]).

call() ->
    {ok, File} = file:open("input.txt", [read]),
    Answer = process_lines(File, 0),
    io:format("~p~n", [Answer]),
    file:close(File).

process_lines(File, Sum) ->
    case file:read_line(File) of
        {ok, Line} ->
            Half = length(Line) div 2,
            FirstCompartment = lists:sublist(Line, Half),
            SecondCompartment = lists:nthtail(Half, Line),
            CompartmentMap = lists:foldl(fun(Item, Map) -> 
                                            NewMap = maps:put(Item, maps:get(Item, Map, 0) + 1, Map),
                                            NewMap
                                        end, #{}, FirstCompartment),
            Sum1 = check_second_compartment(SecondCompartment, CompartmentMap, Sum),
            process_lines(File, Sum1);
        eof ->
            Sum
    end.

check_second_compartment([], _, Sum) ->
    Sum;
check_second_compartment([Item|Rest], CompartmentMap, Sum) ->
    case maps:is_key(Item, CompartmentMap) of
        true -> 
            Sum + item_priority(Item);
        false ->
            check_second_compartment(Rest, CompartmentMap, Sum)
    end.

item_priority(Item) when Item >= $a, Item =< $z ->
    Item - $a + 1;
item_priority(Item) when Item >= $A, Item =< $Z ->
    Item - $A + 27.