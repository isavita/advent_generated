-module(task).
-export([call/0]).

call() ->
  {ok, Data} = file:read_file("input.txt"),
  Lines = string:split(binary_to_list(Data), "\n", all),
  RealRooms = [parse_room(Line) || Line <- Lines],
  RealRoomIds = [SectorId || {_Name, SectorId, _Checksum} <- RealRooms, is_real_room(_Name, SectorId, _Checksum)],
  SumOfRealRoomIds = lists:sum(RealRoomIds),
  io:format("Part 1: ~p~n", [SumOfRealRoomIds]),
  io:format("Part 2: ~p~n", [find_north_pole_object_room(RealRooms)]).

parse_room(Line) ->
  {match, [Name, SectorId, Checksum]} = re:run(Line, "(.*)-(\\d+)\\[(.*)\\]", [{capture, [1, 2, 3], list}]),
  {Name, list_to_integer(SectorId), Checksum}.

is_real_room(Name, _, Checksum) ->
  Letters = [C || C <- Name, C /= $-],
  SortedLetters = lists:sort([{count_letter(Letters, C), C} || C <- sets:to_list(sets:from_list(Letters))]),
  lists:sublist(Checksum, 5) == lists:sublist([C || {_, C} <- SortedLetters], 5).

count_letter(List, Char) ->
  lists:foldl(fun(X, Acc) -> if X == Char -> Acc + 1; true -> Acc end end, 0, List).

find_north_pole_object_room(Rooms) ->
  [SectorId || {Name, SectorId, _Checksum} <- Rooms, decrypt_name(Name, SectorId) == "northpole object storage"].

decrypt_name(Name, SectorId) ->
  Shift = SectorId rem 26,
  lists:map(
    fun(C) when C == $- -> $ ;
       (C) -> ($a + (C - $a + Shift) rem 26)
    end,
    Name
  ).