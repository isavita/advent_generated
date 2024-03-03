-module(task).
-export([call/0]).

call() ->
    {ok, File} = file:read_file("input.txt"),
    Passports = re:split(binary_to_list(File), "\n\n", [{return, binary}]),
    ValidCount = lists:foldl(fun(Passport, Acc) -> case is_valid(Passport) of true -> Acc + 1; false -> Acc end end, 0, Passports),
    io:format("~p~n", [ValidCount]).

is_valid(Passport) ->
    Fields = re:split(Passport, "[ \n]", [{return, list}]),
    RequiredFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"],
    lists:all(fun(F) -> lists:any(fun(X) -> lists:prefix(F, X) end, Fields) end, RequiredFields).