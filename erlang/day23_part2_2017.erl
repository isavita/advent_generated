-module(task).
-export([call/0]).

is_prime(N) -> 
    is_prime(N, 2).
is_prime(N, I) when I*I =< N -> 
    case N rem I of
        0 -> false;
        _ -> is_prime(N, I+1)
    end;
is_prime(_N, _I) -> 
    true.

call() ->
    B = 57*100 + 100000,
    C = B + 17000,
    H = count_primes(B, C, 0),
    io:format("~p~n", [H]).

count_primes(B, C, H) when B =< C -> 
    NewH = case is_prime(B) of
                true -> H;
                false -> H + 1
           end,
    count_primes(B+17, C, NewH);
count_primes(_B, _C, H) -> 
    H.