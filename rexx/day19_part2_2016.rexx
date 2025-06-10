
/* REXX */
call main

main:
    num_elves = linein('input.txt')
    close('input.txt')
    say josephus(num_elves)
    return

josephus: procedure
    parse arg n
    i = 1
    do while i * 3 <= n
        i = i * 3
    end
    return n - i + max(n - 2 * i, 0)
