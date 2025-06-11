
/* REXX */
call main

main:
    line = linein("input.txt")
    total = 0
    do forever
        parse var line step ',' line
        if step = '' then leave
        total = total + hash(step)
        if line = '' then leave
    end
    say total
return

hash: procedure
    parse arg s
    val = 0
    do i = 1 to length(s)
        val = (val + c2d(substr(s, i, 1))) * 17 // 256
    end
    return val
