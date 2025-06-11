
/* REXX */
main:
    call linein 'input.txt'
    parse var result start '-' end
    count = 0
    do i = start to end
        if isValid(i) then count = count + 1
    end
    say count
exit

isValid:
    parse arg s
    hasDouble = 0
    do j = 1 to length(s) - 1
        if substr(s, j, 1) > substr(s, j + 1, 1) then return 0
        if substr(s, j, 1) = substr(s, j + 1, 1) then hasDouble = 1
    end
    return hasDouble
