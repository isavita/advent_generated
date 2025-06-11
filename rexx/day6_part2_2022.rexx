
/* Rexx solution for finding the first N unique characters marker */
call main
exit

main:
    n = 14
    s = strip(charin('input.txt', 1, chars('input.txt')))

    do i = n to length(s)
        chunk = substr(s, i - n + 1, n)
        if has_unique_chars(chunk) then do
            say i
            exit
        end
    end
    return

has_unique_chars: procedure
    arg chunk
    seen. = 0
    do j = 1 to length(chunk)
        char = substr(chunk, j, 1)
        if seen.char = 1 then
            return 0 /* Not unique */
        seen.char = 1
    end
    return 1 /* Unique */
