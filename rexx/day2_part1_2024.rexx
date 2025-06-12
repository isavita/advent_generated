
/* REXX */
call main
exit

main:
    safe_count = 0
    file = 'input.txt'
    do while lines(file) > 0
        line = linein(file)
        if is_safe(line) then
            safe_count = safe_count + 1
    end
    call lineout file
    say safe_count
return

is_safe: procedure
    parse arg report
    is_increasing = 1
    is_decreasing = 1
    do i = 1 to words(report) - 1
        prev = word(report, i)
        curr = word(report, i + 1)
        diff = abs(prev - curr)
        if diff < 1 | diff > 3 then
            return 0
        if prev >= curr then
            is_increasing = 0
        if prev <= curr then
            is_decreasing = 0
    end
    return (is_increasing | is_decreasing)
