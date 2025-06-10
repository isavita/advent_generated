
/* REXX */
call main

main:
    data = linein('input.txt')
    do 40
        data = look_and_say(data)
    end
    say length(data)
exit

look_and_say: procedure
    parse arg input_str
    result = ''
    i = 1
    len = length(input_str)
    do while i <= len
        char = substr(input_str, i, 1)
        count = 1
        do j = i + 1 to len
            if substr(input_str, j, 1) \= char then leave
            count = count + 1
        end
        result = result || count || char
        i = i + count
    end
    return result
