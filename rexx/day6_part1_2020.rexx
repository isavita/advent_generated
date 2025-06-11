
/* REXX */
call main
exit

main:
    total_count = 0
    group_answers = ''
    file_name = 'input.txt'

    do while lines(file_name) > 0
        line = linein(file_name)
        if line = '' then do
            total_count = total_count + length(group_answers)
            group_answers = ''
        end
        else do
            do i = 1 to length(line)
                char = substr(line, i, 1)
                if pos(char, group_answers) = 0 then
                    group_answers = group_answers || char
            end
        end
    end

    total_count = total_count + length(group_answers)
    say total_count
    call lineout file_name
return
