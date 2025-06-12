
/* REXX */
call main

main:
    filename = 'input.txt'
    i = 0
    do while lines(filename) > 0
        i = i + 1
        instr.i = linein(filename)
    end
    instr.0 = i

    reg.a = 0
    reg.b = 0
    ip = 1

    do while ip >= 1 & ip <= instr.0
        parse var instr.ip op p1 ',' p2
        p1 = strip(p1)
        p2 = strip(p2)
        next_ip = ip + 1

        select
            when op = 'hlf' then call value 'reg.'p1, value('reg.'p1) % 2
            when op = 'tpl' then call value 'reg.'p1, value('reg.'p1) * 3
            when op = 'inc' then call value 'reg.'p1, value('reg.'p1) + 1
            when op = 'jmp' then next_ip = ip + p1
            when op = 'jie' then do
                if value('reg.'p1) // 2 = 0 then next_ip = ip + p2
            end
            when op = 'jio' then do
                if value('reg.'p1) = 1 then next_ip = ip + p2
            end
        end
        ip = next_ip
    end

    say reg.b
return
