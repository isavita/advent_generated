
call main
exit

main:
    pw = 'fbgdceah'
    call read_instructions
    do i = instructions.0 to 1 by -1
        line = instructions.i
        w1 = word(line, 1)
        w2 = word(line, 2)
        select
            when w1 = 'swap' then do
                if w2 = 'position' then
                    call swap_positions word(line, 3), word(line, 6)
                else
                    call swap_letters word(line, 3), word(line, 6)
            end
            when w1 = 'rotate' then do
                if w2 = 'based' then
                    call derotate_letter word(line, 7)
                else do
                    steps = word(line, 3)
                    if w2 = 'left' then call rotate_right steps
                    else call rotate_left steps
                end
            end
            when w1 = 'reverse' then
                call reverse_span word(line, 3), word(line, 5)
            when w1 = 'move' then
                call move_position word(line, 6), word(line, 3)
        end
    end
    say pw
return

read_instructions:
    fname = 'input.txt'
    i = 0
    do while lines(fname) > 0
        i = i + 1
        instructions.i = linein(fname)
    end
    instructions.0 = i
    call lineout fname
return

swap_positions: procedure expose pw
    parse arg x, y
    x = x + 1; y = y + 1
    cx = substr(pw, x, 1); cy = substr(pw, y, 1)
    pw = overlay(cy, pw, x); pw = overlay(cx, pw, y)
return

swap_letters: procedure expose pw
    parse arg lx, ly
    call swap_positions pos(lx, pw) - 1, pos(ly, pw) - 1
return

rotate_left: procedure expose pw
    parse arg steps
    len = length(pw); steps = steps // len
    if steps > 0 then pw = substr(pw, steps + 1) || substr(pw, 1, steps)
return

rotate_right: procedure expose pw
    parse arg steps
    len = length(pw); steps = steps // len
    if steps > 0 then pw = substr(pw, len - steps + 1) || substr(pw, 1, len - steps)
return

derotate_letter: procedure expose pw
    parse arg letter
    idx = pos(letter, pw) - 1
    if idx // 2 = 1 then rot = -((idx + 1) % 2)
    else if idx \= 0 then rot = (6 - idx) % 2
    else rot = -1
    if rot > 0 then call rotate_right rot
    else if rot < 0 then call rotate_left abs(rot)
return

reverse_span: procedure expose pw
    parse arg x, y
    x = x + 1; y = y + 1
    middle = substr(pw, x, y - x + 1)
    pw = overlay(reverse(middle), pw, x)
return

move_position: procedure expose pw
    parse arg x, y
    x = x + 1; y = y + 1
    char = substr(pw, x, 1)
    pw = delstr(pw, x, 1)
    pw = insert(char, pw, y - 1)
return
