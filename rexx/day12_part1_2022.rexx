
/* Rexx */
call main
exit

main:
    file = 'input.txt'
    y = 0
    x_max = 0
    do y = 0 while lines(file) > 0
        line = linein(file)
        if x_max = 0 then x_max = length(line) - 1
        do x = 0 to x_max
            char = substr(line, x + 1, 1)
            select
                when char = 'S' then do
                    start = y x
                    grid.y.x = c2d('a')
                end
                when char = 'E' then do
                    end_pos = y x
                    grid.y.x = c2d('z')
                end
                otherwise
                    grid.y.x = c2d(char)
            end
        end
    end
    y_max = y - 1
    call lineout file

    dist. = -1
    queue = ''
    parse var end_pos end_y end_x
    dist.end_y.end_x = 0
    queue end_y end_x

    do while queued() > 0
        pull curr_y curr_x
        curr_h = grid.curr_y.curr_x
        curr_d = dist.curr_y.curr_x
        call process_neighbor curr_y - 1, curr_x, curr_h, curr_d
        call process_neighbor curr_y + 1, curr_x, curr_h, curr_d
        call process_neighbor curr_y, curr_x - 1, curr_h, curr_d
        call process_neighbor curr_y, curr_x + 1, curr_h, curr_d
    end

    parse var start start_y start_x
    say dist.start_y.start_x
return

process_neighbor:
    procedure expose grid. y_max x_max dist.
    parse arg next_y, next_x, curr_h, curr_d
    if next_y < 0 | next_y > y_max then return
    if next_x < 0 | next_x > x_max then return
    if dist.next_y.next_x <> -1 then return

    next_h = grid.next_y.next_x
    if curr_h - next_h > 1 then return

    dist.next_y.next_x = curr_d + 1
    queue next_y next_x
return
