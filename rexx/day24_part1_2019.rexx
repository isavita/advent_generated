
/* Rexx */
call main
exit

main:
    side = 5
    square = side * side
    grid = parse(side)
    appeared. = 0

    do forever
        if appeared.grid then leave
        appeared.grid = 1
        grid = next_grid(grid, side, square)
    end

    say biodiversity(grid, square)
return

parse: procedure
    parse arg side
    grid = ''
    do side
        line = linein('input.txt')
        grid = grid || translate(strip(line), '10', '#.')
    end
    return grid

next_grid: procedure
    parse arg grid, side, square
    new_grid = ''
    do i = 0 to square - 1
        row = i % side
        col = i // side
        neighbours = 0
        if row > 0         then neighbours = neighbours + substr(grid, i - side + 1, 1)
        if row < side - 1  then neighbours = neighbours + substr(grid, i + side + 1, 1)
        if col > 0         then neighbours = neighbours + substr(grid, i - 1 + 1, 1)
        if col < side - 1  then neighbours = neighbours + substr(grid, i + 1 + 1, 1)

        current_state = substr(grid, i + 1, 1)
        select
            when current_state = '1' & neighbours \= 1 then
                new_grid = new_grid || '0'
            when current_state = '0' & (neighbours = 1 | neighbours = 2) then
                new_grid = new_grid || '1'
            otherwise
                new_grid = new_grid || current_state
        end
    end
    return new_grid

biodiversity: procedure
    parse arg grid, square
    bio = 0
    do i = 1 to square
        if substr(grid, i, 1) = '1' then
            bio = bio + 2**(i - 1)
    end
    return bio
