
/* Rexx solution */
call main
exit

main:
    fname = 'input.txt'
    nr = 0
    do while lines(fname) > 0
        nr = nr + 1
        line = linein(fname)
        if nr = 1 then nc = length(line)
        do c = 1 to nc
            grid.nr.c = substr(line, c, 1)
        end
    end
    call lineout fname

    dp. = -1
    total = 0
    do r = 1 to nr
        do c = 1 to nc
            if grid.r.c = 0 then
                total = total + dfs(r, c)
        end
    end
    say total
return

dfs: procedure expose grid. dp. nr nc
    arg r, c
    if dp.r.c \= -1 then return dp.r.c

    h = grid.r.c
    if h = 9 then do
        dp.r.c = 1
        return 1
    end

    sum_paths = 0
    dr_list = '1 -1 0 0'
    dc_list = '0 0 1 -1'
    do i = 1 to 4
        nr2 = r + word(dr_list, i)
        nc2 = c + word(dc_list, i)
        if nr2 > 0 & nr2 <= nr & nc2 > 0 & nc2 <= nc then
            if grid.nr2.nc2 = h + 1 then
                sum_paths = sum_paths + dfs(nr2, nc2)
    end

    dp.r.c = sum_paths
return sum_paths
