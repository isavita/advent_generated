
/* REXX */
CALL main
EXIT

main:
    total_price = 0
    rows = 0
    fname = 'input.txt'
    DO WHILE lines(fname) > 0
        rows = rows + 1
        line = linein(fname)
        cols = length(line)
        DO c = 1 TO cols
            grid.rows.c = substr(line, c, 1)
        END
    END
    CALL stream fname, 'c', 'close'

    visited. = 0
    DO r = 1 TO rows
        DO c = 1 TO cols
            IF visited.r.c = 0 THEN DO
                CALL calculate_region r, c
                PARSE VALUE result WITH area perimeter
                total_price = total_price + (area * perimeter)
            END
        END
    END
    SAY total_price
RETURN

calculate_region: PROCEDURE EXPOSE grid. rows cols visited.
    PARSE ARG r_start, c_start
    char = grid.r_start.c_start
    area = 0
    perimeter = 0
    q. = ''
    q.1 = r_start c_start
    q_head = 1
    q_tail = 1
    visited.r_start.c_start = 1

    dr.1 =  0; dc.1 =  1
    dr.2 =  0; dc.2 = -1
    dr.3 =  1; dc.3 =  0
    dr.4 = -1; dc.4 =  0

    DO WHILE q_head <= q_tail
        PARSE VAR q.q_head r c
        q_head = q_head + 1
        area = area + 1

        DO i = 1 TO 4
            nr = r + dr.i
            nc = c + dc.i
            IF nr >= 1 & nr <= rows & nc >= 1 & nc <= cols THEN DO
                IF grid.nr.nc \= char THEN
                    perimeter = perimeter + 1
                ELSE IF visited.nr.nc = 0 THEN DO
                    visited.nr.nc = 1
                    q_tail = q_tail + 1
                    q.q_tail = nr nc
                END
            END
            ELSE
                perimeter = perimeter + 1
        END
    END
RETURN area perimeter
