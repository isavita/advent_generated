
/* REXX */
main:
    y = 0
    a_count = 0
    height. = -1
    a_points. = ''
    filename = 'input.txt'

    DO y = 0 WHILE LINES(filename) > 0
        line = LINEIN(filename)
        DO x = 0 FOR LENGTH(line)
            p_char = SUBSTR(line, x + 1, 1)
            px = x
            py = y
            SELECT
                WHEN p_char == 'S' THEN DO
                    start_y = py; start_x = px
                    height.py.px = C2D('a')
                END
                WHEN p_char == 'E' THEN DO
                    end_y = py; end_x = px
                    height.py.px = C2D('z')
                END
                OTHERWISE DO
                    IF p_char == 'a' THEN DO
                        a_count = a_count + 1
                        a_points.a_count = py px
                    END
                    height.py.px = C2D(p_char)
                END
            END
        END
    END
    CALL STREAM filename, 'C', 'CLOSE'

    CALL dijkstra end_y, end_x

    min_dist = value('dist.'start_y'.'start_x, 999999)

    DO i = 1 TO a_count
        PARSE VAR a_points.i ay ax
        dist_a = value('dist.'ay'.'ax, 999999)
        min_dist = min(min_dist, dist_a)
    END

    SAY min_dist
EXIT

dijkstra:
    PARSE ARG start_node_y, start_node_x
    dist. = 999999
    dist.start_node_y.start_node_x = 0
    pq. = ''
    pq.1 = 0 start_node_y start_node_x
    pq_count = 1
    neighbors = '-1 0 1 0 0 -1 0 1'

    DO WHILE pq_count > 0
        min_prio = 999999
        min_idx = -1
        DO i = 1 TO pq_count
            PARSE VAR pq.i p . .
            IF p < min_prio THEN DO; min_prio = p; min_idx = i; END
        END

        PARSE VAR pq.min_idx curr_prio curr_y curr_x
        pq.min_idx = pq.pq_count
        pq_count = pq_count - 1

        IF curr_prio > dist.curr_y.curr_x THEN ITERATE

        temp_neighbors = neighbors
        DO 4
            PARSE VAR temp_neighbors dy dx temp_neighbors
            next_y = curr_y + dy
            next_x = curr_x + dx

            IF height.next_y.next_x = -1 THEN ITERATE
            IF height.curr_y.curr_x - height.next_y.next_x > 1 THEN ITERATE

            next_dist = dist.curr_y.curr_x + 1
            IF next_dist < dist.next_y.next_x THEN DO
                dist.next_y.next_x = next_dist
                pq_count = pq_count + 1
                pq.pq_count = next_dist next_y next_x
            END
        END
    END
RETURN
