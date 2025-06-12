
/* REXX */
call main

main:
    directions = CHARIN('input.txt', 1, CHARS('input.txt'))
    x = 0
    y = 0
    houses. = 0
    key = x'.'y
    houses.key = 1
    count = 1

    DO i = 1 TO LENGTH(directions)
        char = SUBSTR(directions, i, 1)
        SELECT
            WHEN char = '^' THEN y = y + 1
            WHEN char = 'v' THEN y = y - 1
            WHEN char = '>' THEN x = x + 1
            WHEN char = '<' THEN x = x - 1
        END
        key = x'.'y
        IF houses.key = 0 THEN DO
            houses.key = 1
            count = count + 1
        END
    END

    SAY count
EXIT
