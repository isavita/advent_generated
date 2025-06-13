
/* REXX */
main:
    fileName = 'input.txt'
    data = CHARIN(fileName, 1, STREAM(fileName, 'C', 'QUERY SIZE'))

    score = 0
    garbage_count = 0
    in_garbage = 0
    ignore_next = 0

    DO i = 1 TO LENGTH(data)
        char = SUBSTR(data, i, 1)

        SELECT
            WHEN ignore_next THEN
                ignore_next = 0
            WHEN char = '!' THEN
                ignore_next = 1
            WHEN in_garbage THEN DO
                IF char = '>' THEN
                    in_garbage = 0
                ELSE
                    garbage_count = garbage_count + 1
            END
            WHEN char = '<' THEN
                in_garbage = 1
            WHEN char = '{' THEN
                score = score + 1
            OTHERWISE
                NOP
        END
    END

    SAY score
    SAY garbage_count
RETURN
