
/* Rexx */
CALL main
EXIT

main:
    bots. = ''
    bots.chips.0 = 0
    ready_queue = ''
    filename = 'input.txt'

    DO WHILE LINES(filename) > 0
        line = LINEIN(filename)
        type = WORD(line, 1)

        IF type = 'value' THEN DO
            PARSE VAR line 'value' val 'goes to' id
            CALL give_chip format_id(id), val
        END
        ELSE DO
            PARSE VAR line id 'gives low to' low_to 'and high to' high_to
            id = format_id(id)
            bots.id.lowTo = format_id(low_to)
            bots.id.highTo = format_id(high_to)
        END
    END
    CALL LINEOUT filename

    DO WHILE ready_queue \= ''
        PARSE VAR ready_queue bot_id ready_queue
        bot_id = STRIP(bot_id)
        IF bot_id = '' THEN ITERATE

        c1 = bots.bot_id.chips.1
        c2 = bots.bot_id.chips.2

        IF c1 < c2 THEN DO; low = c1; high = c2; END
        ELSE DO; low = c2; high = c1; END

        IF low = 17 & high = 61 THEN DO
            PARSE VAR bot_id 'bot.' answer
            SAY answer
            EXIT
        END

        low_dest = bots.bot_id.lowTo
        IF LEFT(low_dest, 3) = 'bot' THEN CALL give_chip low_dest, low

        high_dest = bots.bot_id.highTo
        IF LEFT(high_dest, 3) = 'bot' THEN CALL give_chip high_dest, high
    END
RETURN

give_chip: PROCEDURE EXPOSE bots. ready_queue
    ARG id, val
    count = bots.id.chips.0 + 1
    bots.id.chips.0 = count
    bots.id.chips.count = val
    IF count = 2 THEN ready_queue = ready_queue || ' ' || id
RETURN

format_id:
    RETURN TRANSLATE(STRIP(ARG(1)), '.', ' ')
