
/* REXX */
CALL main

main:
  total_sum = 0
  filename = "input.txt"

  DO WHILE LINES(filename) > 0
    line = LINEIN(filename)
    PARSE VAR line 'Game' game_id ':' rounds
    game_id = STRIP(game_id)
    is_valid = 1

    DO WHILE rounds <> '' & is_valid
      PARSE VAR rounds round ';' rounds
      round = STRIP(round)

      DO WHILE round <> '' & is_valid
        PARSE VAR round count color ',' round
        count = STRIP(count)
        color = STRIP(color)

        SELECT
          WHEN color = 'red'   & count > 12 THEN is_valid = 0
          WHEN color = 'green' & count > 13 THEN is_valid = 0
          WHEN color = 'blue'  & count > 14 THEN is_valid = 0
          OTHERWISE NOP
        END
      END
    END

    IF is_valid THEN
      total_sum = total_sum + game_id
  END

  SAY total_sum
RETURN
