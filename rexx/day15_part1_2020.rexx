
/* Rexx */
main:
  line = CHANGESTR(',', LINEIN('input.txt'), ' ')
  last_spoken. = 0
  num_words = WORDS(line)

  DO i = 1 TO num_words - 1
    number = WORD(line, i)
    last_spoken.number = i
  END

  last_number = WORD(line, num_words)

  DO turn = num_words + 1 TO 2020
    prev_turn = last_spoken.last_number
    last_spoken.last_number = turn - 1
    IF prev_turn = 0 THEN
      last_number = 0
    ELSE
      last_number = (turn - 1) - prev_turn
  END

  SAY last_number
EXIT
