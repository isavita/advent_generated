
/* REXX */
CALL main
EXIT

main:
  PARSE VALUE LINEIN('input.txt') WITH . times_str
  PARSE VALUE LINEIN('input.txt') WITH . distances_str
  CALL STREAM 'input.txt', 'C', 'CLOSE'

  total_ways = 1
  DO i = 1 TO WORDS(times_str)
    time = WORD(times_str, i)
    record = WORD(distances_str, i)
    total_ways = total_ways * number_of_ways_to_win(time, record)
  END

  SAY total_ways
RETURN

number_of_ways_to_win: PROCEDURE
  ARG time, record
  ways = 0
  DO hold_time = 1 TO time - 1
    IF hold_time * (time - hold_time) > record THEN
      ways = ways + 1
  END
RETURN ways
