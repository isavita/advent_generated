
/* Rexx */
CALL main

main:
  file = 'input.txt'
  line = STRIP(LINEIN(file))
  CALL STREAM file, 'C', 'CLOSE'

  positions.0 = 0
  total_sum = 0
  line = CHANGESTR(',', line, ' ')

  DO i = 1 TO WORDS(line)
    num = WORD(line, i)
    n = positions.0 + 1
    positions.n = num
    positions.0 = n
    total_sum = total_sum + num
  END

  avg = total_sum / positions.0
  pos1 = avg % 1
  pos2 = pos1 + 1

  fuel1 = calculate_total_fuel(pos1)
  fuel2 = calculate_total_fuel(pos2)

  SAY MIN(fuel1, fuel2)
  RETURN

calculate_total_fuel: PROCEDURE EXPOSE positions.
  ARG target
  fuel = 0
  DO i = 1 TO positions.0
    dist = ABS(positions.i - target)
    fuel = fuel + (dist * (dist + 1)) % 2
  END
  RETURN fuel
