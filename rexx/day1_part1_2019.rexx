
/* Rexx */
CALL main

main:
  total = 0
  fileName = 'input.txt'
  DO WHILE LINES(fileName) > 0
    mass = LINEIN(fileName)
    total = total + (mass % 3 - 2)
  END
  CALL STREAM fileName, 'C', 'CLOSE'
  SAY total
RETURN
