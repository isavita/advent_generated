
/* REXX */
main:
  invalidNumber = 14360655
  fileName = 'input.txt'
  i = 0
  DO WHILE LINES(fileName) > 0
    i = i + 1
    numbers.i = LINEIN(fileName)
  END
  numbers.0 = i

  DO i = 1 TO numbers.0
    sum = numbers.i
    min = numbers.i
    max = numbers.i
    DO j = i + 1 TO numbers.0
      sum = sum + numbers.j
      min = MIN(min, numbers.j)
      max = MAX(max, numbers.j)

      IF sum = invalidNumber THEN DO
        SAY min + max
        EXIT
      END

      IF sum > invalidNumber THEN
        LEAVE
    END
  END
RETURN
