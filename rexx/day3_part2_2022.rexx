
/* Rexx */
main:
  sum = 0
  fileName = 'input.txt'
  alphabet = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'

  DO WHILE LINES(fileName) > 0
    line1 = LINEIN(fileName)
    line2 = LINEIN(fileName)
    line3 = LINEIN(fileName)

    DO i = 1 TO LENGTH(line1)
      char = SUBSTR(line1, i, 1)
      IF POS(char, line2) > 0 & POS(char, line3) > 0 THEN DO
        sum = sum + POS(char, alphabet)
        LEAVE
      END
    END
  END

  SAY sum
EXIT
