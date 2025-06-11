
/* REXX */
main:
  count = 0
  fileName = 'input.txt'

  DO WHILE LINES(fileName) > 0
    line = LINEIN(fileName)
    PARSE VAR line s1 '-' e1 ',' s2 '-' e2
    IF (s1 <= s2 & e1 >= e2) | (s2 <= s1 & e2 >= e1) THEN
      count = count + 1
  END

  SAY count
RETURN
