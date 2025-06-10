
main:
  data = CHARIN('input.txt', 1, CHARS('input.txt'))
  floor = 0
  DO position = 1 TO LENGTH(data)
    char = SUBSTR(data, position, 1)
    IF char = '(' THEN
      floor = floor + 1
    ELSE IF char = ')' THEN
      floor = floor - 1

    IF floor = -1 THEN
      LEAVE
  END
  SAY position
EXIT
