
/* REXX */
main:
  x = 0
  y = 0
  facing = 0

  dx.0 = 1;    dy.0 = 0
  dx.90 = 0;   dy.90 = -1
  dx.180 = -1; dy.180 = 0
  dx.270 = 0;  dy.270 = 1

  inputFile = 'input.txt'
  DO WHILE LINES(inputFile) > 0
    line = LINEIN(inputFile)
    action = SUBSTR(line, 1, 1)
    value = SUBSTR(line, 2)

    SELECT
      WHEN action = 'N' THEN y = y + value
      WHEN action = 'S' THEN y = y - value
      WHEN action = 'E' THEN x = x + value
      WHEN action = 'W' THEN x = x - value
      WHEN action = 'L' THEN facing = ((facing - value) // 360 + 360) // 360
      WHEN action = 'R' THEN facing = (facing + value) // 360
      WHEN action = 'F' THEN DO
        x = x + dx.facing * value
        y = y + dy.facing * value
      END
      OTHERWISE NOP
    END
  END
  CALL LINEIN inputFile, 1, 0

  SAY ABS(x) + ABS(y)
EXIT
