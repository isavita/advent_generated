
/* REXX */
CALL main
EXIT

main:
  tiles. = 0
  count = 0
  fileName = 'input.txt'

  DO WHILE LINES(fileName) > 0
    line = STRIP(LINEIN(fileName))
    q = 0
    r = 0
    i = 1
    DO WHILE i <= LENGTH(line)
      c1 = SUBSTR(line, i, 1)
      dir = c1
      i = i + 1
      IF c1 = 'n' | c1 = 's' THEN DO
        dir = dir || SUBSTR(line, i, 1)
        i = i + 1
      END

      SELECT
        WHEN dir = 'e'  THEN q = q + 1
        WHEN dir = 'se' THEN r = r + 1
        WHEN dir = 'sw' THEN DO; q = q - 1; r = r + 1; END
        WHEN dir = 'w'  THEN q = q - 1
        WHEN dir = 'nw' THEN r = r - 1
        WHEN dir = 'ne' THEN DO; q = q + 1; r = r - 1; END
        OTHERWISE NOP
      END
    END

    key = q','r
    IF tiles.key = 1 THEN
      count = count - 1
    ELSE
      count = count + 1
    tiles.key = 1 - tiles.key
  END

  CALL STREAM fileName, 'C', 'CLOSE'
  SAY count
RETURN
