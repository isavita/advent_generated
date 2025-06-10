
main:
  lights. = 0
  count = 0
  file = 'input.txt'

  DO WHILE LINES(file) > 0
    line = LINEIN(file)
    w1 = WORD(line, 1)

    IF w1 = 'toggle' THEN DO
      action = 'toggle'
      x1y1 = WORD(line, 2)
      x2y2 = WORD(line, 4)
    END
    ELSE DO
      action = WORD(line, 2)
      x1y1 = WORD(line, 3)
      x2y2 = WORD(line, 5)
    END

    PARSE VAR x1y1 x1 ',' y1
    PARSE VAR x2y2 x2 ',' y2

    DO i = x1 TO x2
      DO j = y1 TO y2
        old = lights.i.j
        SELECT
          WHEN action = 'on'    THEN new = 1
          WHEN action = 'off'   THEN new = 0
          WHEN action = 'toggle' THEN new = 1 - old
        END
        count = count + new - old
        lights.i.j = new
      END
    END
  END
  CALL LINEOUT file

  SAY count
EXIT
