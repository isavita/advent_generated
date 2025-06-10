
/* REXX */
main:
  lights. = 0
  total = 0
  filename = 'input.txt'

  DO WHILE LINES(filename) > 0
    line = LINEIN(filename)
    PARSE VAR line word1 rest
    SELECT
      WHEN word1 = 'toggle' THEN DO
        PARSE VAR rest start 'through' end
        delta = 2
      END
      WHEN word1 = 'turn' THEN DO
        PARSE VAR rest action start 'through' end
        IF action = 'on' THEN delta = 1
        ELSE delta = -1
      END
    END

    PARSE VAR start x1 ',' y1
    PARSE VAR end   x2 ',' y2

    DO x = x1 TO x2
      DO y = y1 TO y2
        old = lights.x.y
        IF delta = -1 THEN
          lights.x.y = MAX(0, old - 1)
        ELSE
          lights.x.y = old + delta
        total = total + (lights.x.y - old)
      END
    END
  END

  CALL STREAM filename, 'C', 'CLOSE'
  SAY total
EXIT

MAX: PROCEDURE
  PARSE ARG n1, n2
  IF n1 > n2 THEN RETURN n1
  RETURN n2
