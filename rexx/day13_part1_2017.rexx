
/* REXX */
main:
  severity = 0
  fileName = 'input.txt'

  DO WHILE LINES(fileName) > 0
    line = LINEIN(fileName)
    PARSE VAR line depth ':' range
    depth = STRIP(depth)
    range = STRIP(range)

    isCaught = 0
    IF range = 1 THEN
      isCaught = 1
    ELSE DO
      cycle = (range - 1) * 2
      IF depth // cycle = 0 THEN
        isCaught = 1
    END

    IF isCaught THEN
      severity = severity + (depth * range)
  END

  CALL LINEOUT fileName
  SAY severity
EXIT
