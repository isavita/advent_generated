
/* REXX */
main:
  inputFile = 'input.txt'
  totalCount = 0
  totalSum = 0
  lines.0 = 0

  DO WHILE LINES(inputFile) > 0
    line = STRIP(LINEIN(inputFile))
    IF line = '' THEN DO
      IF lines.0 > 0 THEN CALL processMachine
    END
    ELSE DO
      c = lines.0 + 1
      lines.c = line
      lines.0 = c
    END
  END
  IF lines.0 > 0 THEN CALL processMachine

  IF totalCount = 0 THEN SAY '0 0'
  ELSE SAY totalCount totalSum
EXIT

processMachine:
  PROCEDURE EXPOSE totalCount totalSum lines.
  m. = 0
  DO i = 1 TO lines.0
    l = lines.i
    l = CHANGESTR("Button A:", l, "A:")
    l = CHANGESTR("Button B:", l, "B:")
    l = CHANGESTR("Prize:", l, "P:")
    PARSE VAR l prefix':' coords
    prefix = STRIP(prefix)
    PARSE VAR coords . 'X' x_str ',' 'Y' y_str
    x = parseVal(x_str)
    y = parseVal(y_str)
    SELECT
      WHEN prefix = 'A' THEN DO; m.ax = x; m.ay = y; END
      WHEN prefix = 'B' THEN DO; m.bx = x; m.by = y; END
      WHEN prefix = 'P' THEN DO; m.px = x; m.py = y; END
      OTHERWISE
    END
  END

  cost = solveMachine(m.ax, m.ay, m.bx, m.by, m.px, m.py)
  IF cost >= 0 THEN DO
    totalCount = totalCount + 1
    totalSum = totalSum + cost
  END
  DROP lines.
  lines.0 = 0
RETURN

parseVal:
  PROCEDURE
  RETURN STRIP(TRANSLATE(ARG(1), '', '+=XY '))

solveMachine:
  PROCEDURE
  ARG ax, ay, bx, by, px, py
  minCost = -1
  DO aCount = 0 TO 100
    DO bCount = 0 TO 100
      x = ax * aCount + bx * bCount
      y = ay * aCount + by * bCount
      IF x = px & y = py THEN DO
        cost = aCount * 3 + bCount
        IF minCost < 0 | cost < minCost THEN minCost = cost
      END
    END
  END
RETURN minCost
