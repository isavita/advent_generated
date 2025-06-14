
/* Rexx */
main:
  SIZE = 10
  totalFlashes = 0
  CALL readInput
  DO step = 1 TO 100
    totalFlashes = totalFlashes + simulateStep()
  END
  SAY totalFlashes
EXIT

readInput:
  PROCEDURE EXPOSE grid. SIZE
  fileName = "input.txt"
  DO y = 1 TO SIZE
    line = LINEIN(fileName)
    DO x = 1 TO SIZE
      grid.y.x = SUBSTR(line, x, 1)
    END
  END
  CALL LINEOUT fileName
RETURN

simulateStep:
  PROCEDURE EXPOSE grid. flashed. SIZE
  stepFlashes = 0
  DO y = 1 TO SIZE
    DO x = 1 TO SIZE
      grid.y.x = grid.y.x + 1
      flashed.y.x = 0
    END
  END

  DO y = 1 TO SIZE
    DO x = 1 TO SIZE
      IF grid.y.x > 9 THEN
        stepFlashes = stepFlashes + flash(x, y)
    END
  END

  DO y = 1 TO SIZE
    DO x = 1 TO SIZE
      IF flashed.y.x = 1 THEN grid.y.x = 0
    END
  END
RETURN stepFlashes

flash:
  PROCEDURE EXPOSE grid. flashed. SIZE
  ARG x, y
  IF flashed.y.x = 1 THEN RETURN 0
  flashed.y.x = 1
  count = 1
  DO dy = -1 TO 1
    DO dx = -1 TO 1
      IF dx = 0 & dy = 0 THEN ITERATE
      newX = x + dx
      newY = y + dy
      IF newX >= 1 & newX <= SIZE & newY >= 1 & newY <= SIZE THEN DO
        grid.newY.newX = grid.newY.newX + 1
        IF grid.newY.newX > 9 THEN
          count = count + flash(newX, newY)
      END
    END
  END
RETURN count
