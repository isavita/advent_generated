
main:
  ship. = 0
  ship.wx = 10
  ship.wy = 1
  fileName = 'input.txt'

  CALL STREAM fileName, 'c', 'OPEN READ'
  DO WHILE LINES(fileName) > 0
    line = LINEIN(fileName)
    PARSE VAR line action 2 value

    SELECT
      WHEN action = 'N' THEN ship.wy = ship.wy + value
      WHEN action = 'S' THEN ship.wy = ship.wy - value
      WHEN action = 'E' THEN ship.wx = ship.wx + value
      WHEN action = 'W' THEN ship.wx = ship.wx - value
      WHEN action = 'L' THEN CALL rotateWaypoint -value
      WHEN action = 'R' THEN CALL rotateWaypoint value
      WHEN action = 'F' THEN DO
        ship.x = ship.x + ship.wx * value
        ship.y = ship.y + ship.wy * value
      END
    END
  END
  CALL STREAM fileName, 'c', 'CLOSE'

  manhattanDistance = ABS(ship.x) + ABS(ship.y)
  SAY manhattanDistance
EXIT

rotateWaypoint:
  ARG degrees
  degrees = (degrees + 36000) // 360

  SELECT
    WHEN degrees = 90 THEN DO
      temp_wx = ship.wx
      ship.wx = ship.wy
      ship.wy = -temp_wx
    END
    WHEN degrees = 180 THEN DO
      ship.wx = -ship.wx
      ship.wy = -ship.wy
    END
    WHEN degrees = 270 THEN DO
      temp_wx = ship.wx
      ship.wx = -ship.wy
      ship.wy = temp_wx
    END
  END
RETURN
