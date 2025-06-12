
/* Rexx */
main:
  CALL read_grid
  DO 100
    CALL animate
  END
  SAY count_lights()
EXIT

read_grid:
  PROCEDURE EXPOSE grid. height width
  file = 'input.txt'
  height = 0
  DO WHILE LINES(file) > 0
    height = height + 1
    grid.height = LINEIN(file)
  END
  IF height > 0 THEN width = LENGTH(grid.1)
  ELSE width = 0
  CALL STREAM file, 'c', 'CLOSE'
RETURN

animate:
  PROCEDURE EXPOSE grid. height width
  new_grid. = ''
  DO i = 1 TO height
    new_row = ''
    DO j = 1 TO width
      on_neighbors = 0
      DO x = i - 1 TO i + 1
        DO y = j - 1 TO j + 1
          IF x >= 1 & x <= height & y >= 1 & y <= width THEN
            IF x \= i | y \= j THEN
              IF SUBSTR(grid.x, y, 1) = '#' THEN
                on_neighbors = on_neighbors + 1
        END
      END
      current_state = SUBSTR(grid.i, j, 1)
      new_state = current_state
      IF current_state = '#' THEN DO
        IF on_neighbors \= 2 & on_neighbors \= 3 THEN new_state = '.'
      END
      ELSE IF on_neighbors = 3 THEN new_state = '#'
      new_row = new_row || new_state
    END
    new_grid.i = new_row
  END
  DO i = 1 TO height
    grid.i = new_grid.i
  END
RETURN

count_lights:
  PROCEDURE EXPOSE grid. height
  count = 0
  DO i = 1 TO height
    count = count + COUNTSTR('#', grid.i)
  END
RETURN count
