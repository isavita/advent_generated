
/* REXX */
main:
  y = 0
  DO WHILE LINES('input.txt') > 0
    y = y + 1
    line = LINEIN('input.txt')
    IF y = 1 THEN max_x = LENGTH(line)
    DO x = 1 TO max_x
      grid.x.y = SUBSTR(line, x, 1)
    END
  END
  max_y = y

  max_score = 0
  DO y = 1 TO max_y
    DO x = 1 TO max_x
      height = grid.x.y
      score = 1

      view = 0
      DO ny = y - 1 BY -1 TO 1
        view = view + 1
        IF grid.x.ny >= height THEN LEAVE
      END
      score = score * view

      view = 0
      DO ny = y + 1 TO max_y
        view = view + 1
        IF grid.x.ny >= height THEN LEAVE
      END
      score = score * view

      view = 0
      DO nx = x - 1 BY -1 TO 1
        view = view + 1
        IF grid.nx.y >= height THEN LEAVE
      END
      score = score * view

      view = 0
      DO nx = x + 1 TO max_x
        view = view + 1
        IF grid.nx.y >= height THEN LEAVE
      END
      score = score * view

      max_score = MAX(max_score, score)
    END
  END

  SAY max_score
EXIT
