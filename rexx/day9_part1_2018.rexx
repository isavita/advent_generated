
/* REXX */
main:
  CALL solve
RETURN

solve:
  line = LINEIN('input.txt')
  CALL LINEOUT 'input.txt'
  PARSE VAR line players . 'worth' lastMarble .

  scores. = 0
  next.0 = 0
  prev.0 = 0
  current = 0

  DO marble = 1 TO lastMarble
    IF marble // 23 = 0 THEN DO
      player = (marble - 1) // players
      DO 7
        current = prev.current
      END
      scores.player = scores.player + marble + current
      p = prev.current
      n = next.current
      next.p = n
      prev.n = p
      current = n
    END
    ELSE DO
      current = next.current
      p = current
      n = next.current
      prev.marble = p
      next.marble = n
      next.p = marble
      prev.n = marble
      current = marble
    END
  END

  maxScore = 0
  DO i = 0 TO players - 1
    maxScore = MAX(maxScore, scores.i)
  END
  SAY maxScore
RETURN
