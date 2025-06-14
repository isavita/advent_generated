
/* REXX */
main:
  in_degree. = 0
  graph. = 0
  alphabet = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
  fname = 'input.txt'

  DO WHILE lines(fname) > 0
    line = linein(fname)
    PARSE VAR line 'Step ' pred ' must be finished before step ' succ ' can begin.'
    if graph.pred.succ = 0 then do
      graph.pred.succ = 1
      in_degree.succ = in_degree.succ + 1
    end
  END
  CALL stream fname, 'C', 'CLOSE'

  result = ''
  DO 26
    DO i = 1 TO length(alphabet)
      step = substr(alphabet, i, 1)
      if in_degree.step = 0 then do
        result = result || step
        in_degree.step = -1
        DO j = 1 TO length(alphabet)
          successor = substr(alphabet, j, 1)
          if graph.step.successor = 1 then
            in_degree.successor = in_degree.successor - 1
        END
        LEAVE i
      end
    END
  END

  SAY result
RETURN
