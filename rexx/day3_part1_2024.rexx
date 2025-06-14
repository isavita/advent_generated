
main:
  totalSum = 0
  fileName = 'input.txt'

  DO WHILE LINES(fileName) > 0
    line = LINEIN(fileName)
    p = POS('mul(', line)
    DO WHILE p > 0
      PARSE VAR line =(p) 'mul(' x ',' y ')' .
      IF DATATYPE(x, 'W') & DATATYPE(y, 'W') THEN
        totalSum = totalSum + x * y
      p = POS('mul(', line, p + 1)
    END
  END

  CALL LINEOUT fileName
  SAY totalSum
  RETURN
