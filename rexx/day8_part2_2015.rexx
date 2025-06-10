
/* Rexx */

main:
  part1 = 0
  part2 = 0
  fileName = 'input.txt'

  DO WHILE LINES(fileName) > 0
    line = LINEIN(fileName)
    codeLen = LENGTH(line)

    memLen = 0
    i = 2
    DO WHILE i < codeLen
      memLen = memLen + 1
      c = SUBSTR(line, i, 1)
      IF c = '\' THEN DO
        c2 = SUBSTR(line, i + 1, 1)
        IF c2 = 'x' THEN
          i = i + 4
        ELSE
          i = i + 2
      END
      ELSE
        i = i + 1
    END
    part1 = part1 + (codeLen - memLen)

    part2 = part2 + COUNTSTR('\', line) + COUNTSTR('"', line) + 2
  END
  CALL STREAM fileName, 'c', 'close'

  SAY part1
  SAY part2
EXIT
