
/* Rexx */
adapters.0 = 0
inputFile = 'input.txt'
DO i = 1 WHILE lines(inputFile) > 0
  PARSE VALUE linein(inputFile) WITH num .
  adapters.i = num
  adapters.0 = i
END
CALL lineout inputFile

DO i = 1 TO adapters.0 - 1
  DO j = i + 1 TO adapters.0
    IF adapters.j < adapters.i THEN DO
      temp = adapters.i
      adapters.i = adapters.j
      adapters.j = temp
    END
  END
END

diffs. = 0
diffs.3 = 1
previousJoltage = 0
DO i = 1 TO adapters.0
  diff = adapters.i - previousJoltage
  diffs.diff = diffs.diff + 1
  previousJoltage = adapters.i
END

SAY diffs.1 * diffs.3
