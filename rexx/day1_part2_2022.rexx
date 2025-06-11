
/* REXX */
main:
  inputFile = 'input.txt'
  currentCalories = 0
  top.1 = 0
  top.2 = 0
  top.3 = 0

  DO WHILE LINES(inputFile) > 0
    line = STRIP(LINEIN(inputFile))
    IF line = '' THEN DO
      CALL checkTop currentCalories
      currentCalories = 0
    END
    ELSE
      currentCalories = currentCalories + line
  END
  CALL checkTop currentCalories

  SAY top.1 + top.2 + top.3
EXIT

checkTop:
  PARSE ARG calories
  IF calories > top.1 THEN DO
    top.3 = top.2
    top.2 = top.1
    top.1 = calories
  END
  ELSE IF calories > top.2 THEN DO
    top.3 = top.2
    top.2 = calories
  END
  ELSE IF calories > top.3 THEN
    top.3 = calories
RETURN
