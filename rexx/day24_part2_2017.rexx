
/* REXX */
main:
  maxLength = 0
  maxStrength = 0
  comp.0 = 0
  fileName = 'input.txt'

  DO i = 1 WHILE LINES(fileName) > 0
    line = LINEIN(fileName)
    PARSE VAR line p1 '/' p2
    c = comp.0 + 1
    comp.c.1 = p1
    comp.c.2 = p2
    comp.c.used = 0
    comp.0 = c
  END
  CALL STREAM fileName, 'c', 'CLOSE'

  CALL buildBridge 0, 0, 0

  SAY maxStrength
EXIT

buildBridge:
  PROCEDURE EXPOSE comp. maxLength maxStrength
  ARG port, length, strength

  IF length > maxLength | (length = maxLength & strength > maxStrength) THEN DO
    maxLength = length
    maxStrength = strength
  END

  DO i = 1 TO comp.0
    IF comp.i.used = 0 THEN DO
      nextPort = -1
      IF comp.i.1 = port THEN nextPort = comp.i.2
      ELSE IF comp.i.2 = port THEN nextPort = comp.i.1

      IF nextPort \= -1 THEN DO
        comp.i.used = 1
        CALL buildBridge nextPort, length + 1, strength + comp.i.1 + comp.i.2
        comp.i.used = 0
      END
    END
  END
RETURN
