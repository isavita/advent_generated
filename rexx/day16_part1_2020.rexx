
/* REXX */
main:
  filename = 'input.txt'
  errorRate = 0
  range.0 = 0
  scanningRules = 1

  DO WHILE LINES(filename) > 0
    line = STRIP(LINEIN(filename))

    IF line = '' THEN ITERATE

    IF POS('your ticket:', line) = 1 | POS('nearby tickets:', line) = 1 THEN DO
      scanningRules = 0
      ITERATE
    END

    IF scanningRules THEN
      CALL parseRule line
    ELSE
      CALL processTicketLine line
  END

  SAY errorRate
EXIT

parseRule:
  PARSE ARG line . ':' r1 'or' r2
  PARSE VAR r1 min1 '-' max1
  PARSE VAR r2 min2 '-' max2
  rc = range.0 + 1
  range.rc.min = STRIP(min1); range.rc.max = STRIP(max1)
  range.0 = rc
  rc = range.0 + 1
  range.rc.min = STRIP(min2); range.rc.max = STRIP(max2)
  range.0 = rc
RETURN

processTicketLine:
  PARSE ARG line
  DO WHILE line <> ''
    PARSE VAR line value ',' line
    value = value + 0
    IF \isValidForAnyRule(value) THEN
      errorRate = errorRate + value
  END
RETURN

isValidForAnyRule:
  USE ARG value
  DO i = 1 TO range.0
    IF value >= range.i.min & value <= range.i.max THEN
      RETURN 1
  END
  RETURN 0
