
/* REXX */
main:
  count = 0
  fileName = "input.txt"
  DO WHILE LINES(fileName) > 0
    ip = LINEIN(fileName)
    IF SupportsTLS(ip) THEN
      count = count + 1
  END
  CALL LINEOUT fileName
  SAY count
EXIT

SupportsTLS:
  PROCEDURE
  ARG ip
  abba_inside = 0
  abba_outside = 0
  parts = TRANSLATE(ip, ' ', '[]')
  DO i = 1 TO WORDS(parts)
    part = WORD(parts, i)
    IF HasABBA(part) THEN DO
      IF i // 2 = 0 THEN
        abba_inside = 1
      ELSE
        abba_outside = 1
    END
  END
  RETURN abba_outside & \abba_inside

HasABBA:
  PROCEDURE
  ARG s
  DO i = 1 TO LENGTH(s) - 3
    c1 = SUBSTR(s, i, 1)
    c2 = SUBSTR(s, i + 1, 1)
    c3 = SUBSTR(s, i + 2, 1)
    c4 = SUBSTR(s, i + 3, 1)
    IF c1 \= c2 & c1 = c4 & c2 = c3 THEN
      RETURN 1
  END
  RETURN 0
