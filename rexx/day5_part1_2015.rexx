
CALL main

main:
  filename = "input.txt"
  nice_count = 0

  rc = STREAM(filename, 'c', 'open read')
  IF rc \= "READY:" THEN DO
    SAY "Error opening file:" rc
    EXIT 1
  END

  DO WHILE LINES(filename) > 0
    s = LINEIN(filename)

    vowels = 0
    DO i = 1 TO LENGTH(s)
      IF POS(SUBSTR(s, i, 1), "aeiou") > 0 THEN
        vowels = vowels + 1
    END

    double_letter = 0
    DO i = 1 TO LENGTH(s) - 1
      IF SUBSTR(s, i, 1) = SUBSTR(s, i+1, 1) THEN DO
        double_letter = 1
        LEAVE
      END
    END

    forbidden = 0
    IF POS("ab", s) > 0 THEN forbidden = 1
    ELSE IF POS("cd", s) > 0 THEN forbidden = 1
    ELSE IF POS("pq", s) > 0 THEN forbidden = 1
    ELSE IF POS("xy", s) > 0 THEN forbidden = 1

    IF vowels >= 3 & double_letter = 1 & forbidden = 0 THEN
      nice_count = nice_count + 1
  END

  rc = STREAM(filename, 'c', 'close')

  SAY nice_count
  RETURN
