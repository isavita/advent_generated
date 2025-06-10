
main:
  nice_count = 0
  file = 'input.txt'
  DO WHILE LINES(file) > 0
    string = LINEIN(file)
    pair_found = 0
    DO i = 1 TO LENGTH(string) - 1
      IF POS(SUBSTR(string, i, 2), string, i + 2) > 0 THEN DO
        pair_found = 1
        LEAVE
      END
    END

    IF pair_found THEN DO
      sandwich_found = 0
      DO j = 1 TO LENGTH(string) - 2
        IF SUBSTR(string, j, 1) = SUBSTR(string, j + 2, 1) THEN DO
          sandwich_found = 1
          LEAVE
        END
      END
      IF sandwich_found THEN nice_count = nice_count + 1
    END
  END

  SAY nice_count
  RETURN
