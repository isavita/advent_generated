
/* Rexx */
main:
  fileName = 'input.txt'
  molecule = ''

  DO WHILE LINES(fileName) > 0
    line = LINEIN(fileName)
    IF line = '' THEN DO
      molecule = STRIP(LINEIN(fileName))
      LEAVE
    END
  END
  CALL STREAM fileName, 'c', 'CLOSE'

  total_elements = 0
  rn_count = 0
  ar_count = 0
  y_count = 0
  pos = 1

  DO WHILE pos <= LENGTH(molecule)
    total_elements = total_elements + 1
    element_len = 1
    element = SUBSTR(molecule, pos, 1)

    IF pos < LENGTH(molecule) THEN DO
      IF DATATYPE(SUBSTR(molecule, pos + 1, 1), 'L') THEN DO
        element = SUBSTR(molecule, pos, 2)
        element_len = 2
      END
    END

    SELECT
      WHEN element = 'Rn' THEN rn_count = rn_count + 1
      WHEN element = 'Ar' THEN ar_count = ar_count + 1
      WHEN element = 'Y'  THEN y_count = y_count + 1
      OTHERWISE NOP
    END

    pos = pos + element_len
  END

  steps = total_elements - rn_count - ar_count - 2 * y_count - 1
  SAY steps

EXIT
