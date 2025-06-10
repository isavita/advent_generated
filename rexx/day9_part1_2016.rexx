
main:
  inputFile = 'input.txt'
  input_text = LINEIN(inputFile)
  CALL STREAM inputFile, 'C', 'CLOSE'

  length = 0
  i = 1
  str_len = LENGTH(input_text)

  DO FOREVER
    IF i > str_len THEN LEAVE

    start_marker = POS('(', input_text, i)

    IF start_marker = 0 THEN DO
      length = length + (str_len - i + 1)
      LEAVE
    END

    length = length + (start_marker - i)

    end_marker = POS(')', input_text, start_marker)
    marker_content = SUBSTR(input_text, start_marker + 1, end_marker - start_marker - 1)
    PARSE VAR marker_content char_count 'x' repeat_count

    length = length + (char_count * repeat_count)
    i = end_marker + char_count + 1
  END

  SAY length
EXIT
