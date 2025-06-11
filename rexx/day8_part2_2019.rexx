
main:
  width = 25
  height = 6
  layerSize = width * height
  imageData = STRIP(CHARIN('input.txt', 1, STREAM('input.txt', 'C', 'QUERY SIZE')))
  CALL STREAM 'input.txt', 'C', 'CLOSE'

  finalImage = ''
  DO p = 1 TO layerSize
    DO i = p TO LENGTH(imageData) BY layerSize
      pixel = SUBSTR(imageData, i, 1)
      IF pixel \== '2' THEN DO
        finalImage = finalImage || pixel
        LEAVE
      END
    END
  END

  SAY "Decoded image:"
  DO y = 0 TO height - 1
    offset = y * width + 1
    row = SUBSTR(finalImage, offset, width)
    displayRow = TRANSLATE(row, ' #', '01')
    SAY displayRow
  END
EXIT
