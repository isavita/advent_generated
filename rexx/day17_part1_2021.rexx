
/* Rexx */
call main
exit

main:
  parse value linein('input.txt') with . 'y=' yPart
  parse var yPart yMin '..' .
  yVel = -yMin - 1
  maxY = (yVel * (yVel + 1)) % 2
  say maxY
  return
