
/* Rexx */
Call main
Exit

main:
  width = 101
  height = 103
  steps = 100
  center_x = 50
  center_y = 51
  q1 = 0
  q2 = 0
  q3 = 0
  q4 = 0
  fileName = 'input.txt'

  Do While lines(fileName) > 0
    line = linein(fileName)
    Parse Var line 'p=' px ',' py ' v=' vx ',' vy

    x = (px + steps * vx) // width
    If x < 0 Then x = x + width

    y = (py + steps * vy) // height
    If y < 0 Then y = y + height

    If x = center_x | y = center_y Then Iterate

    If x < center_x & y < center_y Then
      q1 = q1 + 1
    Else If x > center_x & y < center_y Then
      q2 = q2 + 1
    Else If x < center_x & y > center_y Then
      q3 = q3 + 1
    Else If x > center_x & y > center_y Then
      q4 = q4 + 1
  End
  Call stream fileName, 'C', 'CLOSE'

  Say q1 * q2 * q3 * q4
Return
