
/* REXX */
main:
  parse value linein('input.txt') with 'target area: x=' xMin '..' xMax ', y=' yMin '..' yMax

  count = 0
  do xVel = 1 to xMax
    do yVel = yMin to -yMin - 1
      xPos = 0
      yPos = 0
      curXVel = xVel
      curYVel = yVel
      do forever
        xPos = xPos + curXVel
        yPos = yPos + curYVel

        if xPos >= xMin & xPos <= xMax & yPos >= yMin & yPos <= yMax then do
          count = count + 1
          leave
        end

        if curXVel > 0 then
          curXVel = curXVel - 1

        curYVel = curYVel - 1

        if xPos > xMax | yPos < yMin | (curXVel = 0 & xPos < xMin) then
          leave
      end
    end
  end

  say count
return
