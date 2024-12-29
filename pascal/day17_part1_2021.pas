
program solve;

{$O+,R-,Q-}

var
  xMin, xMax, yMin, yMax, maxY, xVel, yVel, xPos, yPos, curXVel, curYVel, highestY, i, j: longint;
  line: string;
  parts: array[0..1] of string;
  xRange, yRange: array[0..1] of string;
  f: text;

function isMovingAway(xPos, yPos, xVel, yVel, xMin, xMax, yMin, yMax: longint): boolean;
begin
  isMovingAway := (xPos < xMin) and (xVel < 0) or (xPos > xMax) and (xVel > 0) or (yPos < yMin) and (yVel < 0);
end;

begin
  assign(f, 'input.txt');
  reset(f);
  readln(f, line);
  close(f);

  parts[0] := copy(line, 1, pos(', ', line) - 1);
  parts[1] := copy(line, pos(', ', line) + 2, length(line));

  xRange[0] := copy(parts[0], pos('=', parts[0]) + 1, pos('..', parts[0]) - pos('=', parts[0]) - 1);
  xRange[1] := copy(parts[0], pos('..', parts[0]) + 2, length(parts[0]));
  yRange[0] := copy(parts[1], pos('=', parts[1]) + 1, pos('..', parts[1]) - pos('=', parts[1]) - 1);
  yRange[1] := copy(parts[1], pos('..', parts[1]) + 2, length(parts[1]));

  val(xRange[0], xMin, i);
  val(xRange[1], xMax, i);
  val(yRange[0], yMin, i);
  val(yRange[1], yMax, i);

  maxY := -1000000000;
  for xVel := -500 to 500 do
    for yVel := -500 to 500 do
    begin
      xPos := 0;
      yPos := 0;
      curXVel := xVel;
      curYVel := yVel;
      highestY := yPos;
      while true do
      begin
        xPos := xPos + curXVel;
        yPos := yPos + curYVel;

        if (xPos >= xMin) and (xPos <= xMax) and (yPos >= yMin) and (yPos <= yMax) then
        begin
          if highestY > maxY then
            maxY := highestY;
          break;
        end;

        if isMovingAway(xPos, yPos, curXVel, curYVel, xMin, xMax, yMin, yMax) then
          break;

        if curXVel > 0 then
          curXVel := curXVel - 1
        else if curXVel < 0 then
          curXVel := curXVel + 1;

        curYVel := curYVel - 1;
        if yPos > highestY then
          highestY := yPos;
      end;
    end;

  writeln(maxY);
end.
