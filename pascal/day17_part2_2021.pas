
program Day17;

{$APPTYPE CONSOLE}

uses
  SysUtils;

var
  xMin, xMax, yMin, yMax: Integer;
  xVel, yVel: Integer;
  xPos, yPos: Integer;
  curXVel, curYVel: Integer;
  inTargetArea: Boolean;
  count: Integer;
  line: string;
  parts: array[0..1] of string;
  range: array[0..1] of string;
  f: TextFile;

begin
  Assign(f, 'input.txt');
  Reset(f);
  Readln(f, line);
  Close(f);

  parts[0] := Copy(line, Pos('target area: ', line) + Length('target area: '), Pos(',', line) - Pos('target area: ', line) - Length('target area: '));
  parts[1] := Copy(line, Pos(',', line) + 2, Length(line) - Pos(',', line) - 1);

  range[0] := Copy(parts[0], Pos('x=', parts[0]) + 2, Pos('..' , parts[0]) - Pos('x=', parts[0]) - 2);
  range[1] := Copy(parts[0], Pos('..' , parts[0]) + 2, Length(parts[0]) - Pos('..' , parts[0]) - 1);

  xMin := StrToInt(range[0]);
  xMax := StrToInt(range[1]);

  range[0] := Copy(parts[1], Pos('y=', parts[1]) + 2, Pos('..' , parts[1]) - Pos('y=', parts[1]) - 2);
  range[1] := Copy(parts[1], Pos('..' , parts[1]) + 2, Length(parts[1]) - Pos('..' , parts[1]) - 1);

  yMin := StrToInt(range[0]);
  yMax := StrToInt(range[1]);
  
  count := 0;
  for xVel := -200 to xMax + 1 do
  begin
    for yVel := yMin - 1 to 200 do
    begin
      xPos := 0;
      yPos := 0;
      curXVel := xVel;
      curYVel := yVel;
      inTargetArea := False;

      while (xPos <= xMax) and (yPos >= yMin) do
      begin
        xPos := xPos + curXVel;
        yPos := yPos + curYVel;

        if (xPos >= xMin) and (xPos <= xMax) and (yPos >= yMin) and (yPos <= yMax) then
        begin
          inTargetArea := True;
          break;
        end;

        if curXVel > 0 then
          curXVel := curXVel - 1
        else if curXVel < 0 then
          curXVel := curXVel + 1;

        curYVel := curYVel - 1;
      end;

      if inTargetArea then
        Inc(count);
    end;
  end;

  Writeln(count);
end.
