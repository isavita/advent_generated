program Solution;

{$mode objfpc}{$h+}

uses
  SysUtils;

var
  f: Text;
  mapLines: array of string;
  grid: array of string;
  rowMin, rowMax: array of Integer;
  colMin, colMax: array of Integer;
  pathLine, line: string;
  numRows, maxWidth: Integer;
  currentX, currentY: Integer;
  facing: Integer;
  ch: Char;
  i, j, steps, k: Integer;
  nextX, nextY: Integer;
  nextTile: Char;
  password: Int64;
  idx: Integer;
  lineLen: Integer;

const
  dx: array[0..3] of Integer = (1, 0, -1, 0);
  dy: array[0..3] of Integer = (0, 1, 0, -1);

begin
  Assign(f, 'input.txt');
  Reset(f);
  SetLength(mapLines, 0);
  line := '';
  while not Eof(f) do
  begin
    ReadLn(f, line);
    if line = '' then Break;
    SetLength(mapLines, Length(mapLines) + 1);
    mapLines[Length(mapLines) - 1] := line;
  end;
  if not Eof(f) then
    ReadLn(f, pathLine)
  else
    pathLine := '';
  Close(f);

  numRows := Length(mapLines);
  if numRows = 0 then begin
    Writeln(0);
    Halt(0);
  end;

  maxWidth := 0;
  for i := 0 to numRows - 1 do
    if Length(mapLines[i]) > maxWidth then
      maxWidth := Length(mapLines[i]);

  SetLength(grid, numRows);
  for i := 0 to numRows - 1 do
  begin
    grid[i] := mapLines[i];
    while Length(grid[i]) < maxWidth do
      grid[i] := grid[i] + ' ';
  end;

  SetLength(rowMin, numRows);
  SetLength(rowMax, numRows);
  for i := 0 to numRows - 1 do
  begin
    rowMin[i] := 0;
    rowMax[i] := 0;
    for j := 0 to maxWidth - 1 do
    begin
      if grid[i][j+1] <> ' ' then
      begin
        if rowMin[i] = 0 then rowMin[i] := j + 1;
        rowMax[i] := j + 1;
      end;
    end;
  end;

  SetLength(colMin, maxWidth);
  SetLength(colMax, maxWidth);
  for i := 0 to maxWidth - 1 do
  begin
    colMin[i] := 0;
    colMax[i] := 0;
    for j := 0 to numRows - 1 do
    begin
      if grid[j][i+1] <> ' ' then
      begin
        if colMin[i] = 0 then colMin[i] := j + 1;
        colMax[i] := j + 1;
      end;
    end;
  end;

  currentX := 0;
  currentY := 1;
  for i := 0 to maxWidth - 1 do
  begin
    if grid[0][i+1] = '.' then
    begin
      currentX := i + 1;
      Break;
    end;
  end;
  if currentX = 0 then begin
    Writeln(0);
    Halt(1);
  end;

  facing := 0; // 0 Right, 1 Down, 2 Left, 3 Up

  idx := 1;
  lineLen := Length(pathLine);
  while idx <= lineLen do
  begin
    ch := pathLine[idx];
    if (ch >= '0') and (ch <= '9') then
    begin
      steps := 0;
      while (idx <= lineLen) and (pathLine[idx] >= '0') and (pathLine[idx] <= '9') do
      begin
        steps := steps * 10 + Ord(pathLine[idx]) - Ord('0');
        Inc(idx);
      end;
      for k := 1 to steps do
      begin
        nextX := currentX + dx[facing];
        nextY := currentY + dy[facing];
        if facing = 0 then
        begin
          if nextX > rowMax[currentY - 1] then nextX := rowMin[currentY - 1];
        end
        else if facing = 2 then
        begin
          if nextX < rowMin[currentY - 1] then nextX := rowMax[currentY - 1];
        end
        else if facing = 1 then
        begin
          if nextY > colMax[currentX - 1] then nextY := colMin[currentX - 1];
        end
        else // facing = 3
        begin
          if nextY < colMin[currentX - 1] then nextY := colMax[currentX - 1];
        end;

        nextTile := grid[nextY - 1][nextX];
        if nextTile = '#' then Break;
        if nextTile = '.' then
        begin
          currentX := nextX;
          currentY := nextY;
        end;
      end;
    end
    else if ch = 'L' then
    begin
      facing := (facing + 3) mod 4;
      Inc(idx);
    end
    else if ch = 'R' then
    begin
      facing := (facing + 1) mod 4;
      Inc(idx);
    end
    else
      Inc(idx);
  end;

  password := Int64(1000) * currentY + Int64(4) * currentX + facing;
  Writeln(password);
end.