
program XMAS;

{$O+,R-,Q-}

type
  TGrid = array of string;

function checkMAS(const grid: TGrid; x, y, dx, dy: Integer): Boolean;
var
  i: Integer;
  newX, newY: Integer;
  word: string;
  forward, backward: Boolean;
begin
  if (x < 0) or (y < 0) or (x >= Length(grid)) or (y >= Length(grid[0])) then
  begin
    checkMAS := False;
    exit;
  end;

  word := 'MAS';
  forward := True;
  backward := True;

  for i := 0 to Length(word) - 1 do
  begin
    newX := x + (dx * i);
    newY := y + (dy * i);
    if (newX < 0) or (newY < 0) or (newX >= Length(grid)) or (newY >= Length(grid[0])) then
    begin
      forward := False;
      break;
    end;
    if grid[newX][newY+1] <> word[i+1] then
      forward := False;
  end;

  for i := 0 to Length(word) - 1 do
  begin
    newX := x + (dx * i);
    newY := y + (dy * i);
    if (newX < 0) or (newY < 0) or (newX >= Length(grid)) or (newY >= Length(grid[0])) then
    begin
      backward := False;
      break;
    end;
    if grid[newX][newY+1] <> word[Length(word) - i] then
      backward := False;
  end;

  checkMAS := forward or backward;
end;

function checkXMAS(const grid: TGrid; x, y: Integer): Boolean;
begin
  checkXMAS := (checkMAS(grid, x - 1, y - 1, 1, 1) and checkMAS(grid, x - 1, y + 1, 1, -1)) or
               (checkMAS(grid, x + 1, y - 1, -1, 1) and checkMAS(grid, x + 1, y + 1, -1, -1));
end;

function countXMASPatterns(const grid: TGrid): Integer;
var
  count, i, j: Integer;
begin
  count := 0;
  if (Length(grid) < 3) or (Length(grid[0]) < 3) then
  begin
    countXMASPatterns := 0;
    exit;
  end;

  for i := 1 to Length(grid) - 2 do
    for j := 1 to Length(grid[0]) - 2 do
      if grid[i][j+1] = 'A' then
        if checkXMAS(grid, i, j) then
          Inc(count);

  countXMASPatterns := count;
end;

var
  grid: TGrid;
  line: string;
  f: TextFile;
  count: Integer;
begin
  Assign(f, 'input.txt');
  Reset(f);
  SetLength(grid, 0);
  while not Eof(f) do
  begin
    ReadLn(f, line);
    if Length(line) > 0 then
      SetLength(grid, Length(grid) + 1);
      grid[Length(grid) - 1] := line;
  end;
  Close(f);

  count := countXMASPatterns(grid);
  WriteLn('X-MAS patterns appear ', count, ' times in the word search');
end.
