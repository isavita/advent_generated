program Solution;

{$mode objfpc}

uses
  SysUtils;

var
  inputFile: Text;
  grid: array of string;
  x, y: Integer;
  dx, dy: Integer;
  letters: String;
  i: Integer;
  cell: Char;

begin
  Assign(inputFile, 'input.txt');
  Reset(inputFile);

  SetLength(grid, 0);
  while not EOF(inputFile) do
  begin
    SetLength(grid, Length(grid) + 1);
    Readln(inputFile, grid[High(grid)]);
  end;

  Close(inputFile);

  x := 0;
  y := 0;
  for i := 1 to Length(grid[0]) do
  begin
    if grid[0][i] = '|' then
    begin
      x := i;
      break;
    end;
  end;

  dx := 0;
  dy := 1;
  letters := '';

  repeat
    if (x < 0) or (x >= Length(grid[0])) or (y < 0) or (y >= Length(grid)) then
      break;

    cell := grid[y][x];

    if cell = ' ' then
      break;

    if (cell >= 'A') and (cell <= 'Z') then
      letters := letters + cell;

    if cell = '+' then
    begin
      if dx = 0 then
      begin
        if (x > 0) and ((grid[y][x-1] = '-') or ((grid[y][x-1] >= 'A') and (grid[y][x-1] <= 'Z'))) then
          begin
            dx := -1;
            dy := 0;
          end
        else
        begin
          dx := 1;
          dy := 0;
        end;
      end
      else
      begin
        if (y > 0) and ((grid[y-1][x] = '|') or ((grid[y-1][x] >= 'A') and (grid[y-1][x] <= 'Z'))) then
          begin
            dx := 0;
            dy := -1;
          end
        else
        begin
          dx := 0;
          dy := 1;
        end;
      end;
    end;

    x := x + dx;
    y := y + dy;
  until False;

  WriteLn(letters);
end.