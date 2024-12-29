
program visible_trees;

{$O+,R-,Q-}

type
  TPoint = record
    x, y: Integer;
  end;

  TGrid = array of array of Integer;

var
  grid: TGrid;
  visible: array of array of Boolean;
  width, height: Integer;

function isValid(x, y: Integer): Boolean;
begin
  isValid := (x >= 0) and (x < width) and (y >= 0) and (y < height);
end;

procedure solve;
var
  x, y, dx, dy, nx, ny: Integer;
  i: Integer;
  count: Integer;
begin
  count := 0;
  for y := 0 to height - 1 do
    for x := 0 to width - 1 do
    begin
      for i := 0 to 3 do
      begin
        case i of
          0: begin dx := 0; dy := 1; end;
          1: begin dx := 0; dy := -1; end;
          2: begin dx := 1; dy := 0; end;
          3: begin dx := -1; dy := 0; end;
        end;
        nx := x + dx;
        ny := y + dy;
        while isValid(nx, ny) do
        begin
          if grid[ny][nx] >= grid[y][x] then
            break;
          nx := nx + dx;
          ny := ny + dy;
        end;
        if not isValid(nx, ny) then
        begin
          if not visible[y][x] then
          begin
            visible[y][x] := true;
            count := count + 1;
          end;
          break;
        end;
      end;
    end;
  writeln(count);
end;

procedure readInput;
var
  f: Text;
  line: string;
  y, x: Integer;
begin
  Assign(f, 'input.txt');
  Reset(f);
  height := 0;
  while not Eof(f) do
  begin
    Readln(f, line);
    if height = 0 then
      width := Length(line);
    Inc(height);
  end;
  Close(f);

  SetLength(grid, height);
  SetLength(visible, height);
  for y := 0 to height - 1 do
  begin
    SetLength(grid[y], width);
    SetLength(visible[y], width);
  end;

  Reset(f);
  y := 0;
  while not Eof(f) do
  begin
    Readln(f, line);
    for x := 0 to width - 1 do
      grid[y][x] := Ord(line[x + 1]) - Ord('0');
    Inc(y);
  end;
  Close(f);
end;

begin
  readInput;
  solve;
end.
