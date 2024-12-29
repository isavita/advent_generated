
program solve;

{$O+,Q+}
type
  TPoint = record
    x, y: Integer;
  end;

var
  grid: array of array of Char;
  h, w, x, y, dirX, dirY, dirIdx, i, j, nx, ny, count: Integer;
  dirs: array[0..3, 0..1] of Integer = ((0, -1), (1, 0), (0, 1), (-1, 0));
  visited: array of array of Boolean;
  found: Boolean;
  f: Text;

begin
  Assign(f, 'input.txt');
  Reset(f);
  h := 0;
  SetLength(grid, 1);
  while not Eof(f) do
  begin
    SetLength(grid, h + 1);
    SetLength(grid[h], 0);
    while not Eoln(f) do
    begin
      SetLength(grid[h], Length(grid[h]) + 1);
      Read(f, grid[h][Length(grid[h]) - 1]);
    end;
    ReadLn(f);
    Inc(h);
  end;
  Close(f);

  w := Length(grid[0]);
  found := False;
  for i := 0 to h - 1 do
    for j := 0 to w - 1 do
      case grid[i][j] of
        '^': begin x := j; y := i; dirIdx := 0; found := True; end;
        '>': begin x := j; y := i; dirIdx := 1; found := True; end;
        'v': begin x := j; y := i; dirIdx := 2; found := True; end;
        '<': begin x := j; y := i; dirIdx := 3; found := True; end;
      end;
  if not found then
    Halt;

  SetLength(visited, h);
  for i := 0 to h - 1 do
    SetLength(visited[i], w);

  dirX := dirs[dirIdx, 0];
  dirY := dirs[dirIdx, 1];
  visited[y][x] := True;
  count := 1;

  while True do
  begin
    nx := x + dirX;
    ny := y + dirY;
    if (nx < 0) or (nx >= w) or (ny < 0) or (ny >= h) then
      Break;
    if grid[ny][nx] = '#' then
    begin
      dirIdx := (dirIdx + 1) mod 4;
      dirX := dirs[dirIdx, 0];
      dirY := dirs[dirIdx, 1];
      Continue;
    end;
    x := nx;
    y := ny;
    if not visited[y][x] then
    begin
      visited[y][x] := True;
      Inc(count);
    end;
  end;

  WriteLn(count);
end.
