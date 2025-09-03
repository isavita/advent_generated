program FourDConway;

const
  MAX_SIZE = 30;
  CYCLES = 6;

type
  Grid4D = array[0..MAX_SIZE-1] of array[0..MAX_SIZE-1] of array[0..MAX_SIZE-1] of array[0..MAX_SIZE-1] of Byte;

var
  grid, newGrid: Grid4D;
  inputFile: Text;
  mid, offset: Integer;
  i, j, k, l, cycle, val: Integer;
  ch: Char;
  total: Integer;

function CountActive(x, y, z, w: Integer): Integer;
var a, b, c, d, cnt: Integer;
begin
  cnt := 0;
  for a := x - 1 to x + 1 do
    for b := y - 1 to y + 1 do
      for c := z - 1 to z + 1 do
        for d := w - 1 to w + 1 do
          if not ((a = x) and (b = y) and (c = z) and (d = w)) then
            cnt := cnt + grid[a][b][c][d];
  CountActive := cnt;
end;

function NextChar: Char;
var tmp: Char;
begin
  repeat
    Read(inputFile, tmp);
  until not (tmp in [#10, #13, #9, ' ']);
  NextChar := tmp;
end;

begin
  Assign(inputFile, 'input.txt');
  Reset(inputFile);

  for i := 0 to MAX_SIZE - 1 do
    for j := 0 to MAX_SIZE - 1 do
      for k := 0 to MAX_SIZE - 1 do
        for l := 0 to MAX_SIZE - 1 do
          grid[i][j][k][l] := 0;

  mid := MAX_SIZE div 2;
  offset := mid - 3;

  for i := offset to offset + 7 do
    for j := offset to offset + 7 do
    begin
      ch := NextChar;
      if ch = '#' then grid[mid][mid][i][j] := 1
      else grid[mid][mid][i][j] := 0;
    end;

  Close(inputFile);

  for cycle := 1 to CYCLES do
  begin
    for i := 1 to MAX_SIZE - 2 do
      for j := 1 to MAX_SIZE - 2 do
        for k := 1 to MAX_SIZE - 2 do
          for l := 1 to MAX_SIZE - 2 do
          begin
            val := CountActive(i, j, k, l);
            if grid[i][j][k][l] = 1 then
            begin
              if (val = 2) or (val = 3) then newGrid[i][j][k][l] := 1
              else newGrid[i][j][k][l] := 0;
            end
            else
            begin
              if val = 3 then newGrid[i][j][k][l] := 1
              else newGrid[i][j][k][l] := 0;
            end;
          end;

    for i := 1 to MAX_SIZE - 2 do
      for j := 1 to MAX_SIZE - 2 do
        for k := 1 to MAX_SIZE - 2 do
          for l := 1 to MAX_SIZE - 2 do
            grid[i][j][k][l] := newGrid[i][j][k][l];
  end;

  total := 0;
  for i := 0 to MAX_SIZE - 1 do
    for j := 0 to MAX_SIZE - 1 do
      for k := 0 to MAX_SIZE - 1 do
        for l := 0 to MAX_SIZE - 1 do
          total := total + grid[i][j][k][l];

  WriteLn(total);
end.