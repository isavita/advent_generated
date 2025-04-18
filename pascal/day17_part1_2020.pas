program ConwayCubes3D;

const
  CYCLES = 6;
  MARGIN = 7;  { allow expansion of up to 6 in each direction }

type
  TBool3D = array of array of array of Boolean;

var
  inputFile: Text;
  lines: array[1..100] of string;
  numRows, numCols: Integer;
  maxX, maxY, maxZ: Integer;
  cur, nxt: TBool3D;
  i, j, k, cycle, dx, dy, dz, x, y, z, cnt, activeCount: Integer;
  s: string;

procedure InitArrays;
begin
  { Dimensions: initial 2D slice size = numRows x numCols, depth = 1 }
  maxX := numRows + 2*MARGIN;
  maxY := numCols + 2*MARGIN;
  maxZ := 1 + 2*MARGIN;
  SetLength(cur, maxX, maxY, maxZ);
  SetLength(nxt, maxX, maxY, maxZ);
  { initialize all to false }
  for i := 0 to maxX-1 do
    for j := 0 to maxY-1 do
      for k := 0 to maxZ-1 do begin
        cur[i][j][k] := False;
        nxt[i][j][k] := False;
      end;
  { load initial slice at z = MARGIN }
  for i := 1 to numRows do
    for j := 1 to numCols do
      if lines[i][j] = '#' then
        cur[i-1 + MARGIN][j-1 + MARGIN][MARGIN] := True;
end;

function CountActiveNeighbors(ix, iy, iz: Integer): Integer;
begin
  CountActiveNeighbors := 0;
  for dx := -1 to 1 do
    for dy := -1 to 1 do
      for dz := -1 to 1 do
        if not ((dx = 0) and (dy = 0) and (dz = 0)) then
        begin
          x := ix + dx;
          y := iy + dy;
          z := iz + dz;
          if (x >= 0) and (x < maxX)
            and (y >= 0) and (y < maxY)
            and (z >= 0) and (z < maxZ) then
            if cur[x][y][z] then
              Inc(CountActiveNeighbors);
        end;
end;

begin
  { read input.txt }
  Assign(inputFile, 'input.txt');
  Reset(inputFile);
  numRows := 0;
  while not Eof(inputFile) do
  begin
    ReadLn(inputFile, s);
    if s <> '' then
    begin
      Inc(numRows);
      lines[numRows] := s;
    end;
  end;
  Close(inputFile);
  if numRows = 0 then
    Halt(0);
  numCols := Length(lines[1]);

  { initialize 3D arrays }
  InitArrays;

  { simulate CYCLES cycles }
  for cycle := 1 to CYCLES do
  begin
    { compute next state }
    for i := 0 to maxX-1 do
      for j := 0 to maxY-1 do
        for k := 0 to maxZ-1 do
        begin
          cnt := CountActiveNeighbors(i,j,k);
          if cur[i][j][k] then
          begin
            { active stays active if 2 or 3 neighbors active }
            nxt[i][j][k] := (cnt = 2) or (cnt = 3);
          end
          else
          begin
            { inactive becomes active if exactly 3 neighbors active }
            nxt[i][j][k] := (cnt = 3);
          end;
        end;
    { copy nxt into cur for next iteration }
    for i := 0 to maxX-1 do
      for j := 0 to maxY-1 do
        for k := 0 to maxZ-1 do
          cur[i][j][k] := nxt[i][j][k];
  end;

  { count active cubes after last cycle }
  activeCount := 0;
  for i := 0 to maxX-1 do
    for j := 0 to maxY-1 do
      for k := 0 to maxZ-1 do
        if cur[i][j][k] then
          Inc(activeCount);

  { output result }
  WriteLn(activeCount);
end.