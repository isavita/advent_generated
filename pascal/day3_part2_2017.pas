program SpiralMemory;
{ Reads a single integer from input.txt, computes:
  1) Manhattan distance from that square to center in the spiral.
  2) First value in the stress-test spiral larger than the input.
  Prints the two results (one per line). }

const
  MAXGRID = 1001;          { must be odd; gives center at (MAXGRID div 2 + 1) }
type
  TInt64Grid = array[1..MAXGRID,1..MAXGRID] of Int64;

var
  N: Int64;
  distPart1: Integer;
  part2Answer: Int64;
  layer, side, ringMax, offset, md, k, tmp, best: Int64;
  grid: TInt64Grid;
  cx, cy, x, y: Integer;
  stepCount, dir, i, dloop: Integer;
  dx, dy: array[0..3] of Integer;
  sumN: Int64;
  found: Boolean;
  inF: Text;

function AbsInt(x: Int64): Int64;
begin
  if x < 0 then AbsInt := -x else AbsInt := x;
end;

begin
  { Open input file }
  Assign(inF, 'input.txt');
  Reset(inF);
  if not Eof(inF) then
    Read(inF, N);
  Close(inF);

  { Part 1: find which ring contains N }
  if N <= 1 then
    distPart1 := 0
  else
  begin
    layer := 0;
    repeat
      side := 2*layer + 1;
      ringMax := side*side;
      if ringMax >= N then Break;
      Inc(layer);
    until False;
    { Midpoints of each side in this ring are at values ringMax - layer - k*(side-1) }
    offset := ringMax - layer;
    best := MaxInt;  { large sentinel }
    for k := 0 to 3 do
    begin
      tmp := AbsInt(N - (offset - k*(side-1)));
      if tmp < best then
        best := tmp;
    end;
    distPart1 := layer + Integer(best);
  end;

  { Part 2: build the stress-test spiral }
  { zero the grid }
  for x := 1 to MAXGRID do
    for y := 1 to MAXGRID do
      grid[x,y] := 0;

  { center coordinates }
  cx := (MAXGRID div 2) + 1;
  cy := cx;
  x := cx;
  y := cy;
  grid[x,y] := 1;

  { directions: right, up, left, down }
  dx[0] := 1; dy[0] := 0;
  dx[1] := 0; dy[1] := 1;
  dx[2] := -1; dy[2] := 0;
  dx[3] := 0; dy[3] := -1;

  stepCount := 1;
  dir := 0;
  found := False;
  part2Answer := 0;

  while not found do
  begin
    { do two legs with the same stepCount }
    for dloop := 1 to 2 do
    begin
      for i := 1 to stepCount do
      begin
        Inc(x, dx[dir]);
        Inc(y, dy[dir]);
        { sum neighbors }
        sumN := 0;
        sumN := sumN
          + grid[x-1, y-1] + grid[x, y-1] + grid[x+1, y-1]
          + grid[x-1, y  ]             + grid[x+1, y  ]
          + grid[x-1, y+1] + grid[x, y+1] + grid[x+1, y+1];
        grid[x,y] := sumN;
        if (not found) and (sumN > N) then
        begin
          part2Answer := sumN;
          found := True;
          Break;
        end;
      end;
      if found then Break;
      dir := (dir + 1) mod 4;
    end;
    Inc(stepCount);
  end;

  { Output results }
  WriteLn(distPart1);
  WriteLn(part2Answer);
end.