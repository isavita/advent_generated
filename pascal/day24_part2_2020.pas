
program LobbyLayout;

{$mode objfpc}{$H+}

const
  MAX = 1000;      { Our 2D array is 0..MAX in each dimension }
  OFFSET = 500;    { Center offset so coordinates -500..+500 map into 0..1000 }

type
  TBoolGrid = array[0..MAX,0..MAX] of Boolean;

var
  grid, newGrid: TBoolGrid;
  part1Count, part2Count: Integer;
  minQ, maxQ, minR, maxR: Integer;

procedure FlipInitialTiles;
var
  s: string;
  i, len, dq, dr, x, y: Integer;
begin
  part1Count := 0;
  { start with empty grid and extents at 0,0 }
  FillChar(grid, SizeOf(grid), 0);
  minQ := 0; maxQ := 0; minR := 0; maxR := 0;

  while not EOF do
  begin
    ReadLn(s);
    dq := 0; dr := 0;
    i := 1; len := Length(s);
    while i <= len do
    begin
      case s[i] of
        'e': begin dq += 1;           dr += 0;           Inc(i); end;
        'w': begin dq -= 1;           dr += 0;           Inc(i); end;
        'n': begin
               if s[i+1]='e' then begin dq += 1; dr -= 1; end
               else                  begin dq += 0; dr -= 1; end;
               Inc(i,2);
             end;
        's': begin
               if s[i+1]='e' then begin dq += 0; dr += 1; end
               else                  begin dq -= 1; dr += 1; end;
               Inc(i,2);
             end;
      end;
    end;
    x := dq + OFFSET;
    y := dr + OFFSET;
    { flip tile }
    grid[x,y] := not grid[x,y];
    if grid[x,y] then
    begin
      Inc(part1Count);
      { expand extents to cover this black tile }
      if dq < minQ then minQ := dq;
      if dq > maxQ then maxQ := dq;
      if dr < minR then minR := dr;
      if dr > maxR then maxR := dr;
    end
    else
      Dec(part1Count);
  end;
end;

function CountNeighbors(q, r: Integer): Integer;
var
  cnt, x, y: Integer;
begin
  cnt := 0;
  { e, w, ne, nw, se, sw in our axial coords }
  x := q+1+OFFSET; y := r   +OFFSET; if grid[x,y] then Inc(cnt);
  x := q-1+OFFSET; y := r   +OFFSET; if grid[x,y] then Inc(cnt);
  x := q+1+OFFSET; y := r-1 +OFFSET; if grid[x,y] then Inc(cnt);
  x := q   +OFFSET; y := r-1 +OFFSET; if grid[x,y] then Inc(cnt);
  x := q   +OFFSET; y := r+1 +OFFSET; if grid[x,y] then Inc(cnt);
  x := q-1+OFFSET; y := r+1 +OFFSET; if grid[x,y] then Inc(cnt);
  CountNeighbors := cnt;
end;

procedure SimulateDays(days: Integer);
var
  day, q, r, n, x, y: Integer;
  nextMinQ, nextMaxQ, nextMinR, nextMaxR: Integer;
begin
  { We'll expand our region by 1 each day, since flips only affect neighbors }
  nextMinQ := minQ; nextMaxQ := maxQ;
  nextMinR := minR; nextMaxR := maxR;

  for day := 1 to days do
  begin
    nextMinQ := minQ - 1;
    nextMaxQ := maxQ + 1;
    nextMinR := minR - 1;
    nextMaxR := maxR + 1;
    FillChar(newGrid, SizeOf(newGrid), 0);

    { evaluate every tile in the expanded bounding box }
    for q := nextMinQ to nextMaxQ do
      for r := nextMinR to nextMaxR do
      begin
        n := CountNeighbors(q, r);
        x := q + OFFSET;
        y := r + OFFSET;
        if grid[x,y] then
        begin
          { black tile }
          newGrid[x,y] := not (n = 0) and (n <= 2);
        end
        else
        begin
          { white tile }
          newGrid[x,y] := (n = 2);
        end;
      end;

    { swap in newGrid → grid and update extents to the actual black tile bounds }
    minQ := MaxInt; maxQ := -MaxInt;
    minR := MaxInt; maxR := -MaxInt;
    for q := nextMinQ to nextMaxQ do
      for r := nextMinR to nextMaxR do
      begin
        x := q + OFFSET;
        y := r + OFFSET;
        grid[x,y] := newGrid[x,y];
        if grid[x,y] then
        begin
          if q < minQ then minQ := q;
          if q > maxQ then maxQ := q;
          if r < minR then minR := r;
          if r > maxR then maxR := r;
        end;
      end;
  end;

  { final count after all days }
  part2Count := 0;
  for q := minQ to maxQ do
    for r := minR to maxR do
      if grid[q+OFFSET, r+OFFSET] then
        Inc(part2Count);
end;

begin
  { redirect standard input to input.txt }
  AssignFile(Input, 'input.txt');
  Reset(Input);

  FlipInitialTiles;
  WriteLn('Part 1: ', part1Count);

  SimulateDays(100);
  WriteLn('Part 2: ', part2Count);
end.
