
program LongWalk;
{$APPTYPE CONSOLE}
const
  MAXR = 1000;
  MAXC = 1000;
type
  TGrid = array[1..MAXR,1..MAXC] of char;
  TVisited = array[1..MAXR,1..MAXC] of boolean;

var
  grid: TGrid;
  visited: TVisited;
  nrows, ncols: Integer;
  startR, startC, endR, endC: Integer;
  totalOpen, maxLen: Integer;
  dR: array[0..3] of Integer = (-1, 0, 1, 0);
  dC: array[0..3] of Integer = (0, 1, 0, -1);
  arrowDir: array[char] of Integer;

procedure ReadInput;
var
  f: Text;
  line: string;
  r: Integer;
begin
  Assign(f, 'input.txt');
  Reset(f);
  nrows := 0;
  while not Eof(f) do
  begin
    ReadLn(f, line);
    Inc(nrows);
    if nrows = 1 then
      ncols := Length(line);
    // copy into grid, 1-based
    Move(line[1], grid[nrows,1], ncols);
  end;
  Close(f);
  // locate start on row 1 and end on row nrows
  for r := 1 to ncols do
    if grid[1,r] = '.' then
    begin
      startR := 1; startC := r;
      Break;
    end;
  for r := 1 to ncols do
    if grid[nrows,r] = '.' then
    begin
      endR := nrows; endC := r;
      Break;
    end;
end;

procedure InitArrow;
begin
  // default no arrow
  arrowDir['.'] := -1;
  arrowDir['#'] := -1;
  arrowDir['^'] := 0;
  arrowDir['>'] := 1;
  arrowDir['v'] := 2;
  arrowDir['<'] := 3;
end;

// DFS from (r,c), having taken stepCount steps so far, with an optional forced dir
procedure DFS(r, c, reqDir, stepCount: Integer);
var
  dir, nr, nc, needDir, remain: Integer;
begin
  // mark visited
  visited[r,c] := True;
  // if we reached the goal, update and back out
  if (r = endR) and (c = endC) then
  begin
    if stepCount > maxLen then
      maxLen := stepCount;
    visited[r,c] := False;
    Exit;
  end;
  // simple prune: you cannot exceed totalOpen cells
  // visitedCount = stepCount+1
  remain := totalOpen - (stepCount + 1);
  if stepCount + remain <= maxLen then
  begin
    visited[r,c] := False;
    Exit;
  end;
  // try each direction (or only reqDir if forced)
  for dir := 0 to 3 do
  begin
    if (reqDir < 0) or (dir = reqDir) then
    begin
      nr := r + dR[dir];
      nc := c + dC[dir];
      if (nr >= 1) and (nr <= nrows) and (nc >= 1) and (nc <= ncols) then
        if (grid[nr,nc] <> '#') and not visited[nr,nc] then
        begin
          // determine the next forced dir from the arrow (or -1)
          needDir := arrowDir[grid[nr,nc]];
          DFS(nr, nc, needDir, stepCount + 1);
        end;
    end;
  end;
  visited[r,c] := False;
end;

var
  i, j: Integer;
begin
  ReadInput;
  InitArrow;
  // count all non-# cells
  totalOpen := 0;
  for i := 1 to nrows do
    for j := 1 to ncols do
      if grid[i,j] <> '#' then
        Inc(totalOpen);
  // initialize
  FillChar(visited, SizeOf(visited), 0);
  maxLen := 0;
  // start DFS: no forced direction at start, 0 steps so far
  DFS(startR, startC, -1, 0);
  // output result
  WriteLn(maxLen);
end.
