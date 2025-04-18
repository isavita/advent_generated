program DumboOctopus;
{$mode objfpc}{$H+}
uses
  SysUtils;

const
  N = 10;
  MaxQueue = 1000;

type
  TPos = record
    r, c: Integer;
  end;

var
  grid: array[1..N,1..N] of Integer;
  flashed: array[1..N,1..N] of Boolean;
  q: array[1..MaxQueue] of TPos;
  head, tail: Integer;
  totalFlashes, flashCount, step, syncStep: Integer;
  f: Text;
  line: string;
  i, j: Integer;

procedure ReadInput;
begin
  Assign(f, 'input.txt');
  Reset(f);
  for i := 1 to N do
  begin
    ReadLn(f, line);
    for j := 1 to N do
      grid[i, j] := Ord(line[j]) - Ord('0');
  end;
  Close(f);
end;

procedure ProcessStep;
var
  di, dj, ni, nj: Integer;
begin
  { 1) Increase all energy by 1 and clear flash marks }
  for i := 1 to N do
    for j := 1 to N do
    begin
      Inc(grid[i, j]);
      flashed[i, j] := False;
    end;

  { 2) Enqueue any octopus ready to flash }
  head := 1;
  tail := 0;
  for i := 1 to N do
    for j := 1 to N do
      if grid[i, j] > 9 then
      begin
        Inc(tail);
        q[tail].r := i;
        q[tail].c := j;
      end;

  flashCount := 0;

  { 3) Process the queue: each flash increases neighbors }
  while head <= tail do
  begin
    with q[head] do
    begin
      if not flashed[r, c] then
      begin
        flashed[r, c] := True;
        Inc(flashCount);
        for di := -1 to 1 do
          for dj := -1 to 1 do
            if (di <> 0) or (dj <> 0) then
            begin
              ni := r + di;
              nj := c + dj;
              if (ni >= 1) and (ni <= N) and (nj >= 1) and (nj <= N) then
              begin
                Inc(grid[ni, nj]);
                if (grid[ni, nj] > 9) and (not flashed[ni, nj]) then
                begin
                  Inc(tail);
                  q[tail].r := ni;
                  q[tail].c := nj;
                end;
              end;
            end;
      end;
    end;
    Inc(head);
  end;

  { 4) Reset any octopus that flashed this step to 0 }
  for i := 1 to N do
    for j := 1 to N do
      if flashed[i, j] then
        grid[i, j] := 0;
end;

begin
  ReadInput;

  totalFlashes := 0;
  syncStep := 0;

  { Simulate until at least 100 steps have passed and we find a
    step where all octopuses flash simultaneously. }
  for step := 1 to MaxInt do
  begin
    ProcessStep;
    if step <= 100 then
      Inc(totalFlashes, flashCount);
    if flashCount = N * N then
    begin
      syncStep := step;
      Break;
    end;
  end;

  { Output Part 1 and Part 2 answers }
  writeln(totalFlashes);
  writeln(syncStep);
end.