
program SeatingSystem;
{$mode objfpc}{$H+}
uses
  Classes, SysUtils;

var
  lines: TStringList;
  origGrid, grid, newGrid: array of string;
  rows, cols: Integer;

// Count occupied adjacent seats (part 1)
function AdjOccupied(r, c: Integer): Integer;
var
  dr, dc, rr, cc: Integer;
begin
  Result := 0;
  for dr := -1 to 1 do
    for dc := -1 to 1 do
      if not ((dr = 0) and (dc = 0)) then
      begin
        rr := r + dr;
        cc := c + dc;
        if (rr >= 0) and (rr < rows) and (cc >= 0) and (cc < cols) then
          if grid[rr][cc+1] = '#' then
            Inc(Result);
      end;
end;

// Count occupied visible seats (part 2)
function VisOccupied(r, c: Integer): Integer;
var
  dr, dc, rr, cc: Integer;
begin
  Result := 0;
  for dr := -1 to 1 do
    for dc := -1 to 1 do
      if not ((dr = 0) and (dc = 0)) then
      begin
        rr := r + dr;
        cc := c + dc;
        // step until we hit a seat or go out of bounds
        while (rr >= 0) and (rr < rows) and (cc >= 0) and (cc < cols)
          and (grid[rr][cc+1] = '.') do
        begin
          rr := rr + dr;
          cc := cc + dc;
        end;
        if (rr >= 0) and (rr < rows) and (cc >= 0) and (cc < cols)
          and (grid[rr][cc+1] = '#') then
          Inc(Result);
      end;
end;

// Simulate until no changes; threshold=4,useVis=false for part1,
// threshold=5,useVis=true for part2.
function Simulate(threshold: Integer; useVis: Boolean): Integer;
var
  changed: Boolean;
  r, c, cnt: Integer;
begin
  grid := Copy(origGrid, 0, rows);
  repeat
    changed := False;
    SetLength(newGrid, rows);
    for r := 0 to rows-1 do
    begin
      newGrid[r] := grid[r];
      for c := 0 to cols-1 do
      begin
        if grid[r][c+1] = '.' then
          Continue;
        if not useVis then
          cnt := AdjOccupied(r, c)
        else
          cnt := VisOccupied(r, c);
        if (grid[r][c+1] = 'L') and (cnt = 0) then
        begin
          newGrid[r][c+1] := '#';
          changed := True;
        end
        else if (grid[r][c+1] = '#') and (cnt >= threshold) then
        begin
          newGrid[r][c+1] := 'L';
          changed := True;
        end;
      end;
    end;
    grid := Copy(newGrid, 0, rows);
  until not changed;

  // count occupied
  Result := 0;
  for r := 0 to rows-1 do
    for c := 1 to cols do
      if grid[r][c] = '#' then
        Inc(Result);
end;

var
  part1, part2: Integer;
  i: Integer;
begin
  // load input
  lines := TStringList.Create;
  try
    lines.LoadFromFile('input.txt');
    rows := lines.Count;
    if rows = 0 then
      Halt;
    cols := Length(lines[0]);
    SetLength(origGrid, rows);
    for i := 0 to rows-1 do
      origGrid[i] := lines[i];

    part1 := Simulate(4, False);
    part2 := Simulate(5, True);

    Writeln(part1);  // Part 1 result
    Writeln(part2);  // Part 2 result
  finally
    lines.Free;
  end;
end.
