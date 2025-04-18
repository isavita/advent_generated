program TreetopTreeHouse;
{$mode objfpc}{$H+}
uses
  SysUtils;

var
  lines: array of string;
  grid: array of array of Integer;
  n, m: Integer;
  i, j, x, y: Integer;
  line: string;
  visibleCount, maxScenic, scenic: Integer;
  blocked: Boolean;
  viewL, viewR, viewU, viewD: Integer;

begin
  { Read the input grid from "input.txt" }
  Assign(Input, 'input.txt');
  Reset(Input);
  i := 0;
  while not Eof(Input) do
  begin
    ReadLn(line);
    SetLength(lines, i + 1);
    lines[i] := line;
    Inc(i);
  end;
  Close(Input);

  { Dimensions }
  n := i;
  if n = 0 then
    Exit;              { no data }
  m := Length(lines[0]);

  { Build integer grid }
  SetLength(grid, n, m);
  for i := 0 to n - 1 do
    for j := 1 to m do
      grid[i][j - 1] := Ord(lines[i][j]) - Ord('0');

  { Part 1: count trees visible from outside }
  visibleCount := 0;
  for i := 0 to n - 1 do
    for j := 0 to m - 1 do
    begin
      { Edge trees are always visible }
      if (i = 0) or (i = n - 1) or (j = 0) or (j = m - 1) then
      begin
        Inc(visibleCount);
        Continue;
      end;
      { Check left }
      blocked := False;
      for y := 0 to j - 1 do
        if grid[i][y] >= grid[i][j] then
        begin
          blocked := True;
          Break;
        end;
      if not blocked then
      begin
        Inc(visibleCount);
        Continue;
      end;
      { Check right }
      blocked := False;
      for y := j + 1 to m - 1 do
        if grid[i][y] >= grid[i][j] then
        begin
          blocked := True;
          Break;
        end;
      if not blocked then
      begin
        Inc(visibleCount);
        Continue;
      end;
      { Check up }
      blocked := False;
      for x := 0 to i - 1 do
        if grid[x][j] >= grid[i][j] then
        begin
          blocked := True;
          Break;
        end;
      if not blocked then
      begin
        Inc(visibleCount);
        Continue;
      end;
      { Check down }
      blocked := False;
      for x := i + 1 to n - 1 do
        if grid[x][j] >= grid[i][j] then
        begin
          blocked := True;
          Break;
        end;
      if not blocked then
        Inc(visibleCount);
    end;

  { Part 2: compute maximum scenic score }
  maxScenic := 0;
  for i := 0 to n - 1 do
    for j := 0 to m - 1 do
    begin
      { look left }
      viewL := 0;
      for y := j - 1 downto 0 do
      begin
        Inc(viewL);
        if grid[i][y] >= grid[i][j] then
          Break;
      end;
      { look right }
      viewR := 0;
      for y := j + 1 to m - 1 do
      begin
        Inc(viewR);
        if grid[i][y] >= grid[i][j] then
          Break;
      end;
      { look up }
      viewU := 0;
      for x := i - 1 downto 0 do
      begin
        Inc(viewU);
        if grid[x][j] >= grid[i][j] then
          Break;
      end;
      { look down }
      viewD := 0;
      for x := i + 1 to n - 1 do
      begin
        Inc(viewD);
        if grid[x][j] >= grid[i][j] then
          Break;
      end;
      scenic := viewL * viewR * viewU * viewD;
      if scenic > maxScenic then
        maxScenic := scenic;
    end;

  { Output results }
  WriteLn(visibleCount);
  WriteLn(maxScenic);
end.