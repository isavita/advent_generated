
program Day13;

{$mode objfpc}{$H+}

uses
  SysUtils;

type
  TGrid = array of array of Char;

function FindVerticalReflection(const grid: TGrid): Integer;
var
  i, j, k, width, height: Integer;
  isReflection: Boolean;
begin
  height := Length(grid);
  width := Length(grid[0]);

  for i := 1 to width - 1 do
  begin
    isReflection := True;
    for j := 0 to width - 1 do
    begin
      k := i - 1;
      if (i + (i - 1 - j) >= width) or (j > i - 1) then
        continue;
        
      if grid[0][j] <> grid[0][i + (i - 1 - j)] then
      begin
        isReflection := False;
        break;
      end;
    end;
    
    if isReflection then
    begin
      for j := 0 to height - 1 do
      begin
        for k := 0 to width - 1 do
        begin
          if (i + (i - 1 - k) >= width) or (k > i - 1) then
            continue;

          if grid[j][k] <> grid[j][i + (i - 1 - k)] then
          begin
            isReflection := False;
            break;
          end;
        end;
        if not isReflection then
          break;
      end;
    end;

    if isReflection then
    begin
      Result := i;
      Exit;
    end;
  end;
  Result := 0;
end;

function FindHorizontalReflection(const grid: TGrid): Integer;
var
  i, j, k, width, height: Integer;
  isReflection: Boolean;
begin
  height := Length(grid);
  width := Length(grid[0]);

  for i := 1 to height - 1 do
  begin
    isReflection := True;
    for j := 0 to height - 1 do
    begin
      k := i - 1;
      if (i + (i - 1 - j) >= height) or (j > i - 1) then
        continue;

      if grid[j][0] <> grid[i + (i - 1 - j)][0] then
      begin
        isReflection := False;
        break;
      end;
    end;

    if isReflection then
    begin
      for j := 0 to width - 1 do
      begin
        for k := 0 to height - 1 do
        begin
          if (i + (i - 1 - k) >= height) or (k > i - 1) then
            continue;

          if grid[k][j] <> grid[i + (i - 1 - k)][j] then
          begin
            isReflection := False;
            break;
          end;
        end;
        if not isReflection then
          break;
      end;
    end;

    if isReflection then
    begin
      Result := i;
      Exit;
    end;
  end;
  Result := 0;
end;

function Solve(const filename: string): Integer;
var
  f: TextFile;
  line: string;
  grids: array of TGrid;
  grid: TGrid;
  gridIndex: Integer;
  i, j, sum, vertical, horizontal: Integer;
begin
  Assign(f, filename);
  Reset(f);

  SetLength(grids, 0);
  gridIndex := -1;

  while not Eof(f) do
  begin
    ReadLn(f, line);
    if Length(line) = 0 then
    begin
      // New grid
      if gridIndex <> -1 then
        SetLength(grids, Length(grids) + 1);
      
      grids[Length(grids) - 1] := grid;
      gridIndex := -1;
    end
    else
    begin
      if gridIndex = -1 then
      begin
        gridIndex := 0;
        SetLength(grid, 0);
      end;

      SetLength(grid, Length(grid) + 1);
      SetLength(grid[Length(grid) - 1], Length(line));
      for i := 0 to Length(line) - 1 do
      begin
        grid[Length(grid) - 1][i] := line[i + 1];
      end;
    end;
  end;

  // Add the last grid
  if gridIndex <> -1 then
  begin
    SetLength(grids, Length(grids) + 1);
    grids[Length(grids) - 1] := grid;
  end;

  Close(f);

  sum := 0;
  for i := 0 to Length(grids) - 1 do
  begin
    vertical := FindVerticalReflection(grids[i]);
    horizontal := FindHorizontalReflection(grids[i]);

    sum := sum + vertical + (horizontal * 100);
  end;

  Result := sum;
end;

var
  result: Integer;

begin
  result := Solve('input.txt');
  WriteLn(result);
end.
