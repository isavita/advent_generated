
program PowerGrid;

{$mode objfpc}{$H+}
const
  GRID_SIZE = 300;
type
  TGrid = array[0..GRID_SIZE-1,0..GRID_SIZE-1] of Integer;
  TSum  = array[0..GRID_SIZE,0..GRID_SIZE] of Integer;

var
  grid : TGrid;
  sum  : TSum;
  serial, x, y, size, maxPower, maxX, maxY, maxSize, rackID, powerLevel, totalPower: Integer;
  f: TextFile;

begin
  AssignFile(f,'input.txt');
  Reset(f);
  ReadLn(f,serial);
  CloseFile(f);

  for y:=0 to GRID_SIZE-1 do
    for x:=0 to GRID_SIZE-1 do
    begin
      rackID := x+11;
      powerLevel := rackID*(y+1);
      powerLevel := powerLevel + serial;
      powerLevel := powerLevel * rackID;
      powerLevel := (powerLevel div 100) mod 10;
      powerLevel := powerLevel - 5;
      grid[y,x] := powerLevel;
    end;

  for y:=0 to GRID_SIZE do
    for x:=0 to GRID_SIZE do
      sum[y,x] := 0;

  for y:=1 to GRID_SIZE do
    for x:=1 to GRID_SIZE do
      sum[y,x] := grid[y-1,x-1] + sum[y-1,x] + sum[y,x-1] - sum[y-1,x-1];

  maxPower := -2147483648;
  for size:=1 to GRID_SIZE do
    for y:=1 to GRID_SIZE-size+1 do
      for x:=1 to GRID_SIZE-size+1 do
      begin
        totalPower := sum[y+size-1,x+size-1] - sum[y+size-1,x-1] -
                      sum[y-1,x+size-1] + sum[y-1,x-1];
        if totalPower > maxPower then
        begin
          maxPower := totalPower;
          maxX := x;
          maxY := y;
          maxSize := size;
        end;
      end;

  WriteLn(maxX,',',maxY,',',maxSize);
end.
