program ChronalCharge;

var
  grid: array[1..300, 1..300] of Integer;
  x, y, serial, maxPower, maxX, maxY, i, j, sum: Integer;

begin
  assign(input, 'input.txt');
  reset(input);
  readln(serial);
  close(input);

  for x := 1 to 300 do
  begin
    for y := 1 to 300 do
    begin
      grid[x, y] := ((((x + 10) * y + serial) * (x + 10)) div 100 mod 10) - 5;
    end;
  end;

  maxPower := -46; // minimum possible value for a 3x3 square
  for x := 1 to 298 do
  begin
    for y := 1 to 298 do
    begin
      sum := 0;
      for i := x to x + 2 do
      begin
        for j := y to y + 2 do
        begin
          sum := sum + grid[i, j];
        end;
      end;
      if sum > maxPower then
      begin
        maxPower := sum;
        maxX := x;
        maxY := y;
      end;
    end;
  end;

  writeln(maxX, ',', maxY);
end.