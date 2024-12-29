
program crt;

{$APPTYPE CONSOLE}

uses
  SysUtils;

var
  x: array of Integer;
  line: string;
  n, i, crtx, crty, cycle, regx: Integer;
  grid: array[0..5, 0..39] of Boolean;
  f: Text;

function abs(x: Integer): Integer;
begin
  if x < 0 then
    abs := -x
  else
    abs := x;
end;

begin
  SetLength(x, 1);
  x[0] := 1;
  cycle := 0;
  regx := 1;

  Assign(f, 'input.txt');
  Reset(f);
  while not Eof(f) do
  begin
    ReadLn(f, line);
    if line = 'noop' then
    begin
      Inc(cycle);
      SetLength(x, Length(x) + 1);
      x[Length(x) - 1] := regx;
    end
    else
    begin
      Delete(line, 1, 5);
      Val(line, n);
      Inc(cycle);
      SetLength(x, Length(x) + 1);
      x[Length(x) - 1] := regx;
      Inc(cycle);
      SetLength(x, Length(x) + 1);
      regx := regx + n;
      x[Length(x) - 1] := regx;
    end;
  end;
  Close(f);

  for i := 0 to Length(x) - 1 do
  begin
    crtx := i mod 40;
    crty := i div 40;
    if abs(crtx - x[i]) <= 1 then
      grid[crty, crtx] := True
    else
      grid[crty, crtx] := False;
  end;

  for crty := 0 to 5 do
  begin
    for crtx := 0 to 39 do
    begin
      if grid[crty, crtx] then
        Write('#')
      else
        Write('.');
    end;
    writeln;
  end;
end.
