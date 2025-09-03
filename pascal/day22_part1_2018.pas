program RiskLevel;

uses
  SysUtils;

type
  TInt64Array = array of Int64;

const
  MODULO: Int64 = 20183;
  MUL_X: Int64 = 16807;
  MUL_Y: Int64 = 48271;

var
  f: Text;
  s: string;
  value: string;
  depth: Int64;
  target_x, target_y: Integer;
  risk_level: Int64;
  prevCol, currCol: TInt64Array;
  x, y: Integer;
  geologic_index: Int64;
  code: Integer;
  commaPos: Integer;
  xStr, yStr: string;

begin
  if not FileExists('input.txt') then
  begin
    WriteLn('Error opening file');
    Halt(1);
  end;

  Assign(f, 'input.txt');
  Reset(f);

  ReadLn(f, s);
  value := Copy(s, 8, Length(s) - 7);
  Val(value, depth, code);
  if code <> 0 then
  begin
    WriteLn('Error parsing depth');
    Halt(1);
  end;

  ReadLn(f, s);
  value := Copy(s, 9, Length(s) - 8);
  commaPos := Pos(',', value);
  xStr := Copy(value, 1, commaPos - 1);
  yStr := Copy(value, commaPos + 1, Length(value) - commaPos);
  Val(xStr, target_x, code);
  if code <> 0 then
  begin
    WriteLn('Error parsing target_x');
    Halt(1);
  end;
  Val(yStr, target_y, code);
  if code <> 0 then
  begin
    WriteLn('Error parsing target_y');
    Halt(1);
  end;

  SetLength(prevCol, target_y + 1);
  SetLength(currCol, target_y + 1);

  risk_level := 0;

  for x := 0 to target_x do
  begin
    for y := 0 to target_y do
    begin
      if ((x = 0) and (y = 0)) or ((x = target_x) and (y = target_y)) then
        geologic_index := 0
      else if y = 0 then
        geologic_index := Int64(x) * MUL_X
      else if x = 0 then
        geologic_index := Int64(y) * MUL_Y
      else
        geologic_index := prevCol[y] * currCol[y - 1];

      currCol[y] := (geologic_index + depth) mod MODULO;
      risk_level := risk_level + (currCol[y] mod 3);
    end;

    for y := 0 to target_y do
      prevCol[y] := currCol[y];
  end;

  WriteLn(risk_level);

  Close(f);
end.