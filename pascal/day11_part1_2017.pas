
program HexGridPath;

uses
  Math; // For Max and Abs

var
  inputFile: Text;
  x, y, z: LongInt;
  ch: Char;
  direction: string;
  result: LongInt;

function Max3(a, b, c: LongInt): LongInt;
begin
  Result := Max(a, Max(b, c));
end;

begin
  Assign(inputFile, 'input.txt');
  {$I-}
  Reset(inputFile);
  {$I+}
  if IOResult <> 0 then
  begin
    WriteLn('Error: Cannot open input.txt');
    Halt(1);
  end;

  x := 0;
  y := 0;
  z := 0;
  direction := '';

  while not Eof(inputFile) do
  begin
    Read(inputFile, ch);

    if ch in ['a'..'z'] then
    begin
      direction := direction + ch;
    end;

    if (ch = ',') or Eof(inputFile) then
    begin
      if Length(direction) > 0 then
      begin
        if direction = 'n' then begin Inc(y); Dec(z); end
        else if direction = 'ne' then begin Inc(x); Dec(z); end
        else if direction = 'se' then begin Inc(x); Dec(y); end
        else if direction = 's' then begin Dec(y); Inc(z); end
        else if direction = 'sw' then begin Dec(x); Inc(z); end
        else if direction = 'nw' then begin Dec(x); Inc(y); end;
        
        direction := ''; 
      end;
    end;
  end;

  Close(inputFile);

  result := Max3(Abs(x), Abs(y), Abs(z));
  WriteLn(result);
end.
