
program DockingData;

{$MODE DELPHI}  // Use Delphi mode for 64-bit integer support

uses
  SysUtils;

type
  TMemory = array of Int64;

var
  Mask: string;
  Memory: TMemory;
  Address, Value: Int64;
  Line: string;
  I: Integer;
  Sum: Int64;

function ApplyMask(Value: Int64): Int64;
var
  I: Integer;
begin
  Result := Value;
  for I := 0 to 35 do
  begin
    case Mask[36 - I] of  // Process mask from left to right
      '1': Result := Result or (Int64(1) shl I);
      '0': Result := Result and (not (Int64(1) shl I));
    end;
  end;
end;

begin
  SetLength(Memory, 100000); // Allocate a reasonable amount of memory
  Sum := 0;
  Mask := '';

  Assign(Input, 'input.txt');
  Reset(Input);

  try
    while not Eof(Input) do
    begin
      ReadLn(Input, Line);
      if Copy(Line, 1, 4) = 'mask' then
      begin
        Mask := Copy(Line, 8, Length(Line) - 7);
      end
      else if Copy(Line, 1, 3) = 'mem' then
      begin
        // Extract address and value
        Address := StrToInt64(Copy(Line, 5, Pos(']', Line) - 5));
        Value := StrToInt64(Copy(Line, Pos('=', Line) + 2, Length(Line) - Pos('=', Line) - 1));
        
        // Apply mask and write to memory
        Memory[Address] := ApplyMask(Value);
      end;
    end;
  finally
    Close(Input);
  end;

  // Calculate the sum of all values in memory
  Sum := 0;
  for I := Low(Memory) to High(Memory) do
  begin
    Sum := Sum + Memory[I];
  end;

  WriteLn('Sum of values in memory: ', Sum);
end.
