program PartTwo;
var
  seen: array[0..16777215] of Boolean;
  r3, r5, r1, lastu: LongInt;
  f: Text;
begin
  Assign(f,'input.txt'); Reset(f); Close(f);
  r5:=0; lastu:=0;
  while True do
  begin
    r3:=r5 or 65536;
    r5:=7586220;
    repeat
      r1:=r3 and 255;
      r5:=(((r5 + r1) and 16777215)*65899) and 16777215;
      if r3<256 then break;
      r3:=r3 div 256;
    until False;
    if seen[r5] then
    begin
      Writeln(lastu);
      Halt;
    end;
    seen[r5]:=True;
    lastu:=r5;
  end;
end.