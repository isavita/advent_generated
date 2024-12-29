
program secret;

{$O+,Q+}

function nextSecret(s: UInt64): UInt64;
var
  x: UInt64;
begin
  x := s shl 6;
  s := s xor x;
  s := s and $FFFFFF;
  x := s div 32;
  s := s xor x;
  s := s and $FFFFFF;
  x := s shl 11;
  s := s xor x;
  s := s and $FFFFFF;
  nextSecret := s;
end;

var
  f: Text;
  line: string;
  n, total, s: UInt64;
  i: Integer;
begin
  Assign(f, 'input.txt');
  Reset(f);
  total := 0;
  while not Eof(f) do
  begin
    ReadLn(f, line);
    if line <> '' then
    begin
      Val(line, n);
      s := n;
      for i := 1 to 2000 do
        s := nextSecret(s);
      total := total + s;
    end;
  end;
  Close(f);
  WriteLn(total);
end.
