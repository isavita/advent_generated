
program solve;

{$APPTYPE CONSOLE}

uses SysUtils;

var
  f: Text;
  line, part, digit: string;
  count, i, j, len: Integer;
  parts: array[0..1] of string;

begin
  Assign(f, 'input.txt');
  Reset(f);
  count := 0;
  while not Eof(f) do
  begin
    ReadLn(f, line);
    j := 1;
    parts[0] := '';
    parts[1] := '';
    for i := 1 to Length(line) do
    begin
      if line[i] = '|' then
      begin
        j := 2;
        continue;
      end;
      if j = 1 then
        parts[0] := parts[0] + line[i]
      else
        parts[1] := parts[1] + line[i];
    end;
    
    i := 1;
    while i <= Length(parts[1]) do
    begin
      digit := '';
      while (i <= Length(parts[1])) and (parts[1][i] <> ' ') do
      begin
        digit := digit + parts[1][i];
        i := i + 1;
      end;
      len := Length(digit);
      if (len = 2) or (len = 4) or (len = 3) or (len = 7) then
        count := count + 1;
      i := i + 1;
    end;
  end;
  Close(f);
  WriteLn(count);
end.
