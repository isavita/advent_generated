program SumDigits;
var
  f: Text;
  line: string;
  names: array[0..9] of string;
  i, j, fd, ld, total: LongInt;
  firstSet: Boolean;
begin
  names[0] := 'zero'; names[1] := 'one'; names[2] := 'two';
  names[3] := 'three'; names[4] := 'four'; names[5] := 'five';
  names[6] := 'six'; names[7] := 'seven'; names[8] := 'eight';
  names[9] := 'nine';
  Assign(f, 'input.txt');
  Reset(f);
  total := 0;
  while not Eof(f) do
  begin
    ReadLn(f, line);
    fd := 0; ld := 0; firstSet := False;
    for i := 1 to Length(line) do
    begin
      if (line[i] >= '0') and (line[i] <= '9') then
      begin
        j := Ord(line[i]) - Ord('0');
        if not firstSet then
        begin
          fd := j;
          firstSet := True
        end;
        ld := j
      end
      else
        for j := 0 to 9 do
          if (i + Length(names[j]) - 1 <= Length(line)) and
             (Copy(line, i, Length(names[j])) = names[j]) then
          begin
            if not firstSet then
            begin
              fd := j;
              firstSet := True
            end;
            ld := j;
            Break
          end;
    end;
    Inc(total, fd * 10 + ld)
  end;
  WriteLn(total);
  Close(f)
end.
