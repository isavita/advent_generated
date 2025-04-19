program SafeReports;
uses sysutils;
var
  f: Text;
  s: string;
  i, j, num, cnt, err: Integer;
  arr: array of Integer;
  incd, decd: Boolean;
begin
  Assign(f, 'input.txt'); Reset(f);
  cnt := 0;
  while not Eof(f) do
  begin
    Readln(f, s);
    i := 1;
    SetLength(arr, 0);
    while i <= Length(s) do
    begin
      while (i <= Length(s)) and (s[i] = ' ') do Inc(i);
      if i > Length(s) then Break;
      j := i;
      while (j <= Length(s)) and (s[j] <> ' ') do Inc(j);
      Val(Copy(s, i, j - i), num, err);
      SetLength(arr, Length(arr) + 1);
      arr[High(arr)] := num;
      i := j;
    end;
    incd := True; decd := True;
    for i := 0 to High(arr) - 1 do
    begin
      num := arr[i+1] - arr[i];
      if (Abs(num) < 1) or (Abs(num) > 3) then
      begin
        incd := False; decd := False;
        Break;
      end;
      if num < 0 then incd := False
      else if num > 0 then decd := False;
    end;
    if incd or decd then Inc(cnt);
  end;
  Close(f);
  Writeln(cnt);
end.