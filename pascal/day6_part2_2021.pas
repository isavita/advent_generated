program LanternfishGrowth;
{$mode objfpc}{$H+}
uses
  SysUtils;

const
  DAYS1 = 80;
  DAYS2 = 256;

var
  counts: array[0..8] of QWord;
  line, token: string;
  i, day, p: Integer;
  births: QWord;
  answer80, answer256: QWord;

begin
  { Read initial state from input.txt }
  AssignFile(Input, 'input.txt');
  Reset(Input);
  for i := 0 to 8 do
    counts[i] := 0;
  if not EOF(Input) then
  begin
    ReadLn(line);
    while line <> '' do
    begin
      p := Pos(',', line);
      if p > 0 then
      begin
        token := Copy(line, 1, p-1);
        Delete(line, 1, p);
      end
      else
      begin
        token := line;
        line := '';
      end;
      counts[StrToInt(token)] += 1;
    end;
  end;
  CloseFile(Input);

  { Simulate for DAYS2 days, record after DAYS1 }
  for day := 1 to DAYS2 do
  begin
    births := counts[0];
    { Shift timers down }
    for i := 0 to 7 do
      counts[i] := counts[i+1];
    { Reset parents and add new fish }
    counts[6] := counts[6] + births;
    counts[8] := births;

    if day = DAYS1 then
    begin
      answer80 := 0;
      for i := 0 to 8 do
        answer80 += counts[i];
    end;
  end;

  { Total after DAYS2 }
  answer256 := 0;
  for i := 0 to 8 do
    answer256 += counts[i];

  { Output results }
  WriteLn('After ', DAYS1, ' days: ', answer80);
  WriteLn('After ', DAYS2, ' days: ', answer256);
end.