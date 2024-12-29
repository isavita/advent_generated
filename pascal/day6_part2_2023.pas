
program RaceWays;

var
  time, distance, waysToWin, holdTime, travelTime: Int64;
  inputFile: Text;
  line, numStr: String;
  i: Integer;

begin
  Assign(inputFile, 'input.txt');
  Reset(inputFile);
  time := 0;
  distance := 0;
  while not Eof(inputFile) do
  begin
    ReadLn(inputFile, line);
    if line = '' then
      Continue;
    i := Pos(':', line);
    numStr := '';
    for i := i + 1 to Length(line) do
    begin
      if line[i] in ['0'..'9'] then
        numStr := numStr + line[i];
    end;
    if time = 0 then
      Val(numStr, time)
    else
      Val(numStr, distance);
  end;
  Close(inputFile);
  waysToWin := 0;
  holdTime := time div 2;
  travelTime := time - holdTime;
  if holdTime * travelTime > distance then
  begin
    while holdTime * (time - holdTime) > distance do
    begin
      Dec(holdTime);
    end;
    waysToWin := (time div 2) - holdTime;
    if time mod 2 = 0 then
      waysToWin := waysToWin * 2 -1
    else
      waysToWin := waysToWin * 2;
  end;
  WriteLn(waysToWin);
end.
