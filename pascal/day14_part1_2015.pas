program ReindeerRace;

type
  TReindeer = record
    speed, flyTime, restTime: Integer;
  end;

var
  reindeers: array of TReindeer;

{ Extracts the first integer found in s and removes everything up to it }
function ExtractInt(var s: string): Integer;
var
  i, j, code: Integer;
  numStr: string;
  num: Integer;
begin
  i := 1;
  while (i <= Length(s)) and not (s[i] in ['0'..'9']) do
    Inc(i);
  j := i;
  while (j <= Length(s)) and (s[j] in ['0'..'9']) do
    Inc(j);
  numStr := Copy(s, i, j - i);
  Val(numStr, num, code);
  Delete(s, 1, j - 1);
  ExtractInt := num;
end;

procedure Main;
var
  f: TextFile;
  line: string;
  r: TReindeer;
  T, i: Integer;
  fullCycles, remTime, flySec, dist, maxDist: Integer;
begin
  Assign(f, 'input.txt');
  Reset(f);
  SetLength(reindeers, 0);
  while not Eof(f) do
  begin
    ReadLn(f, line);
    r.speed    := ExtractInt(line);
    r.flyTime  := ExtractInt(line);
    r.restTime := ExtractInt(line);
    SetLength(reindeers, Length(reindeers) + 1);
    reindeers[High(reindeers)] := r;
  end;
  Close(f);

  T := 2503;
  maxDist := 0;
  for i := 0 to High(reindeers) do
  begin
    fullCycles := T div (reindeers[i].flyTime + reindeers[i].restTime);
    remTime    := T mod (reindeers[i].flyTime + reindeers[i].restTime);
    if remTime > reindeers[i].flyTime then
      flySec := reindeers[i].flyTime
    else
      flySec := remTime;
    dist := (fullCycles * reindeers[i].flyTime + flySec) * reindeers[i].speed;
    if dist > maxDist then
      maxDist := dist;
  end;

  WriteLn(maxDist);
end;

begin
  Main;
end.
