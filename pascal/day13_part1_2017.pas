
program PacketScanners;

uses
  SysUtils;

procedure ParseLine(Line: string; var D, R: LongInt);
var
  i: Integer;
begin
  D := 0;
  R := 0;
  i := 1;

  { Read first number (depth) }
  while (i <= Length(Line)) and (Line[i] in ['0'..'9']) do
  begin
    D := D * 10 + (Ord(Line[i]) - Ord('0'));
    Inc(i);
  end;

  { Skip non-digit characters until the next number }
  while (i <= Length(Line)) and not (Line[i] in ['0'..'9']) do
    Inc(i);

  { Read second number (range) }
  while (i <= Length(Line)) and (Line[i] in ['0'..'9']) do
  begin
    R := R * 10 + (Ord(Line[i]) - Ord('0'));
    Inc(i);
  end;
end;

var
  f: Text;
  line: string;
  depth, rng: LongInt;
  severity: LongInt;

begin
  severity := 0;

  assign(f, 'input.txt');
  reset(f);
  while not Eof(f) do
  begin
    ReadLn(f, line);
    depth := 0;
    rng := 0;
    ParseLine(line, depth, rng);

    if rng = 0 then
      continue; { skip invalid lines }

    if rng = 1 then
      severity := severity + depth * rng
    else
      if (depth mod (2 * (rng - 1))) = 0 then
        severity := severity + depth * rng;
  end;
  Close(f);

  WriteLn(severity);
end.
