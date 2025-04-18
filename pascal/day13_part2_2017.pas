
program PacketScanners;
{$mode objfpc}{$h+}
uses
  Classes, SysUtils;

type
  TLayer = record
    depth, range, cycle: Integer;
  end;

var
  layers: array of TLayer;
  tf: TextFile;
  line: string;
  sepPos: Integer;
  d, r, i: Integer;
  severity: Int64;
  delay: Int64;
  caught: Boolean;

begin
  { Read input from input.txt }
  AssignFile(tf, 'input.txt');
  Reset(tf);
  SetLength(layers, 0);
  while not Eof(tf) do
  begin
    ReadLn(tf, line);
    if line = '' then Continue;
    sepPos := Pos(':', line);
    d := StrToInt(Trim(Copy(line, 1, sepPos-1)));
    r := StrToInt(Trim(Copy(line, sepPos+1, MaxInt)));
    { store one layer }
    SetLength(layers, Length(layers)+1);
    layers[High(layers)].depth := d;
    layers[High(layers)].range := r;
    layers[High(layers)].cycle := 2*(r-1);
  end;
  CloseFile(tf);

  { Part 1: compute severity with zero delay (i.e. enter layer d at time T=d) }
  severity := 0;
  for i := 0 to High(layers) do
    with layers[i] do
      if (depth mod cycle) = 0 then
        severity := severity + Int64(depth) * range;

  WriteLn('Part1: Severity = ', severity);

  { Part 2: find the minimal delay so that for all layers, (delay+depth) mod cycle <> 0 }
  delay := 0;
  repeat
    caught := False;
    for i := 0 to High(layers) do
      with layers[i] do
        if ((delay + depth) mod cycle) = 0 then
        begin
          caught := True;
          Break;
        end;
    if not caught then
      Break
    else
      Inc(delay);
  until False;

  WriteLn('Part2: Minimum delay = ', delay);
end.
