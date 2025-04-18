
program UniversalOrbitMap;
{$mode objfpc}{$H+}
uses
  SysUtils;

const
  MAXN = 20000;

var
  nameArr: array[0..MAXN-1] of string;
  parent:  array[0..MAXN-1] of integer;
  distArr: array[0..MAXN-1] of integer;
  N: integer = 0;

function getId(const s: string): Integer;
var
  i: Integer;
begin
  for i := 0 to N-1 do
    if nameArr[i] = s then
      Exit(i);
  if N >= MAXN then
    raise Exception.Create('Exceeded max objects');
  nameArr[N] := s;
  parent[N] := -1;
  Result := N;
  Inc(N);
end;

var
  f: Text;
  line: string;
  pPos, pid, cid: Integer;
  totalOrbits, i, j, d: Integer;
  youId, sanId, youPar, sanPar, transfers: Integer;
begin
  Assign(f, 'input.txt');
  Reset(f);
  try
    while not Eof(f) do
    begin
      ReadLn(f, line);
      if line = '' then Continue;
      pPos := Pos(')', line);
      pid := getId(Copy(line, 1, pPos-1));
      cid := getId(Copy(line, pPos+1, Length(line)-pPos));
      parent[cid] := pid;
    end;
  finally
    Close(f);
  end;

  { Part 1: sum depths of all nodes }
  totalOrbits := 0;
  for i := 0 to N-1 do
  begin
    j := i;
    while (j >= 0) do
    begin
      j := parent[j];
      if j >= 0 then Inc(totalOrbits);
    end;
  end;

  { Part 2: minimum transfers from YOU->SAN }
  youId := getId('YOU');  { Should already exist in map }
  sanId := getId('SAN');
  youPar := parent[youId];
  sanPar := parent[sanId];

  { Mark distance from YOU's parent up to COM }
  for i := 0 to N-1 do
    distArr[i] := -1;
  d := 0; j := youPar;
  while j >= 0 do
  begin
    distArr[j] := d;
    Inc(d);
    j := parent[j];
  end;

  { Walk from SAN's parent upward until we hit a marked node }
  transfers := 0;
  j := sanPar;
  while (j >= 0) and (distArr[j] < 0) do
  begin
    Inc(transfers);
    j := parent[j];
  end;
  if j >= 0 then
    transfers := transfers + distArr[j]
  else
    raise Exception.Create('No common ancestor found');

  { Output results }
  WriteLn(totalOrbits);
  WriteLn(transfers);
end.
