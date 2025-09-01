
program LongestRoute;

{$mode objfpc}{$H+}

uses
  SysUtils, StrUtils;

const
  MAX_LOC = 100;

type
  TDist = array[0..MAX_LOC-1, 0..MAX_LOC-1] of Integer;
  TNames = array[0..MAX_LOC-1] of string;

var
  Names   : TNames;
  Dist    : TDist;
  LocCnt  : Integer = 0;
  Route   : array[0..MAX_LOC-1] of Integer;

function Max(A, B: Integer): Integer;
begin
  if A > B then Max := A else Max := B;
end;

function FindIdx(const S: string): Integer;
var
  i: Integer;
begin
  for i := 0 to LocCnt - 1 do
    if Names[i] = S then exit(i);
  FindIdx := -1;
end;

procedure AddDist(const A, B: string; D: Integer);
var
  i, j: Integer;
begin
  i := FindIdx(A);
  if i = -1 then begin Names[LocCnt] := A; i := LocCnt; Inc(LocCnt); end;
  j := FindIdx(B);
  if j = -1 then begin Names[LocCnt] := B; j := LocCnt; Inc(LocCnt); end;
  Dist[i, j] := D;
  Dist[j, i] := D;
end;

function CalcRoute(N: Integer): Integer;
var
  s, k: Integer;
begin
  s := 0;
  for k := 0 to N - 2 do
    s := s + Dist[Route[k], Route[k + 1]];
  CalcRoute := s;
end;

function Longest(Depth: Integer): Integer;
var
  i, tmp, best: Integer;
begin
  if Depth = LocCnt then
    exit(CalcRoute(LocCnt));
  best := 0;
  for i := Depth to LocCnt - 1 do
  begin
    tmp := Route[Depth];
    Route[Depth] := Route[i];
    Route[i] := tmp;
    best := Max(best, Longest(Depth + 1));
    Route[i] := Route[Depth];
    Route[Depth] := tmp;
  end;
  Longest := best;
end;

var
  F: TextFile;
  Line, FromS, ToS: string;
  Parts: TStringArray;
  D: Integer;
  i: Integer;
begin
  AssignFile(F, 'input.txt');
  Reset(F);
  while not Eof(F) do
  begin
    ReadLn(F, Line);
    if Trim(Line) = '' then Continue;
    Parts := SplitString(Line, ' ');
    if Length(Parts) < 5 then Continue;
    FromS := Parts[0];
    ToS   := Parts[2];
    D     := StrToInt(Parts[4]);
    AddDist(FromS, ToS, D);
  end;
  CloseFile(F);

  for i := 0 to LocCnt - 1 do
    Route[i] := i;

  WriteLn(Longest(0));
end.
