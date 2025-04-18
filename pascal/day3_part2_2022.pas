
program RucksackReorganization;

{$mode objfpc}
{$h+}

uses
  Classes, SysUtils;

const
  MAX_LINES = 10000;

var
  InF: TextFile;
  Lines: array[1..MAX_LINES] of string;
  nLines: Integer;
  sum1, sum2: LongInt;

{ Convert item character to its priority:
  'a'..'z' -> 1..26, 'A'..'Z' -> 27..52 }
function Priority(c: Char): Integer;
begin
  if (c >= 'a') and (c <= 'z') then
    Result := Ord(c) - Ord('a') + 1
  else
    Result := Ord(c) - Ord('A') + 27;
end;

{ Find the single common character between s1 and s2.
  We assume exactly one common type exists. }
function CommonInTwo(const s1, s2: string): Char;
var
  seen: array[Char] of Boolean;
  i: Integer;
begin
  FillChar(seen, SizeOf(seen), False);
  for i := 1 to Length(s1) do
    seen[s1[i]] := True;
  for i := 1 to Length(s2) do
    if seen[s2[i]] then
    begin
      Result := s2[i];
      Exit;
    end;
  { Should not happen if input guarantees one common item. }
  Result := #0;
end;

{ Find the single common character among s1, s2, s3.
  We assume exactly one common type exists. }
function CommonInThree(const s1, s2, s3: string): Char;
var
  seen1, seen2: array[Char] of Boolean;
  i: Integer;
begin
  FillChar(seen1, SizeOf(seen1), False);
  FillChar(seen2, SizeOf(seen2), False);
  for i := 1 to Length(s1) do
    seen1[s1[i]] := True;
  for i := 1 to Length(s2) do
    if seen1[s2[i]] then
      seen2[s2[i]] := True;
  for i := 1 to Length(s3) do
    if seen2[s3[i]] then
    begin
      Result := s3[i];
      Exit;
    end;
  { Should not happen if input guarantees one common item. }
  Result := #0;
end;

var
  i, halfLen: Integer;
  c: Char;
begin
  { 1) Read all lines from "input.txt" }
  AssignFile(InF, 'input.txt');
  Reset(InF);
  nLines := 0;
  while not Eof(InF) do
  begin
    Inc(nLines);
    if nLines > MAX_LINES then
    begin
      Writeln('Error: too many lines in input.');
      Halt(1);
    end;
    ReadLn(InF, Lines[nLines]);
  end;
  CloseFile(InF);

  { 2) Part 1: sum priorities of the single common item in each rucksack }
  sum1 := 0;
  for i := 1 to nLines do
  begin
    halfLen := Length(Lines[i]) div 2;
    c := CommonInTwo(
      Copy(Lines[i], 1, halfLen),
      Copy(Lines[i], halfLen+1, halfLen)
    );
    sum1 := sum1 + Priority(c);
  end;

  { 3) Part 2: sum priorities of the badge item common to each group of 3 }
  sum2 := 0;
  i := 1;
  while i + 2 <= nLines do
  begin
    c := CommonInThree(Lines[i], Lines[i+1], Lines[i+2]);
    sum2 := sum2 + Priority(c);
    Inc(i, 3);
  end;

  { 4) Output each result on its own line }
  Writeln(sum1);
  Writeln(sum2);
end.
