
program SeedMap;

{$mode objfpc}{$H+}

uses
  SysUtils, StrUtils;

type
  TRangeMap = record
    srcStart, destStart, len: Int64;
  end;

function ConvertNumber(num: Int64; const maps: array of TRangeMap; cnt: Integer): Int64;
var
  i: Integer;
begin
  Result := num;
  for i := 0 to cnt - 1 do
    if (num >= maps[i].srcStart) and (num < maps[i].srcStart + maps[i].len) then
    begin
      Result := maps[i].destStart + (num - maps[i].srcStart);
      Exit;
    end;
end;

var
  f: TextFile;
  line: string;
  seeds: array[0..99] of Int64;
  seedCnt, mapIdx, i, j: Integer;
  mapCnt: array[0..9] of Integer;
  maps: array[0..9,0..99] of TRangeMap;
  parts: TStringArray;
  num: Int64;
  minLoc, loc: Int64;
begin
  AssignFile(f, 'input.txt');
  Reset(f);
  seedCnt := 0;
  mapIdx := -1;
  FillChar(mapCnt, SizeOf(mapCnt), 0);
  while not Eof(f) do
  begin
    ReadLn(f, line);
    if Pos('map:', line) > 0 then
    begin
      Inc(mapIdx);
      mapCnt[mapIdx] := 0;
    end
    else if Pos('seeds:', line) = 1 then
    begin
      Delete(line, 1, 7);
      parts := SplitString(Trim(line), ' ');
      for i := Low(parts) to High(parts) do
        if TryStrToInt64(parts[i], num) then
        begin
          seeds[seedCnt] := num;
          Inc(seedCnt);
        end;
    end
    else
    begin
      parts := SplitString(Trim(line), ' ');
      if Length(parts) = 3 then
        if TryStrToInt64(parts[0], num) then
        begin
          maps[mapIdx][mapCnt[mapIdx]].destStart := num;
          TryStrToInt64(parts[1], maps[mapIdx][mapCnt[mapIdx]].srcStart);
          TryStrToInt64(parts[2], maps[mapIdx][mapCnt[mapIdx]].len);
          Inc(mapCnt[mapIdx]);
        end;
    end;
  end;
  CloseFile(f);

  minLoc := High(Int64);
  for i := 0 to seedCnt - 1 do
  begin
    loc := seeds[i];
    for j := 0 to mapIdx do
      loc := ConvertNumber(loc, maps[j], mapCnt[j]);
    if loc < minLoc then
      minLoc := loc;
  end;
  WriteLn(minLoc);
end.
