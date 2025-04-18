
program OneTimePad;
{$mode objfpc}{$H+}

uses
  SysUtils, md5;

const
  TARGET_KEYS = 64;
  LOOKAHEAD   = 1000;

type
  TStringArray = array of string;

var
  Salt: string;

{ Compute the lowercase hex MD5 of S }
function MD5Hex(const S: string): string;
begin
  Result := LowerCase(MD5Print(MD5String(S)));
end;

{ Finds the index of the 64th one‐time‑pad key using 'Stretch' extra rounds of hashing }
function FindKeyIndex(Stretch: Integer): Integer;
var
  Cache: TStringArray;
  
  { Ensure our cache is at least Size long }
  procedure EnsureCache(Size: Integer);
  begin
    if Length(Cache) < Size then
      SetLength(Cache, Size);
  end;
  
  { Return the hash for a given index, computing + caching it if needed }
  function GetHash(Index: Integer): string;
  var
    i, rounds: Integer;
    h: string;
  begin
    EnsureCache(Index + 1);
    if Cache[Index] <> '' then
      Exit(Cache[Index]);
    { initial hash }
    h := MD5Hex(Salt + IntToStr(Index));
    { stretching }
    for rounds := 1 to Stretch do
      h := MD5Hex(h);
    Cache[Index] := h;
    Result := h;
  end;
  
  { Look for the first character that appears three times in a row.
    Returns #0 if none found, or the character e.g. 'a'..'f','0'..'9' }
  function FirstTriplet(const H: string): Char;
  var
    i: Integer;
  begin
    Result := #0;
    for i := 1 to Length(H) - 2 do
      if (H[i] = H[i+1]) and (H[i] = H[i+2]) then
      begin
        Result := H[i];
        Exit;
      end;
  end;

var
  Found, KeysFound, I, J: Integer;
  TripCh, FiveSeq: Char;
  H: string;
begin
  KeysFound := 0;
  I := 0;
  SetLength(Cache, 0);
  while KeysFound < TARGET_KEYS do
  begin
    H := GetHash(I);
    TripCh := FirstTriplet(H);
    if TripCh <> #0 then
    begin
      { build the five‐in‐a‐row string }
      FiveSeq := TripCh;
      { look ahead in the next 1000 hashes }
      Found := 0;
      for J := I + 1 to I + LOOKAHEAD do
      begin
        if Pos(StringOfChar(FiveSeq, 5), GetHash(J)) > 0 then
        begin
          Found := 1;
          Break;
        end;
      end;
      if Found = 1 then
      begin
        Inc(KeysFound);
        if KeysFound = TARGET_KEYS then
        begin
          Result := I;
          Exit;
        end;
      end;
    end;
    Inc(I);
  end;
end;

var
  Part1, Part2: Integer;
  InF: TextFile;
begin
  { Read the salt from input.txt }
  AssignFile(InF, 'input.txt');
  Reset(InF);
  ReadLn(InF, Salt);
  CloseFile(InF);

  { Part 1: no extra stretching }
  Part1 := FindKeyIndex(0);
  WriteLn('Part 1 (no stretch): ', Part1);

  { Part 2: 2016 extra MD5 rounds }
  Part2 := FindKeyIndex(2016);
  WriteLn('Part 2 (2016 extra rounds): ', Part2);
end.
