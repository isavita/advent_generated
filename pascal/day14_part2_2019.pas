
program SpaceStoichiometry;
{$mode objfpc}{$H+}
uses
  SysUtils, Classes;

const
  INPUT_FILE = 'input.txt';
  ORE_LIMIT = 1000000000000;  { 1 trillion }

type
  TReaction = record
    OutQty: Int64;
    InCount: Integer;
    InQty : array of Int64;
    InIdx : array of Integer;
  end;

var
  NameList   : TStringList;
  Reactions  : array of TReaction;
  OreIdx,
  FuelIdx    : Integer;

{ get or assign an index for a chemical name }
function GetChemIdx(const s: string): Integer;
begin
  Result := NameList.IndexOf(s);
  if Result < 0 then
  begin
    Result := NameList.Count;
    NameList.Add(s);
    SetLength(Reactions, NameList.Count);
    { by default Reactions[Result].OutQty = 0, InCount = 0, arrays empty }
  end;
end;

{ parse the input into NameList and Reactions }
procedure LoadReactions;
var
  F        : TextFile;
  line     : string;
  leftPart,
  rightPart: string;
  partsL   : TStringList;
  token    : string;
  qty      : Int64;
  i, outIdx: Integer;
  pArrow,
  pSpace   : Integer;
begin
  NameList := TStringList.Create;
  NameList.Sorted := False;
  NameList.Duplicates := dupIgnore;
  SetLength(Reactions, 0);

  AssignFile(F, INPUT_FILE);
  Reset(F);
  partsL := TStringList.Create;
  partsL.StrictDelimiter := True;
  while not EOF(F) do
  begin
    ReadLn(F, line);
    line := Trim(line);
    if line = '' then Continue;
    { split on '=>' }
    pArrow := Pos('=>', line);
    leftPart  := Trim(Copy(line, 1, pArrow - 1));
    rightPart := Trim(Copy(line, pArrow + 2, MaxInt));

    { parse right: "123 ABC" }
    pSpace := Pos(' ', rightPart);
    qty    := StrToInt64(Copy(rightPart, 1, pSpace - 1));
    outIdx := GetChemIdx(Trim(Copy(rightPart, pSpace + 1, MaxInt)));
    Reactions[outIdx].OutQty := qty;

    { parse left: comma‐separated inputs }
    partsL.Delimiter := ',';
    partsL.DelimitedText := leftPart;
    Reactions[outIdx].InCount := partsL.Count;
    SetLength(Reactions[outIdx].InQty,  partsL.Count);
    SetLength(Reactions[outIdx].InIdx,  partsL.Count);
    for i := 0 to partsL.Count - 1 do
    begin
      token := Trim(partsL[i]);
      pSpace := Pos(' ', token);
      qty  := StrToInt64(Copy(token, 1, pSpace - 1));
      Reactions[outIdx].InQty[i] := qty;
      Reactions[outIdx].InIdx[i] := GetChemIdx(Trim(Copy(token, pSpace + 1, MaxInt)));
    end;
  end;
  CloseFile(F);
  partsL.Free;

  { record special indices }
  OreIdx  := GetChemIdx('ORE');
  FuelIdx := GetChemIdx('FUEL');
end;

{ Given a desired amount of FUEL, compute the minimum ORE required }
function OreNeededForFuel(desiredFuel: Int64): Int64;
var
  need, surplus: array of Int64;
  idx, i       : Integer;
  want, runs   : Int64;
begin
  SetLength(need,     NameList.Count);
  SetLength(surplus, NameList.Count);
  for i := 0 to High(need) do
    need[i] := 0;  
  for i := 0 to High(surplus) do
    surplus[i] := 0;

  need[FuelIdx] := desiredFuel;

  repeat
    { find any chemical (≠ ORE) with a positive need }
    idx := -1;
    for i := 0 to High(need) do
      if (i <> OreIdx) and (need[i] > 0) then
      begin
        idx := i;
        Break;
      end;
    if idx < 0 then Break;

    want := need[idx];
    { use any surplus first }
    if surplus[idx] >= want then
    begin
      surplus[idx] := surplus[idx] - want;
      need[idx] := 0;
      Continue;
    end
    else
    begin
      want := want - surplus[idx];
      surplus[idx] := 0;
    end;

    { how many full runs of reaction we must do? }
    runs := (want + Reactions[idx].OutQty - 1) div Reactions[idx].OutQty;
    { any extra becomes new surplus of this chem }
    surplus[idx] := runs * Reactions[idx].OutQty - want;
    { consume inputs }
    for i := 0 to Reactions[idx].InCount - 1 do
      need[ Reactions[idx].InIdx[i] ] += runs * Reactions[idx].InQty[i];
    { we have now satisfied need[idx] }
    need[idx] := 0;
  until False;

  { what's left in need[OreIdx] is the total ORE we must mine }
  Result := need[OreIdx];
end;

var
  part1, part2 : Int64;
  lo, hi, mid  : Int64;
begin
  LoadReactions;

  { Part 1: how much ORE for exactly 1 FUEL? }
  part1 := OreNeededForFuel(1);
  WriteLn(part1);

  { Part 2: with 1e12 ORE, how much FUEL can we make? }
  lo := 1;
  hi := 1;
  { first find an upper bound }
  while OreNeededForFuel(hi) <= ORE_LIMIT do
    hi := hi * 2;
  { binary search in [lo..hi] }
  while lo < hi do
  begin
    mid := (lo + hi + 1) div 2;
    if OreNeededForFuel(mid) <= ORE_LIMIT then
      lo := mid
    else
      hi := mid - 1;
  end;
  part2 := lo;
  WriteLn(part2);
end.
