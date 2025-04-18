
program SpaceStoichiometry;
{$mode objfpc}{$H+}
uses
  Classes, SysUtils;

type
  TInput = record
    qty: Int64;
    idx: Integer;
  end;

  TReaction = record
    outQty: Int64;
    inputs: array of TInput;
  end;

var
  Reactions: array of TReaction;
  Names: array of string;

// Get the index for a chemical name, adding it if new
function GetIndex(const name: string): Integer;
var
  i: Integer;
begin
  for i := 0 to High(Names) do
    if Names[i] = name then
      Exit(i);
  // not found, append
  Result := Length(Names);
  SetLength(Names, Result + 1);
  Names[Result] := name;
end;

// Split s by delim into a dynamic array of trimmed tokens
function SplitTokens(const s: string; delim: Char): TStringList;
var
  parts: TStringList;
  i: Integer;
  tok: string;
begin
  parts := TStringList.Create;
  parts.StrictDelimiter := True;
  parts.Delimiter := delim;
  parts.DelimitedText := s;
  // trim spaces
  for i := 0 to parts.Count - 1 do
  begin
    tok := parts[i];
    tok := Trim(tok);
    parts[i] := tok;
  end;
  Result := parts;
end;

var
  f: TextFile;
  line, leftSide, rightSide: string;
  leftTokens, tmp: TStringList;
  outToken: string;
  i, j, outIdx, inIdx, c: Integer;
  qty, times: Int64;
  need, leftover: array of Int64;
  pending: Boolean;
begin
  // 1) Parse input.txt
  AssignFile(f, 'input.txt');
  Reset(f);
  Names := nil;
  Reactions := nil;
  while not Eof(f) do
  begin
    ReadLn(f, line);
    if Trim(line) = '' then Continue;
    // split around =>
    c := Pos('=>', line);
    leftSide := Trim(Copy(line, 1, c-1));
    rightSide := Trim(Copy(line, c+2, MaxInt));
    // parse output
    outToken := Trim(rightSide);
    tmp := SplitTokens(outToken, ' ');
    qty := StrToInt64(tmp[0]);
    outIdx := GetIndex(tmp[1]);
    tmp.Free;
    // ensure Reactions[outIdx] exists
    if outIdx > High(Reactions) then
      SetLength(Reactions, outIdx + 1);
    Reactions[outIdx].outQty := qty;
    // parse inputs
    leftTokens := SplitTokens(leftSide, ',');
    SetLength(Reactions[outIdx].inputs, leftTokens.Count);
    for i := 0 to leftTokens.Count - 1 do
    begin
      tmp := SplitTokens(leftTokens[i], ' ');
      qty := StrToInt64(tmp[0]);
      inIdx := GetIndex(tmp[1]);
      Reactions[outIdx].inputs[i].qty := qty;
      Reactions[outIdx].inputs[i].idx := inIdx;
      tmp.Free;
    end;
    leftTokens.Free;
  end;
  CloseFile(f);

  // 2) Prepare need & leftover arrays
  SetLength(need, Length(Names));
  SetLength(leftover, Length(Names));
  for i := 0 to High(need) do
    need[i] := 0;
  // we want 1 FUEL
  // assume "FUEL" is in Names
  i := GetIndex('FUEL');
  need[i] := 1;

  // 3) Loop: satisfy needs until only ORE remains
  repeat
    pending := False;
    for i := 0 to High(need) do
    begin
      // skip ORE and chemicals with no demand
      if (Names[i] = 'ORE') or (need[i] = 0) then
        Continue;
      pending := True;
      // first use leftover if any
      if leftover[i] >= need[i] then
      begin
        leftover[i] := leftover[i] - need[i];
        need[i] := 0;
        Continue;
      end
      else
      begin
        need[i] := need[i] - leftover[i];
        leftover[i] := 0;
      end;
      // determine how many reaction runs
      qty := Reactions[i].outQty;
      times := (need[i] + qty - 1) div qty; // ceil
      // add leftovers
      leftover[i] := leftover[i] + times * qty - need[i];
      // add inputs to need[]
      for j := 0 to High(Reactions[i].inputs) do
      begin
        need[ Reactions[i].inputs[j].idx ] :=
          need[ Reactions[i].inputs[j].idx ] +
          times * Reactions[i].inputs[j].qty;
      end;
      // this chemical is now satisfied
      need[i] := 0;
    end;
  until not pending;

  // 4) The answer is need["ORE"]
  writeln( need[ GetIndex('ORE') ] );
end.
