
program SpringArrangements;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes;

type
  TIntArray = array of Integer;

function ParseRow(const Line: string; out Springs: string; out Groups: TIntArray): Boolean;
var
  SpacePos, CommaPos, i, Len: Integer;
  GroupStr, Token: string;
begin
  SpacePos := Pos(' ', Line);
  if SpacePos = 0 then Exit(False);
  Springs := Copy(Line, 1, SpacePos - 1);
  GroupStr := Copy(Line, SpacePos + 1, MaxInt);
  Len := 0;
  SetLength(Groups, 0);
  while GroupStr <> '' do
  begin
    CommaPos := Pos(',', GroupStr);
    if CommaPos = 0 then
    begin
      Token := GroupStr;
      GroupStr := '';
    end
    else
    begin
      Token := Copy(GroupStr, 1, CommaPos - 1);
      Delete(GroupStr, 1, CommaPos);
    end;
    SetLength(Groups, Len + 1);
    Groups[Len] := StrToInt(Token);
    Inc(Len);
  end;
  Result := True;
end;

function Unfold(const Springs: string; const Groups: TIntArray; Factor: Integer;
  out NewSprings: string; out NewGroups: TIntArray): Boolean;
var
  i, j, Pos: Integer;
begin
  NewSprings := '';
  for i := 1 to Factor do
  begin
    NewSprings := NewSprings + Springs;
    if i < Factor then
      NewSprings := NewSprings + '?';
  end;

  SetLength(NewGroups, Length(Groups) * Factor);
  Pos := 0;
  for i := 1 to Factor do
    for j := 0 to High(Groups) do
    begin
      NewGroups[Pos] := Groups[j];
      Inc(Pos);
    end;
  Result := True;
end;

function CountArrangements(const Springs: string; const Groups: TIntArray): Int64;
type
  TDP = array of array of array of Int64;
var
  Len, GLen, MaxGrp, i, j, k: Integer;
  DP: TDP;

  function Rec(iSpr, iGrp, iCont: Integer): Int64;
  var
    C: Char;
    Res: Int64;
  begin
    if DP[iSpr][iGrp][iCont] <> -1 then
      Exit(DP[iSpr][iGrp][iCont]);

    if iSpr = Len then
    begin
      if (iGrp = GLen) and (iCont = 0) then
        Result := 1
      else if (iGrp = GLen - 1) and (iCont = Groups[iGrp]) then
        Result := 1
      else
        Result := 0;
      DP[iSpr][iGrp][iCont] := Result;
      Exit;
    end;

    C := Springs[iSpr + 1]; { 1‑based string }
    Res := 0;

    if (C = '.') or (C = '?') then
    begin
      if iCont = 0 then
        Res := Res + Rec(iSpr + 1, iGrp, 0);
      if (iGrp < GLen) and (iCont = Groups[iGrp]) then
        Res := Res + Rec(iSpr + 1, iGrp + 1, 0);
    end;

    if (C = '#') or (C = '?') then
    begin
      if (iGrp < GLen) and (iCont < Groups[iGrp]) then
        Res := Res + Rec(iSpr + 1, iGrp, iCont + 1);
    end;

    DP[iSpr][iGrp][iCont] := Res;
    Result := Res;
  end;

begin
  Len := Length(Springs);
  GLen := Length(Groups);
  MaxGrp := 0;
  for i := 0 to GLen - 1 do
    if Groups[i] > MaxGrp then
      MaxGrp := Groups[i];

  SetLength(DP, Len + 1);
  for i := 0 to Len do
  begin
    SetLength(DP[i], GLen + 1);
    for j := 0 to GLen do
      SetLength(DP[i][j], MaxGrp + 1);
  end;

  for i := 0 to Len do
    for j := 0 to GLen do
      for k := 0 to MaxGrp do
        DP[i][j][k] := -1;

  Result := Rec(0, 0, 0);
end;

var
  InputFile: TextFile;
  Line, Springs, UnfoldedSprings: string;
  Groups, UnfoldedGroups: TIntArray;
  Total: Int64;
begin
  Total := 0;
  AssignFile(InputFile, 'input.txt');
  Reset(InputFile);
  while not Eof(InputFile) do
  begin
    ReadLn(InputFile, Line);
    if Trim(Line) = '' then Continue;
    if not ParseRow(Line, Springs, Groups) then Continue;
    Unfold(Springs, Groups, 5, UnfoldedSprings, UnfoldedGroups);
    Total := Total + CountArrangements(UnfoldedSprings, UnfoldedGroups);
  end;
  CloseFile(InputFile);
  WriteLn(Total);
end.
