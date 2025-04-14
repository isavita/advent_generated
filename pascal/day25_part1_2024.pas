
program LockKeyMatcher;
{$MODE Delphi}{$H+}
uses SysUtils, Classes, Generics.Collections;

type
  TPattern = array[1..5] of Integer;
  TBlock = array[1..7] of string;
  TPatternList = TList<TPattern>;

var
  RawList: TStringList;
  Locks: TPatternList;
  Keys: TPatternList;
  F: TextFile;
  i, LockIdx, KeyIdx, Count, k: Integer;
  Block: TBlock;
  CurrentPattern: TPattern;
  IsLock: Boolean;
  SkipBlock: Boolean;
  Line: string;

function ParseLock(const B: TBlock): TPattern;
var
  Col, Row, Cnt: Integer;
begin
  for Col := 1 to 5 do
  begin
    Cnt := 0;
    for Row := 2 to 7 do
    begin
      if Length(B[Row]) >= Col then
      begin
        if B[Row][Col] = '#' then
          Inc(Cnt)
        else
          Break;
      end
      else
        Break;
    end;
    Result[Col] := Cnt;
  end;
end;

function ParseKey(const B: TBlock): TPattern;
var
  Col, Row, Cnt: Integer;
begin
  for Col := 1 to 5 do
  begin
    Cnt := 0;
    for Row := 6 downto 1 do
    begin
      if Length(B[Row]) >= Col then
      begin
        if B[Row][Col] = '#' then
          Inc(Cnt)
        else
          Break;
      end
      else
        Break;
    end;
    Result[Col] := Cnt;
  end;
end;

function Fits(const Lk, Ky: TPattern): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 1 to 5 do
  begin
    if Lk[i] + Ky[i] > 5 then
    begin
      Result := False;
      Exit;
    end;
  end;
end;

begin
  RawList := TStringList.Create;
  Locks := TPatternList.Create;
  Keys := TPatternList.Create;

  Assign(F, 'input.txt');
  try
    Reset(F);
    while not Eof(F) do
    begin
      ReadLn(F, Line);
      Line := Trim(Line);
      if Length(Line) > 0 then
        RawList.Add(Line);
    end;
    Close(F);

    if (RawList.Count = 0) or (RawList.Count mod 7 <> 0) then
    begin
      WriteLn(0);
      Exit;
    end;

    i := 0;
    while i < RawList.Count do
    begin
      for k := 1 to 7 do
        Block[k] := RawList[i + k - 1];

      SkipBlock := False;
      for k := 1 to 7 do
      begin
        if Length(Block[k]) < 5 then
        begin
          SkipBlock := True;
          Break;
        end;
      end;

      if SkipBlock then
      begin
        i := i + 7;
        Continue;
      end;

      IsLock := True;
      if Length(Block[1]) >= 5 then
      begin
         for k := 1 to 5 do
         begin
           if Block[1][k] <> '#' then
           begin
             IsLock := False;
             Break;
           end;
         end;
      end else begin
         IsLock := False; // Should be caught by SkipBlock, but safe
      end;


      if IsLock then
      begin
        CurrentPattern := ParseLock(Block);
        Locks.Add(CurrentPattern);
      end
      else
      begin
        CurrentPattern := ParseKey(Block);
        Keys.Add(CurrentPattern);
      end;

      i := i + 7;
    end;

    Count := 0;
    for LockIdx := 0 to Locks.Count - 1 do
    begin
      for KeyIdx := 0 to Keys.Count - 1 do
      begin
        if Fits(Locks[LockIdx], Keys[KeyIdx]) then
          Inc(Count);
      end;
    end;

    WriteLn(Count);

  finally
    RawList.Free;
    Locks.Free;
    Keys.Free;
  end;
end.
