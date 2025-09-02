
program CardGame;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes;

const
  MAX_HANDS = 1000;
  HAND_SIZE = 5;

type
  THand = record
    Cards: string[6];
    Bid: Integer;
  end;

  TRankedHand = record
    Hand: THand;
    HandType: Integer;
    RankVal: Int64;
  end;

function CardValue(C: Char): Integer;
begin
  if C in ['0'..'9'] then
    Result := Ord(C) - Ord('0')
  else
    case C of
      'T': Result := 10;
      'J': Result := 11;
      'Q': Result := 12;
      'K': Result := 13;
      'A': Result := 14;
    else
      Result := 0;
    end;
end;

function HandType(const Cards: string): Integer;
var
  Count: array[2..14] of Integer;
  i, v, Pairs, Three, Four, Five: Integer;
begin
  FillChar(Count, SizeOf(Count), 0);
  for i := 1 to HAND_SIZE do
    Inc(Count[CardValue(Cards[i])]);
  Pairs := 0; Three := 0; Four := 0; Five := 0;
  for v := 2 to 14 do
    case Count[v] of
      2: Inc(Pairs);
      3: Inc(Three);
      4: Inc(Four);
      5: Inc(Five);
    end;
  if Five > 0 then Exit(7);
  if Four > 0 then Exit(6);
  if (Three > 0) and (Pairs > 0) then Exit(5);
  if Three > 0 then Exit(4);
  if Pairs = 2 then Exit(3);
  if Pairs = 1 then Exit(2);
  Exit(1);
end;

function CalcRank(const Cards: string): Int64;
var
  i: Integer;
  v: Int64;
begin
  Result := 0;
  for i := 1 to HAND_SIZE do
  begin
    Result := Result shl 4;
    v := CardValue(Cards[i]);
    Result := Result + v;
  end;
end;

procedure QuickSort(var A: array of TRankedHand; L, R: Integer);
var
  i, j: Integer;
  Pivot, Temp: TRankedHand;
begin
  i := L; j := R;
  Pivot := A[(L + R) div 2];
  repeat
    while (A[i].HandType > Pivot.HandType) or
          ((A[i].HandType = Pivot.HandType) and (A[i].RankVal > Pivot.RankVal)) do Inc(i);
    while (A[j].HandType < Pivot.HandType) or
          ((A[j].HandType = Pivot.HandType) and (A[j].RankVal < Pivot.RankVal)) do Dec(j);
    if i <= j then
    begin
      Temp := A[i];
      A[i] := A[j];
      A[j] := Temp;
      Inc(i); Dec(j);
    end;
  until i > j;
  if L < j then QuickSort(A, L, j);
  if i < R then QuickSort(A, i, R);
end;

var
  Hands: array[0..MAX_HANDS-1] of THand;
  Ranked: array[0..MAX_HANDS-1] of TRankedHand;
  NumHands, i, TotalRanked: Integer;
  TotalWinnings: Int64;
  F: TextFile;
  Line, CardStr: string;
  Bid: Integer;
begin
  AssignFile(F, 'input.txt');
  Reset(F);
  NumHands := 0;
  while not Eof(F) do
  begin
    ReadLn(F, Line);
    if Length(Trim(Line)) = 0 then Continue;
    CardStr := '';
    Bid := 0;
    i := 1;
    while (i <= Length(Line)) and (Line[i] <> ' ') do
    begin
      CardStr := CardStr + Line[i];
      Inc(i);
    end;
    while (i <= Length(Line)) and (Line[i] = ' ') do Inc(i);
    while i <= Length(Line) do
    begin
      Bid := Bid * 10 + Ord(Line[i]) - Ord('0');
      Inc(i);
    end;
    Hands[NumHands].Cards := CardStr;
    Hands[NumHands].Bid := Bid;
    Inc(NumHands);
  end;
  CloseFile(F);

  for i := 0 to NumHands - 1 do
  begin
    Ranked[i].Hand := Hands[i];
    Ranked[i].HandType := HandType(Hands[i].Cards);
    Ranked[i].RankVal := CalcRank(Hands[i].Cards);
  end;

  TotalRanked := NumHands;
  if TotalRanked > 1 then
    QuickSort(Ranked, 0, TotalRanked - 1);

  TotalWinnings := 0;
  for i := 0 to TotalRanked - 1 do
    TotalWinnings := TotalWinnings + Int64(Ranked[i].Hand.Bid) * (TotalRanked - i);

  WriteLn(TotalWinnings);
end.
