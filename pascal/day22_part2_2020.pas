
program CrabCombat;
{$mode objfpc}{$H+}
uses
  Classes, SysUtils;

type
  TDeck = record
    cards: array[1..100] of Integer;
    cnt: Integer;
  end;

{ remove and return the top card }
function Pop(var d: TDeck): Integer;
var
  i: Integer;
begin
  Result := d.cards[1];
  for i := 1 to d.cnt - 1 do
    d.cards[i] := d.cards[i + 1];
  Dec(d.cnt);
end;

{ append one card to the bottom }
procedure Push(var d: TDeck; v: Integer);
begin
  Inc(d.cnt);
  d.cards[d.cnt] := v;
end;

{ make a copy of the first n cards }
function CopyDeck(const d: TDeck; n: Integer): TDeck;
var
  i: Integer;
begin
  Result.cnt := n;
  for i := 1 to n do
    Result.cards[i] := d.cards[i];
end;

{ serialize two decks into a single string key for repetition detection }
function DeckKey(const d1, d2: TDeck): string;
var
  i: Integer;
  sb: TStringBuilder;
begin
  sb := TStringBuilder.Create;
  try
    for i := 1 to d1.cnt do
    begin
      sb.Append(IntToStr(d1.cards[i]));
      if i < d1.cnt then sb.Append(',');
    end;
    sb.Append('|');
    for i := 1 to d2.cnt do
    begin
      sb.Append(IntToStr(d2.cards[i]));
      if i < d2.cnt then sb.Append(',');
    end;
    Result := sb.ToString;
  finally
    sb.Free;
  end;
end;

{ play a game of Recursive Combat on d1, d2
  returns winner (1 or 2) and returns the winner's final deck in winnerDeck }
function PlayRecursive(var d1, d2: TDeck; out winnerDeck: TDeck): Integer;
var
  seen: TStringList;
  key: string;
  c1, c2, w: Integer;
  sub1, sub2: TDeck;
begin
  seen := TStringList.Create;
  try
    while (d1.cnt > 0) and (d2.cnt > 0) do
    begin
      { repetition check }
      key := DeckKey(d1, d2);
      if seen.IndexOf(key) >= 0 then
      begin
        winnerDeck := d1;
        Exit(1);
      end;
      seen.Add(key);

      c1 := Pop(d1);
      c2 := Pop(d2);

      { recurse if both have enough cards, otherwise higher card wins }
      if (d1.cnt >= c1) and (d2.cnt >= c2) then
      begin
        sub1 := CopyDeck(d1, c1);
        sub2 := CopyDeck(d2, c2);
        w := PlayRecursive(sub1, sub2, sub1);
      end
      else if c1 > c2 then
        w := 1
      else
        w := 2;

      if w = 1 then
      begin
        Push(d1, c1);
        Push(d1, c2);
      end
      else
      begin
        Push(d2, c2);
        Push(d2, c1);
      end;
    end;

    if d1.cnt > 0 then
    begin
      winnerDeck := d1;
      Exit(1);
    end
    else
    begin
      winnerDeck := d2;
      Exit(2);
    end;
  finally
    seen.Free;
  end;
end;

{ compute the final score of a deck }
function Score(const d: TDeck): Int64;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to d.cnt do
    Result := Result + Int64(d.cards[i]) * (d.cnt - i + 1);
end;

var
  f: TextFile;
  line: string;
  decks1, decks2: TDeck;
  d1, d2: TDeck;
  c1, c2: Integer;
  winner, part1Score, part2Score: Int64;
  winnerDeck: TDeck;
begin
  { read input.txt }
  AssignFile(f, 'input.txt');
  Reset(f);
  try
    { skip "Player 1:" }
    ReadLn(f, line);
    decks1.cnt := 0;
    repeat
      ReadLn(f, line);
      if line = '' then Break;
      Inc(decks1.cnt);
      decks1.cards[decks1.cnt] := StrToInt(line);
    until False;

    { skip "Player 2:" }
    ReadLn(f, line);
    decks2.cnt := 0;
    while not EOF(f) do
    begin
      ReadLn(f, line);
      if line = '' then Break;
      Inc(decks2.cnt);
      decks2.cards[decks2.cnt] := StrToInt(line);
    end;
  finally
    CloseFile(f);
  end;

  { -------- Part 1: normal Combat -------- }
  d1 := decks1;
  d2 := decks2;
  while (d1.cnt > 0) and (d2.cnt > 0) do
  begin
    c1 := Pop(d1);
    c2 := Pop(d2);
    if c1 > c2 then
    begin
      Push(d1, c1);
      Push(d1, c2);
    end
    else
    begin
      Push(d2, c2);
      Push(d2, c1);
    end;
  end;
  if d1.cnt > 0 then
    part1Score := Score(d1)
  else
    part1Score := Score(d2);

  { -------- Part 2: Recursive Combat -------- }
  d1 := decks1;
  d2 := decks2;
  PlayRecursive(d1, d2, winnerDeck);
  part2Score := Score(winnerDeck);

  { output answers }
  WriteLn('Part 1: ', part1Score);
  WriteLn('Part 2: ', part2Score);
end.
