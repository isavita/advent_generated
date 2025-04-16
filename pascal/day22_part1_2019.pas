
program DeckShuffling;

{$mode objfpc}{$H+}

uses
  SysUtils;

type
  TIntegerArray = array of Integer;

var
  deck: TIntegerArray;
  deckSize: Integer;
  inputFile: TextFile;
  line: String;
  i, n, pos: Integer;

procedure ReverseDeck;
var
  i, j: Integer;
  temp: Integer;
begin
  i := 0;
  j := deckSize - 1;
  while i < j do
  begin
    temp := deck[i];
    deck[i] := deck[j];
    deck[j] := temp;
    Inc(i);
    Dec(j);
  end;
end;

procedure CutDeck(n: Integer);
var
  newDeck: TIntegerArray;
begin
  SetLength(newDeck, deckSize);
  if n >= 0 then
  begin
    for i := 0 to deckSize - 1 do
    begin
      newDeck[i] := deck[(i + n) mod deckSize];
    end;
  end
  else
  begin
    for i := 0 to deckSize - 1 do
    begin
      newDeck[i] := deck[(i + deckSize + n) mod deckSize];
    end;
  end;
  deck := newDeck;
end;

procedure DealWithIncrement(n: Integer);
var
  newDeck: TIntegerArray;
begin
  SetLength(newDeck, deckSize);
  pos := 0;
  for i := 0 to deckSize - 1 do
  begin
    newDeck[pos] := deck[i];
    pos := (pos + n) mod deckSize;
  end;
  deck := newDeck;
end;

function FindCard(card: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to deckSize - 1 do
  begin
    if deck[i] = card then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

begin
  deckSize := 10007;
  SetLength(deck, deckSize);
  for i := 0 to deckSize - 1 do
  begin
    deck[i] := i;
  end;

  Assign(inputFile, 'input.txt');
  Reset(inputFile);
  try
    while not Eof(inputFile) do
    begin
      Readln(inputFile, line);
      if line = 'deal into new stack' then
      begin
        ReverseDeck;
      end
      else if Copy(line, 1, 3) = 'cut' then
      begin
        Val(StringReplace(line, 'cut ', '', [rfReplaceAll]), n, i);
        CutDeck(n);
      end
      else if Copy(line, 1, 19) = 'deal with increment' then
      begin
        Val(StringReplace(line, 'deal with increment ', '', [rfReplaceAll]), n, i);
        DealWithIncrement(n);
      end;
    end;
  finally
    CloseFile(inputFile);
  end;

  WriteLn(FindCard(2019));
end.
