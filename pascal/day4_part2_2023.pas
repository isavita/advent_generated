
program ScratchCards;

{$MODE OBJFPC}{$H+} // Use Free Pascal specific directives

uses
  SysUtils; // For string manipulation, file handling, Val

type
  // Set to store numbers efficiently (assuming numbers are within 0-99)
  TIntegerSet = set of 0..99;

  // Record to represent a scratch card
  TCard = record
    Winnings: TIntegerSet;
    Givens: TIntegerSet;
    TotalCount: Int64; // Use Int64 as total can become large
  end;

  // Dynamic array to hold all cards
  TCardList = array of TCard;

// Parses a string of space-separated numbers into a set
function ParseNumbersToSet(const S: string): TIntegerSet;
var
  NumStr: string;
  I, Code, Value: Integer;
  CurrentPos: Integer;
  TempS: string;
begin
  Result := []; // Initialize empty set
  TempS := Trim(S) + ' '; // Add trailing space for easier parsing
  CurrentPos := 1;
  while CurrentPos <= Length(TempS) do
  begin
    // Skip leading spaces
    while (CurrentPos <= Length(TempS)) and (TempS[CurrentPos] = ' ') do
      Inc(CurrentPos);

    if CurrentPos > Length(TempS) then Break; // Exit if only spaces remained

    // Find the end of the number (next space)
    I := CurrentPos;
    while (I <= Length(TempS)) and (TempS[I] <> ' ') do
      Inc(I);

    // Extract the number string
    NumStr := Copy(TempS, CurrentPos, I - CurrentPos);

    // Convert string to integer and add to set if valid
    if Length(NumStr) > 0 then
    begin
      Val(NumStr, Value, Code);
      if (Code = 0) and (Value >= 0) and (Value <= 99) then // Basic validation
        Include(Result, Value);
    end;
    // Move position past the number and the space
    CurrentPos := I + 1;
  end;
end;

// Counts the number of elements (matches) in the intersection set
function CountMatches(const Card: TCard): Integer;
var
  Matches: TIntegerSet;
  i: Integer;
begin
  Result := 0;
  Matches := Card.Winnings * Card.Givens; // Efficient set intersection
  for i := 0 to 99 do // Iterate through possible set elements
  begin
    if i in Matches then
      Inc(Result);
  end;
end;

var
  InputFile: Text;
  Line: string;
  Cards: TCardList;
  I, J, MatchCount, CardCount: Integer;
  ColonPos, PipePos: Integer;
  WinStr, GivenStr: string;
  TotalCards: Int64;

begin
  Assign(InputFile, 'input.txt');
  try
    Reset(InputFile);

    CardCount := 0;
    // Read lines and parse cards
    while not EOF(InputFile) do
    begin
      ReadLn(InputFile, Line);
      Line := Trim(Line);
      if Length(Line) = 0 then Continue; // Skip empty lines

      ColonPos := Pos(':', Line);
      PipePos := Pos('|', Line);

      // Basic format check
      if (ColonPos = 0) or (PipePos = 0) or (ColonPos >= PipePos) then Continue;

      // Extract number strings
      WinStr := Copy(Line, ColonPos + 1, PipePos - ColonPos - 1);
      GivenStr := Copy(Line, PipePos + 1, Length(Line) - PipePos);

      // Increase array size
      Inc(CardCount);
      SetLength(Cards, CardCount);

      // Parse numbers and initialize card
      with Cards[CardCount - 1] do // Use High(Cards) or CardCount-1 for 0-based index
      begin
        Winnings := ParseNumbersToSet(WinStr);
        Givens := ParseNumbersToSet(GivenStr);
        TotalCount := 1; // Start with one of each card
      end;
    end;
  finally
    Close(InputFile);
  end;

  // Calculate card copies based on matches
  if CardCount > 0 then
  begin
    for I := 0 to High(Cards) do // Iterate through each card
    begin
      MatchCount := CountMatches(Cards[I]);

      // Add copies to subsequent cards
      for J := 1 to MatchCount do
      begin
        if I + J <= High(Cards) then // Check bounds
        begin
          // Add the count of the current card to the subsequent card's count
          Cards[I + J].TotalCount := Cards[I + J].TotalCount + Cards[I].TotalCount;
        end
        else
        begin
           Break; // Stop if we go past the last card
        end;
      end;
    end;
  end;

  // Sum total number of cards
  TotalCards := 0;
  if CardCount > 0 then
  begin
     for I := 0 to High(Cards) do
     begin
       TotalCards := TotalCards + Cards[I].TotalCount;
     end;
  end;

  // Print the result
  WriteLn(TotalCards);

end.
