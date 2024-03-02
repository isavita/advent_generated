program Solution;

uses Sysutils, Classes, StrUtils;

var
  inputFile: Text;
  startingNumbers: TStringList;
  lastSpoken: array of integer;
  lastNumber, nextNumber, turn, lastTurn: integer;
  inputLine, startingNumbersStr: string;

begin
  Assign(inputFile, 'input.txt');
  reset(inputFile);

  Readln(inputFile, inputLine);
  startingNumbersStr := Trim(inputLine);
  startingNumbers := TStringList.Create;
  startingNumbers.Delimiter := ',';
  startingNumbers.DelimitedText := startingNumbersStr;

  SetLength(lastSpoken, 2020);
  lastNumber := 0;
  nextNumber := 0;

  for turn := 1 to 2020 do
  begin
    if turn - 1 < startingNumbers.Count then
    begin
      lastNumber := StrToInt(startingNumbers[turn - 1]);
      lastSpoken[lastNumber] := turn;
      Continue;
    end;

    if lastSpoken[lastNumber] <> 0 then
    begin
      lastTurn := lastSpoken[lastNumber];
      nextNumber := turn - 1 - lastTurn;
    end
    else
    begin
      nextNumber := 0;
    end;

    lastSpoken[lastNumber] := turn - 1;
    lastNumber := nextNumber;
  end;

  Writeln(lastNumber);

  Close(inputFile);
end.